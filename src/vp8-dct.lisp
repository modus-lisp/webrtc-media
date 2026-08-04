;;;; vp8-dct.lisp — VP8's 4x4 transforms + quantization (RFC 6386 §14).
;;;;
;;;; Forward DCT (luma/chroma residual) and forward Walsh-Hadamard (the "Y2" second-order
;;;; transform of the sixteen luma DCs in a 16x16-predicted macroblock), ported bit-for-bit from
;;;; the libvpx reference so the coefficients we emit round-trip through the browser's inverse.
;;;; The inverse transforms + a quant round-trip are here purely to self-test the encoder half.

(in-package #:webrtc-media.vp8)

;;; A COEFFICIENT BLOCK IS SIXTEEN BITS, and the bound is proved rather than observed — the whole
;;; chain is worst-case, independent of the picture:
;;;
;;;   residual   = source - prediction, both 8-bit          -> [-255, 255]
;;;   fdct TMP   = 8*(sum of two residuals) + 8*(two more)  -> |x| <= 8160
;;;   fdct OUT   = (a1+b1+7) >> 4 with |a1|,|b1| <= 16320   -> |x| <= 2040
;;;   fwht IN    = the sixteen fdct DCs                     -> |x| <= 2040
;;;   fwht TMP   = 4*(in+in) + 4*(in+in) + 1                -> |x| <= 32641
;;;   fwht OUT   = ((a1+b1)+(d1+c1)+3) >> 3                 -> |x| <= 16320
;;;   levels     = clamped to DCT_MAX_VALUE                 -> |x| <= 2047
;;;   dequantized = level * q, and the clamp only ever LOWERS it below the coefficient it came from
;;;                                                         -> |x| <= 16400
;;;
;;; So every STORED value fits, and measured over real desktop frames at qi 8/24/52/127 the largest
;;; any of them actually reached was fwht TMP at 32641 — exactly its bound.
;;;
;;; THAT ONE HAS 126 COUNTS OF HEADROOM, which is worth knowing before anything here is changed.
;;; It is safe only because the residual is 8-bit; a deeper input, or a different rounding in the
;;; forward DCT, would silently wrap it.  And the WHT's pass-2 INTERMEDIATES (a2..d2, which are
;;; Lisp locals and so stay full-width here) reach +-128068 — eighteen bits.  A vectorised WHT
;;; therefore cannot stay in 16-bit lanes across pass 2, whatever the arrays say; the DCT can,
;;; with a factor of four to spare.
(deftype blk () '(simple-array (signed-byte 16) (16)))

;; INLINE, and that is not cosmetic.  Each transform below allocates its own TMP and declares it
;; DYNAMIC-EXTENT — but a DYNAMIC-EXTENT binding can only be stack-allocated when the compiler can
;; SEE the allocation, and behind an out-of-line call it cannot.  So every one of the ~200000
;; transform calls in a keyframe was heap-allocating a 16-element block: 16 MB of garbage per
;; frame, for arrays that never outlive the call that makes them.
(declaim (inline mkblk))
(defun mkblk (&optional (init 0)) (make-array 16 :element-type '(signed-byte 16) :initial-element init))

;;; ---- forward DCT (vp8_short_fdct4x4_c) -------------------------------------
(defun fdct4x4 (in out)
  "Forward VP8 4x4 DCT.  IN 16 residual samples (row-major), OUT 16 coefficients."
  (declare (type (simple-array (signed-byte 16) (16)) in out)
           (optimize (speed 3) (safety 0)))
  (let ((tmp (mkblk)))
    (declare (dynamic-extent tmp) (type (simple-array (signed-byte 16) (16)) tmp))
    (dotimes (i 4)                                     ; pass 1: rows
      (let* ((o (* i 4))
             (a1 (* 8 (+ (aref in (+ o 0)) (aref in (+ o 3)))))
             (b1 (* 8 (+ (aref in (+ o 1)) (aref in (+ o 2)))))
             (c1 (* 8 (- (aref in (+ o 1)) (aref in (+ o 2)))))
             (d1 (* 8 (- (aref in (+ o 0)) (aref in (+ o 3))))))
        (setf (aref tmp (+ o 0)) (+ a1 b1)
              (aref tmp (+ o 2)) (- a1 b1)
              (aref tmp (+ o 1)) (ash (+ (* c1 2217) (* d1 5352) 14500) -12)
              (aref tmp (+ o 3)) (ash (+ (- (* d1 2217) (* c1 5352)) 7500) -12))))
    (dotimes (i 4)                                     ; pass 2: columns
      (let* ((a1 (+ (aref tmp (+ i 0)) (aref tmp (+ i 12))))
             (b1 (+ (aref tmp (+ i 4)) (aref tmp (+ i 8))))
             (c1 (- (aref tmp (+ i 4)) (aref tmp (+ i 8))))
             (d1 (- (aref tmp (+ i 0)) (aref tmp (+ i 12)))))
        (setf (aref out (+ i 0)) (ash (+ a1 b1 7) -4)
              (aref out (+ i 8)) (ash (+ (- a1 b1) 7) -4)
              (aref out (+ i 4)) (+ (ash (+ (* c1 2217) (* d1 5352) 12000) -16) (if (zerop d1) 0 1))
              (aref out (+ i 12)) (ash (+ (- (* d1 2217) (* c1 5352)) 51000) -16))))
    out))

;;; ---- inverse DCT (RFC 6386 §14.3) — for the round-trip test ----------------
(defconstant +c1const+ 20091)   ; cospi8sqrt2minus1
(defconstant +c2const+ 35468)   ; sinpi8sqrt2
(defun idct4x4 (in out)
  (declare (type (simple-array (signed-byte 16) (16)) in out)
           (optimize (speed 3) (safety 0)))
  (let ((tmp (mkblk)))
    (declare (dynamic-extent tmp) (type (simple-array (signed-byte 16) (16)) tmp))
    (dotimes (i 4)                                     ; columns
      (let* ((i0 (aref in i)) (i4 (aref in (+ i 4))) (i8 (aref in (+ i 8))) (i12 (aref in (+ i 12)))
             (a1 (+ i0 i8)) (b1 (- i0 i8))
             (c1 (- (ash (* i4 +c2const+) -16) (+ i12 (ash (* i12 +c1const+) -16))))
             (d1 (+ (+ i4 (ash (* i4 +c1const+) -16)) (ash (* i12 +c2const+) -16))))
        (setf (aref tmp (+ i 0)) (+ a1 d1) (aref tmp (+ i 12)) (- a1 d1)
              (aref tmp (+ i 4)) (+ b1 c1) (aref tmp (+ i 8)) (- b1 c1))))
    (dotimes (i 4)                                     ; rows
      (let* ((o (* i 4))
             (i0 (aref tmp o)) (i1 (aref tmp (+ o 1))) (i2 (aref tmp (+ o 2))) (i3 (aref tmp (+ o 3)))
             (a1 (+ i0 i2)) (b1 (- i0 i2))
             (c1 (- (ash (* i1 +c2const+) -16) (+ i3 (ash (* i3 +c1const+) -16))))
             (d1 (+ (+ i1 (ash (* i1 +c1const+) -16)) (ash (* i3 +c2const+) -16))))
        (setf (aref out (+ o 0)) (ash (+ a1 d1 4) -3) (aref out (+ o 3)) (ash (+ (- a1 d1) 4) -3)
              (aref out (+ o 1)) (ash (+ b1 c1 4) -3) (aref out (+ o 2)) (ash (+ (- b1 c1) 4) -3))))
    out))

;;; ---- forward / inverse Walsh-Hadamard (Y2) --------------------------------
(defun fwht4x4 (in out)
  "Forward VP8 Walsh-Hadamard of the 16 luma DC values (vp8_short_walsh4x4_c)."
  (declare (type (simple-array (signed-byte 16) (16)) in out)
           (optimize (speed 3) (safety 0)))
  (let ((tmp (mkblk)))
    (declare (dynamic-extent tmp) (type (simple-array (signed-byte 16) (16)) tmp))
    (dotimes (i 4)
      (let* ((o (* i 4))
             (a1 (* 4 (+ (aref in (+ o 0)) (aref in (+ o 2)))))
             (d1 (* 4 (+ (aref in (+ o 1)) (aref in (+ o 3)))))
             (c1 (* 4 (- (aref in (+ o 1)) (aref in (+ o 3)))))
             (b1 (* 4 (- (aref in (+ o 0)) (aref in (+ o 2))))))
        (setf (aref tmp (+ o 0)) (+ a1 d1 (if (zerop a1) 0 1))
              (aref tmp (+ o 1)) (+ b1 c1) (aref tmp (+ o 2)) (- b1 c1) (aref tmp (+ o 3)) (- a1 d1))))
    (dotimes (i 4)
      (let* ((a1 (+ (aref tmp (+ i 0)) (aref tmp (+ i 8))))
             (b1 (+ (aref tmp (+ i 4)) (aref tmp (+ i 12))))
             (c1 (- (aref tmp (+ i 4)) (aref tmp (+ i 12))))
             (d1 (- (aref tmp (+ i 0)) (aref tmp (+ i 8))))
             (a2 (+ a1 b1)) (b2 (+ d1 c1)) (c2 (- d1 c1)) (d2 (- a1 b1)))
        ;; libvpx: a2 += (a2 < 0)  — the bias is added on NEGATIVE values, not positive
        (setf (aref out (+ i 0)) (ash (+ a2 3 (if (minusp a2) 1 0)) -3)
              (aref out (+ i 4)) (ash (+ b2 3 (if (minusp b2) 1 0)) -3)
              (aref out (+ i 8)) (ash (+ c2 3 (if (minusp c2) 1 0)) -3)
              (aref out (+ i 12)) (ash (+ d2 3 (if (minusp d2) 1 0)) -3))))
    out))

(defun iwht4x4 (in out)
  (declare (type (simple-array (signed-byte 16) (16)) in out)
           (optimize (speed 3) (safety 0)))
  (let ((tmp (mkblk)))
    (declare (dynamic-extent tmp) (type (simple-array (signed-byte 16) (16)) tmp))
    (dotimes (i 4)
      (let* ((a1 (+ (aref in (+ i 0)) (aref in (+ i 12)))) (b1 (+ (aref in (+ i 4)) (aref in (+ i 8))))
             (c1 (- (aref in (+ i 4)) (aref in (+ i 8)))) (d1 (- (aref in (+ i 0)) (aref in (+ i 12)))))
        (setf (aref tmp (+ i 0)) (+ a1 b1) (aref tmp (+ i 4)) (+ d1 c1)
              (aref tmp (+ i 8)) (- a1 b1) (aref tmp (+ i 12)) (- d1 c1))))
    (dotimes (i 4)
      (let* ((o (* i 4))
             (a1 (+ (aref tmp (+ o 0)) (aref tmp (+ o 3)))) (b1 (+ (aref tmp (+ o 1)) (aref tmp (+ o 2))))
             (c1 (- (aref tmp (+ o 1)) (aref tmp (+ o 2)))) (d1 (- (aref tmp (+ o 0)) (aref tmp (+ o 3)))))
        (setf (aref out (+ o 0)) (ash (+ a1 b1 3) -3) (aref out (+ o 1)) (ash (+ d1 c1 3) -3)
              (aref out (+ o 2)) (ash (+ (- a1 b1) 3) -3) (aref out (+ o 3)) (ash (+ (- d1 c1) 3) -3))))
    out))

;;; ---- quantization (RFC 6386 §14.1) -----------------------------------------
(defparameter +dc-qlookup+
  #(4 5 6 7 8 9 10 10 11 12 13 14 15 16 17 17 18 19 20 20 21 21 22 22 23 23 24 25 25 26 27 28
    29 30 31 32 33 34 35 36 37 37 38 39 40 41 42 43 44 45 46 46 47 48 49 50 51 52 53 54 55 56 57 58
    59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 76 77 78 79 80 81 82 83 84 85 86 87 88 89
    91 93 95 96 98 100 101 102 104 106 108 110 112 114 116 118 122 124 126 128 130 132 134 136 138 140 143 145 148 151 154 157))
(defparameter +ac-qlookup+
  #(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35
    36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 60 62 64 66 68 70 72 74 76
    78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 119 122 125 128 131 134 137 140 143 146 149 152
    155 158 161 164 167 170 173 177 181 185 189 193 197 201 205 209 213 217 221 225 229 234 239 245 249 254 259 264 269 274 279 284))

;; The four quantizers multiply every dequantized coefficient in the frame.  They are read from
;; untyped tables once per frame, so what matters is not the tables but that the VALUE carries its
;; range to the callers — untyped, each of those multiplies compiled to a GENERIC-*.
(declaim (ftype (function (t) (integer 1 4096)) dc-quant ac-quant y2-dc-quant y2-ac-quant))
(defun qclamp (q) (max 0 (min 127 q)))
(defun dc-quant (qi) (the (integer 1 4096) (aref +dc-qlookup+ (qclamp qi))))
(defun ac-quant (qi) (the (integer 1 4096) (aref +ac-qlookup+ (qclamp qi))))
;; Second-order (Y2) quantizers, libvpx common/quant_common.c vp8_dc2quant / vp8_ac2quant.
;; The AC one is ac_qlookup*155/100 done as an exact integer op — (x*101581)>>16 — with a
;; floor of 8; rounding it as 155*x/100 to nearest disagrees at several quantizer indices
;; (e.g. qi=10: libvpx 21, nearest-rounding 22), which corrupts every Y2 AC coefficient.
(defun y2-dc-quant (qi) (* 2 (dc-quant qi)))
(defun y2-ac-quant (qi) (max 8 (ash (* (ac-quant qi) 101581) -16)))

(defun quantize-block (coeffs levels dcq acq)
  "Quantize 16 COEFFS into LEVELS: DC (index 0) by DCQ, AC (1..15) by ACQ.  Round to nearest."
  (dotimes (i 16 levels)
    (let* ((q (if (zerop i) dcq acq)) (c (aref coeffs i)))
      (setf (aref levels i) (if (minusp c) (- (floor (+ (- c) (floor q 2)) q))
                                (floor (+ c (floor q 2)) q))))))

(defun dequantize-block (levels coeffs dcq acq)
  (dotimes (i 16 coeffs)
    (setf (aref coeffs i) (* (aref levels i) (if (zerop i) dcq acq)))))
