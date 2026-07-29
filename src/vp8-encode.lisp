;;;; vp8-encode.lisp — a real VP8 keyframe encoder: DC+AC coefficients, correct entropy
;;;; contexts, and prediction-aware residuals.
;;;;
;;;; The three things the earlier bring-up got wrong, fixed here:
;;;;
;;;;  1. PREDICTION.  A macroblock is not coded against 128 — it is coded against its DC_PRED
;;;;     prediction, the average of the RECONSTRUCTED pixels above and to the left.  So we keep
;;;;     reconstruction planes and encode residual = source - prediction, macroblock by macroblock
;;;;     in raster order, reconstructing as we go (exactly what the decoder will do).
;;;;
;;;;  2. ENTROPY CONTEXT.  Coefficient probabilities are selected by a context: for the FIRST
;;;;     coded coefficient it is (left-block-had-nonzero + above-block-had-nonzero); after that it
;;;;     is 0/1/2 from the previous coefficient's magnitude.  We keep per-plane left/above flags.
;;;;
;;;;  3. FULL COEFFICIENTS.  All 16 coefficients per block in zigzag order, with the band table
;;;;     selecting probabilities, and EOB once the rest are zero.
;;;;
;;;; Luma uses 16x16 DC_PRED, so the sixteen 4x4 luma DCs go through the WHT into the Y2 block and
;;;; the luma blocks themselves are coded from coefficient 1.
;;;;
;;;; STATUS: correct.  ENCODE-GRAY-FRAME's second return value is the encoder's own reconstruction,
;;;; and it is bit-identical to what ffmpeg's decoder produces for every image, size and quantizer
;;;; index tested (flat, gradients, hard edges, uniform noise; 0..127; non-multiple-of-16 sizes).
;;;;
;;;; Two bugs were fixed to get here, both about matching the reference exactly:
;;;;
;;;;  a. SKIP_EOB_NODE.  The EOB branch of the coefficient tree is NOT coded after a ZERO
;;;;     coefficient — libvpx's GetCoeffs (decoder/detokenize.c) re-enters the tree below node 0
;;;;     in that case, and its tokenizer mirrors it with `t->skip_eob_node = (pt == 0)`.  Emitting
;;;;     that bit desynchronised the whole token partition as soon as any block had an interior
;;;;     zero, i.e. as soon as the picture had detail.  See WRITE-BLOCK.
;;;;
;;;;  b. The Y2 AC quantizer is libvpx's exact integer form (ac_q * 101581) >> 16 with a floor of
;;;;     8, not ac_q * 155/100 rounded to nearest; the two disagree at 63 of the 128 quantizer
;;;;     indices.  See Y2-AC-QUANT in vp8-dct.lisp.
;;;;
;;;; And one thing that turned out NOT to be a bug: a supposed 1.2x amplitude error (once papered
;;;; over by a `+y2-dc-fudge+' constant) was an artefact of the measurement, not the codec.
;;;; Decoding with `ffmpeg -pix_fmt gray' makes swscale expand limited-range luma to full range
;;;; (x255/219 = 1.164); decode to yuv420p and take the Y plane to see what VP8 actually produced.

(in-package #:webrtc-media.vp8)

(defstruct (ectx (:conc-name ec-))
  "Entropy contexts: above (per MB column) and left (per MB), one flag per sub-block."
  above-y above-u above-v above-y2 left-y left-u left-v (left-y2 0))

(defun make-contexts (mb-cols)
  (make-ectx :above-y (make-array (* 4 mb-cols) :initial-element 0)
             :above-u (make-array (* 2 mb-cols) :initial-element 0)
             :above-v (make-array (* 2 mb-cols) :initial-element 0)
             :above-y2 (make-array mb-cols :initial-element 0)
             :left-y (make-array 4 :initial-element 0)
             :left-u (make-array 2 :initial-element 0)
             :left-v (make-array 2 :initial-element 0)))

(defun write-block (bw probs type coeffs first-coef ctx)
  "Code one 4x4 block's quantized COEFFS (already in zigzag order) starting at FIRST-COEF with
initial entropy context CTX.  Returns T if the block had any non-zero coefficient.

The EOB branch of the coefficient tree is coded for the first position (there it is really a
`this block has coefficients' flag) and after every NON-ZERO coefficient — but never after a
ZERO one.  libvpx's decoder (detokenize.c GetCoeffs) simply does not read that bit when the
previous token was DCT_0, and its encoder mirrors that with skip_eob_node = (pt == 0)."
  (let ((last -1))
    (loop for i from first-coef below 16 when (/= 0 (aref coeffs i)) do (setf last i))
    (if (< last 0)
        (progn (write-token bw probs type (aref +coef-bands+ first-coef) ctx +tok-eob+) nil)
        (let ((c ctx) (skip nil))
          (loop for i from first-coef to last
                for v = (aref coeffs i)
                do (write-coef bw probs type (aref +coef-bands+ i) c v skip)
                   (setf c (cond ((zerop v) 0) ((= 1 (abs v)) 1) (t 2))
                         skip (zerop c)))
          (when (< last 15)                     ; EOB unless the block ran to the end
            (write-token bw probs type (aref +coef-bands+ (1+ last)) c +tok-eob+))
          t))))

(defun zigzag-quantize (coeffs out dcq acq &key (first 0))
  "Quantize COEFFS (raster 4x4 order) into OUT in ZIGZAG order.  Positions below FIRST are zeroed."
  (declare (type (simple-array (signed-byte 32) (16)) coeffs out)
           (type (integer 1 4096) dcq acq) (type (integer 0 16) first)
           (optimize (speed 3) (safety 0)))
  (dotimes (i 16 out)
    (let* ((src (aref +zigzag+ i))
           (q (if (zerop i) dcq acq))
           (v (if (< i first) 0 (quant1 (aref coeffs src) q))))
      ;; DCT_MAX_VALUE: the token alphabet only reaches +-2047 (cat6 = 67 + 11 extra bits)
      (setf (aref out i) (max -2047 (min 2047 v))))))

;;; ---- prediction + reconstruction -------------------------------------------
(defun dc-pred (plane stride x y size)
  "VP8 DC_PRED for a SIZE x SIZE block at (X,Y) in PLANE: average of the reconstructed row above
and column to the left; 128 when neither exists."
  (let ((sum 0) (n 0))
    (when (plusp y) (dotimes (i size) (incf sum (aref plane (+ (* (1- y) stride) x i))) (incf n)))
    (when (plusp x) (dotimes (i size) (incf sum (aref plane (+ (* (+ y i) stride) (1- x)))) (incf n)))
    (if (zerop n) 128 (floor (+ sum (floor n 2)) n))))


(defun code-chroma-mb (tw probs src src-w src-h recon rw mbx mby above left uvdcq uvacq)
  "Code one macroblock's 8x8 chroma plane region: DC_PRED against the reconstruction, then the
four 4x4 sub-blocks (type 2, from coefficient 0).  SRC may be NIL for a neutral (128) plane.
Reconstructs into RECON so later macroblocks predict from what the decoder will have."
  (let* ((ox (* mbx 8)) (oy (* mby 8))
         (pred (dc-pred recon rw ox oy 8))
         (blk (mkblk)) (co (mkblk)) (q (mkblk)) (dq (mkblk)) (rec (mkblk)))
    (dotimes (b 4)
      (let ((bx (+ ox (* 4 (mod b 2)))) (by (+ oy (* 4 (floor b 2)))))
        (dotimes (i 16)
          (let* ((px (min (1- src-w) (+ bx (mod i 4)))) (py (min (1- src-h) (+ by (floor i 4)))))
            (setf (aref blk i) (- (if src (aref src (+ (* py src-w) px)) 128) pred))))
        (fdct4x4 blk co)
        (zigzag-quantize co q uvdcq uvacq)
        (let* ((ai (+ (* 2 mbx) (mod b 2))) (li (floor b 2))
               (ctx (+ (aref above ai) (aref left li)))
               (nz (write-block tw probs +blk-uv+ q 0 ctx)))
          (setf (aref above ai) (if nz 1 0) (aref left li) (if nz 1 0)))
        (dotimes (i 16) (setf (aref dq (aref +zigzag+ i)) (* (aref q i) (if (zerop i) uvdcq uvacq))))
        (idct4x4 dq rec)
        (dotimes (i 16)
          (let ((px (+ bx (mod i 4))) (py (+ by (floor i 4))))
            (setf (aref recon (+ (* py rw) px))
                  (max 0 (min 255 (+ pred (aref rec i)))))))))))

(defun encode-gray-frame (src width height &key (qi 30) u v)
  "Encode an 8-bit grayscale image SRC (WIDTH*HEIGHT bytes, row-major) as a VP8 keyframe with real
DC+AC coefficients.  U and V, when supplied, are the 4:2:0 chroma planes (each ceiling(w/2) x
ceiling(h/2)); without them chroma stays neutral (128) and the picture is greyscale.
Returns (values frame-bytes luma-reconstruction)."
  (let* ((mb-cols (ceiling width 16)) (mb-rows (ceiling height 16))
         (pw (* mb-cols 16)) (ph (* mb-rows 16))
         (bw (make-bwriter)) (tw (make-bwriter))
         (probs +default-coef-probs+)
         (dcq (dc-quant qi)) (acq (ac-quant qi)) (y2dcq (y2-dc-quant qi)) (y2acq (y2-ac-quant qi))
         (recon (make-array (* pw ph) :element-type '(unsigned-byte 8) :initial-element 128))
         ;; chroma: 4:2:0, so each macroblock owns an 8x8 region of each plane
         (cw (* mb-cols 8)) (chh (* mb-rows 8))
         (sw (ceiling width 2)) (sh (ceiling height 2))
         (urec (make-array (* cw chh) :element-type '(unsigned-byte 8) :initial-element 128))
         (vrec (make-array (* cw chh) :element-type '(unsigned-byte 8) :initial-element 128))
         ;; uv DC quantizer is clamped to 132 (RFC 6386 s14.1); uv AC is the plain AC quantizer
         (uvdcq (min 132 (dc-quant qi))) (uvacq (ac-quant qi))
         (ctxs (make-contexts mb-cols))
         (blk (mkblk)) (co (mkblk)) (dq (mkblk)) (rec (mkblk))
         (y2in (mkblk)) (y2co (mkblk)) (y2q (mkblk)) (y2dq (mkblk)))
    ;; ---- frame header ----
    (bwrite-literal bw 0 1) (bwrite-literal bw 0 1) (bwrite-literal bw 0 1)
    (bwrite-literal bw 0 1) (bwrite-literal bw 0 6) (bwrite-literal bw 0 3)
    (bwrite-literal bw 0 1) (bwrite-literal bw 0 2) (bwrite-literal bw qi 7)
    (dotimes (i 5) (bwrite-literal bw 0 1))
    (bwrite-literal bw 0 1)                                  ; refresh_entropy_probs = 0
    (loop for p across +coeff-update-probs+ do (bwrite-bit bw p 0))
    (bwrite-literal bw 0 1)                                  ; mb_no_skip_coeff = 0
    (dotimes (i (* mb-cols mb-rows)) (write-ymode-dc bw) (write-uvmode-dc bw))
    ;; ---- macroblocks, raster order ----
    (dotimes (mby mb-rows)
      (fill (ec-left-y ctxs) 0) (fill (ec-left-u ctxs) 0) (fill (ec-left-v ctxs) 0)
      (setf (ec-left-y2 ctxs) 0)
      (dotimes (mbx mb-cols)
        (let* ((ox (* mbx 16)) (oy (* mby 16))
               (pred (dc-pred recon pw ox oy 16))
               (dcs (make-array 16 :element-type '(signed-byte 32))))
          ;; forward-transform each 4x4, stash its DC, quantize the ACs
          (let ((acq-blocks (make-array 16)))
            (dotimes (b 16)
              (let ((bx (+ ox (* 4 (mod b 4)))) (by (+ oy (* 4 (floor b 4)))))
                (dotimes (i 16)
                  (let* ((px (min (1- width) (+ bx (mod i 4)))) (py (min (1- height) (+ by (floor i 4)))))
                    (setf (aref blk i) (- (aref src (+ (* py width) px)) pred))))
                (fdct4x4 blk co)
                (setf (aref dcs b) (aref co 0))
                (let ((q (mkblk))) (zigzag-quantize co q dcq acq :first 1) (setf (aref acq-blocks b) q))))
            ;; Y2: WHT of the sixteen luma DCs
            (dotimes (i 16) (setf (aref y2in i) (aref dcs i)))
            (fwht4x4 y2in y2co)
            (zigzag-quantize y2co y2q y2dcq y2acq)
            ;; code Y2 (block type 1) with its own context
            (let* ((ctx (+ (aref (ec-above-y2 ctxs) mbx) (ec-left-y2 ctxs)))
                   (nz (write-block tw probs +blk-y2+ y2q 0 ctx)))
              (setf (aref (ec-above-y2 ctxs) mbx) (if nz 1 0) (ec-left-y2 ctxs) (if nz 1 0)))
            ;; code the 16 luma blocks (type 0, from coefficient 1)
            (dotimes (b 16)
              (let* ((bcol (mod b 4)) (brow (floor b 4))
                     (ctx (+ (aref (ec-above-y ctxs) (+ (* 4 mbx) bcol)) (aref (ec-left-y ctxs) brow)))
                     (nz (write-block tw probs +blk-y-after-y2+ (aref acq-blocks b) 1 ctx)))
                (setf (aref (ec-above-y ctxs) (+ (* 4 mbx) bcol)) (if nz 1 0)
                      (aref (ec-left-y ctxs) brow) (if nz 1 0))))
            ;; chroma: real 4:2:0 coding (neutral 128 when no planes were supplied)
            (code-chroma-mb tw probs u sw sh urec cw mbx mby
                            (ec-above-u ctxs) (ec-left-u ctxs) uvdcq uvacq)
            (code-chroma-mb tw probs v sw sh vrec cw mbx mby
                            (ec-above-v ctxs) (ec-left-v ctxs) uvdcq uvacq)
            ;; ---- reconstruct this MB exactly as the decoder will (feeds later predictions) ----
            (dotimes (i 16)
              (setf (aref y2dq i) (* (aref y2q i) (if (zerop i) y2dcq y2acq))))
            (let ((y2r (mkblk)))
              ;; scatter by zigzag index rather than searching for each raster index (was O(16) per
                 ;; coefficient, i.e. 256 searches per macroblock)
                 (dotimes (j 16) (setf (aref y2in (aref +zigzag+ j)) (aref y2dq j)))
              (iwht4x4 y2in y2r)
              (dotimes (b 16)
                (let ((q (aref acq-blocks b)))
                  (dotimes (i 16)
                    (setf (aref dq (aref +zigzag+ i)) (* (aref q i) (if (zerop i) dcq acq))))
                  (setf (aref dq 0) (aref y2r b))            ; DC comes back from the WHT
                  (idct4x4 dq rec)
                  (let ((bx (+ ox (* 4 (mod b 4)))) (by (+ oy (* 4 (floor b 4)))))
                    (dotimes (i 16)
                      (let ((px (+ bx (mod i 4))) (py (+ by (floor i 4))))
                        (setf (aref recon (+ (* py pw) px))
                              (max 0 (min 255 (+ pred (aref rec i))))))))))
              )))))
    (let ((part1 (bwrite-finish bw)) (part2 (bwrite-finish tw)) (frame (u8buf)))
      (write-uncompressed-header frame width height (length part1))
      (loop for b across part1 do (vector-push-extend b frame))
      (loop for b across part2 do (vector-push-extend b frame))
      ;; second value: the reconstruction (PW x PH), i.e. exactly the luma plane a conforming
      ;; decoder produces.  Useful as a self-check — it must match a real decoder bit for bit.
      ;; the reconstruction planes are exactly what a conforming decoder produces — they seed the
      ;; reference for the inter frames that follow (and double as a self-check).
      (values frame recon urec vrec pw ph))))
