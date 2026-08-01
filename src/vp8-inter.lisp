;;;; vp8-inter.lisp — inter (P) frames: the thing that makes a desktop stream cheap.
;;;;
;;;; A keyframe costs ~76 KB and ~300 ms for a 1280x800 screen, so keyframe-only streaming runs at
;;;; well under 1 fps.  But a desktop is mostly static: with an inter frame every macroblock that
;;;; has not changed is a single skip bit against the previous frame, and only the parts that
;;;; actually moved carry coefficients.
;;;;
;;;; TWO inter modes matter here, and a macroblock picks between them:
;;;;
;;;;   * ZEROMV against LAST_FRAME — the prediction is the co-located pixels of the previous
;;;;     reconstruction.  Free, and right for everything that appears, disappears or redraws in
;;;;     place, which is most of what a desktop does.
;;;;
;;;;   * a GLOBAL MOTION VECTOR — the prediction is the previous reconstruction shifted by one
;;;;     (dx,dy) shared by the whole frame.  A scroll or a window drag translates a large region
;;;;     by exactly one vector, and against ZEROMV alone that costs a full re-encode of every
;;;;     macroblock it touches: the rate control then rations it out over seconds of mush.  With
;;;;     the vector, the translated part is a skip bit and a mode, and only the newly exposed
;;;;     strip carries coefficients.
;;;;
;;;; The restriction that makes this cheap is INTEGER-PEL vectors.  Luma prediction is then a
;;;; plain block copy — no six-tap filter.  Chroma is not quite free: a 4:2:0 chroma vector is the
;;;; luma vector halved, so an ODD luma vector lands on a chroma half-pel and the decoder runs its
;;;; six-tap filter there.  %SIXTAP-8X8 reproduces that exactly (libvpx vp8_sixtap_predict8x8_c);
;;;; without it the reconstruction would drift from the decoder's on every odd scroll.
;;;;
;;;; Where the vector comes from, in order of preference:
;;;;   1. told to us — :MOTION (dx . dy), the same "the screen moved by this much" fact that
;;;;      drives an RFB CopyRect (glass's FB-TAKE-COPY hint);
;;;;   2. estimated — %ESTIMATE-GLOBAL-MV, a vote over a dozen high-detail probe macroblocks.
;;;;      Exhaustive along one axis (a scroll is exactly that) and a coarse 2-D grid otherwise.
;;;; A wrong estimate costs a little time and nothing else: every macroblock still compares the
;;;; two predictions and keeps the better one, so the newly exposed strip and any non-translating
;;;; content code as ZEROMV regardless.
;;;;
;;;; Both are gated: *MOTION-VECTORS* off restores the pure-ZEROMV encoder exactly, and
;;;; *MOTION-SEARCH* off keeps the vector but takes it only from a caller's hint.
;;;;
;;;; Entropy coding follows libvpx: vp8_mv_ref_tree with probabilities from vp8_mode_contexts
;;;; indexed by the neighbour counts of vp8_find_near_mvs (%FIND-NEAR-MVS reproduces it,
;;;; including the clamps), NEARESTMV/NEARMV when a neighbour already carries the vector, and
;;;; NEWMV — a delta against the neighbourhood's best MV — otherwise.  Vectors are chosen so the
;;;; decoder's own MV clamp never fires and the prediction never reads outside the reference.

(in-package #:webrtc-media.vp8)

(defparameter +mode-contexts+                  ; libvpx vp8_mode_contexts[6][4]
  #2A((7 1 1 143) (14 18 14 107) (135 64 57 68) (60 56 128 65)
      (159 134 128 34) (234 188 128 28)))

(defparameter +mv-update-probs+                ; libvpx vp8_mv_update_probs[2][19]
  #2A((237 246 253 253 254 254 254 254 254 254 254 254 254 254 250 250 252 254 254)
      (231 243 245 253 254 254 254 254 254 254 254 254 254 254 251 251 254 254 254)))

(defparameter +mv-default-probs+               ; libvpx vp8_default_mv_context[2], row then column
  ;; layout (MVPindices): 0 is_short, 1 sign, 2..8 short tree, 9..18 long bits
  #2A((162 128  225 146 172 147 214 39 156   128 129 132 75 145 178 206 239 254 254)
      (164 128  204 170 119 235 140 230 228  128 130 130 74 148 180 203 236 254 254)))

;;; libvpx vp8_sub_pel_filters[8][6].  Only row 4 (half-pel) and row 0 (identity) are ever used
;;; here: integer-pel luma vectors put chroma on a whole or half chroma pixel and nowhere else.
(defparameter +sub-pel-filters+
  #2A((0 0 128 0 0 0) (0 -6 123 12 -1 0) (2 -11 108 36 -8 1) (0 -9 93 50 -6 0)
      (3 -16 77 77 -16 3) (0 -6 50 93 -9 0) (1 -8 36 108 -11 2) (0 -1 12 123 -6 0)))

(defconstant +prob-intra+ 8)                   ; P(intra); we always send inter, so keep it low
(defconstant +prob-last+ 254)                  ; P(LAST); we always reference LAST
(defconstant +prob-gf+ 128)
(defconstant +prob-skip+ 128)

(defconstant +mv-margin+ 128)                  ; libvpx LEFT_TOP_MARGIN / RIGHT_BOTTOM_MARGIN, 1/8 pel
(defconstant +mv-max-pel+ 255)                 ; a component codes 10 bits of quarter-pel: 1023/4

(defparameter *motion-vectors* t
  "Gate for the global motion vector.  NIL restores the pure-ZEROMV encoder: every macroblock is
coded against its co-located predecessor, exactly as before motion vectors existed.")

(defparameter *motion-search* t
  "When no :MOTION hint is supplied, estimate the frame's global vector from the pixels.  NIL
makes the motion path hint-only (and therefore free on frames with no hint).")

(defparameter *motion-search-min-dirty* 0.12
  "Estimate only when at least this fraction of the macroblocks changed.  A scroll or a drag
dirties a large part of the screen; a blinking cursor does not, and is not worth searching for.")

(defstruct (venc (:conc-name ve-))
  "Stateful encoder: holds the previous reconstruction, which is the reference for the next frame."
  ;; slot types matter: without them every plane access in the macroblock loop goes through
  ;; SB-KERNEL:HAIRY-DATA-VECTOR-REF, which the profile showed was ~10% of encode time
  (width 0 :type fixnum) (height 0 :type fixnum)
  (mb-cols 0 :type fixnum) (mb-rows 0 :type fixnum)
  (pw 0 :type fixnum) (ph 0 :type fixnum) (cw 0 :type fixnum) (chh 0 :type fixnum)
  (ref-y nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (ref-u nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (ref-v nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (prev-y nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (prev-u nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (prev-v nil :type (or null (simple-array (unsigned-byte 8) (*))))
  ;; macroblocks last coded at a degraded quantizer.  When the screen goes quiet we re-code just
  ;; these at a low qi, so a burst of motion leaves no lasting mush behind.
  (stale-mbs nil :type (or null simple-bit-vector))
  (motion nil)                                 ; MOTION-STATE, allocated on first use
  (have-ref nil) (frames 0))

(defclass motion-state ()
  ((cur-y :accessor ms-cur-y :initarg :cur-y)
   (cur-u :accessor ms-cur-u :initarg :cur-u)
   (cur-v :accessor ms-cur-v :initarg :cur-v)
   (mv-row :accessor ms-mv-row :initarg :mv-row)   ; per-MB coded vector, 1/8 pel, this frame
   (mv-col :accessor ms-mv-col :initarg :mv-col)
   (was-stale :accessor ms-was-stale :initarg :was-stale)
   (last-mv :accessor ms-last-mv :initform nil))   ; vector accepted last frame: a strong prior
  (:documentation
   "State the motion path needs to live across frames: a SECOND reconstruction buffer, and the
per-macroblock vectors of the frame being coded.

The second buffer is what makes motion compensation possible at all.  ZEROMV can reconstruct in
place — a macroblock's prediction is the very pixels it overwrites — but a motion vector reads
somewhere else in the reference, and in raster order that somewhere else may already have been
overwritten by this frame.  So prediction reads the previous reconstruction and reconstruction
writes the new one, and the two swap at the end of a frame.  On a frame with no motion the two
are the same array and the old in-place path is untouched."))

(defun %motion-state (enc)
  (or (ve-motion enc)
      (setf (ve-motion enc)
            (let ((n (* (ve-mb-cols enc) (ve-mb-rows enc))))
              (make-instance
               'motion-state
               :cur-y (make-array (* (ve-pw enc) (ve-ph enc)) :element-type '(unsigned-byte 8))
               :cur-u (make-array (* (ve-cw enc) (ve-chh enc)) :element-type '(unsigned-byte 8))
               :cur-v (make-array (* (ve-cw enc) (ve-chh enc)) :element-type '(unsigned-byte 8))
               :mv-row (make-array n :element-type '(signed-byte 16) :initial-element 0)
               :mv-col (make-array n :element-type '(signed-byte 16) :initial-element 0)
               :was-stale (make-array n :element-type 'bit :initial-element 0))))))

(defun make-encoder (width height)
  (let* ((mc (ceiling width 16)) (mr (ceiling height 16))
         (pw (* mc 16)) (ph (* mr 16)) (cw (* mc 8)) (chh (* mr 8)))
    (make-venc :width width :height height :mb-cols mc :mb-rows mr :pw pw :ph ph :cw cw :chh chh
               :ref-y (make-array (* pw ph) :element-type '(unsigned-byte 8) :initial-element 128)
               :ref-u (make-array (* cw chh) :element-type '(unsigned-byte 8) :initial-element 128)
               :ref-v (make-array (* cw chh) :element-type '(unsigned-byte 8) :initial-element 128)
               :prev-y (make-array (* width height) :element-type '(unsigned-byte 8) :initial-element 0)
               :prev-u (make-array (* (ceiling width 2) (ceiling height 2)) :element-type '(unsigned-byte 8) :initial-element 0)
               :prev-v (make-array (* (ceiling width 2) (ceiling height 2)) :element-type '(unsigned-byte 8) :initial-element 0)
               :stale-mbs (make-array (* mc mr) :element-type 'bit :initial-element 0))))

;;; ---- neighbour vectors (libvpx vp8_find_near_mvs) --------------------------
;;;
;;; The decoder derives NEAREST/NEAR/best from the left, above and above-left macroblocks, and
;;; picks the mv_ref probabilities from how many of them agree.  We must reproduce it exactly: the
;;; mode we code is only decodable if the decoder infers the same vector for it.  Every macroblock
;;; we emit is inter, so the only "intra" neighbours are the ones outside the frame.

(defun %mv-ref-p0 (mbx mby)
  "Probability for the ZEROMV branch of vp8_mv_ref_tree when EVERY macroblock in the frame is
ZEROMV: the neighbour counts then collapse to a function of position alone (the borders count as
intra and contribute nothing), so cnt[0] is 0 at the top-left, 2 along the top and left edges and
5 in the interior.  %FIND-NEAR-MVS produces the same value the long way round; this is the
shortcut the motion-free path takes."
  (aref +mode-contexts+ (cond ((and (zerop mbx) (zerop mby)) 0)
                              ((or (zerop mbx) (zerop mby)) 2)
                              (t 5))
        0))

(defun %clamp-mv (row col mbx mby mc mr)
  "libvpx vp8_clamp_mv2: keep a candidate vector within a macroblock's legal window (1/8 pel)."
  (declare (type fixnum row col mbx mby mc mr))
  (let ((lo-c (- (* -128 mbx) +mv-margin+)) (hi-c (+ (* 128 (- mc 1 mbx)) +mv-margin+))
        (lo-r (- (* -128 mby) +mv-margin+)) (hi-r (+ (* 128 (- mr 1 mby)) +mv-margin+)))
    (values (max lo-r (min hi-r row)) (max lo-c (min hi-c col)))))

(defun %find-near-mvs (ms mbx mby mc mr out probs)
  "Fill OUT with #(nearest-row nearest-col near-row near-col best-row best-col) and PROBS with the
four vp8_mv_ref_tree probabilities for this macroblock."
  (declare (type (simple-array fixnum (6)) out) (type (simple-array (unsigned-byte 8) (4)) probs)
           (type fixnum mbx mby mc mr) (optimize (speed 3) (safety 0)))
  (let ((mvr (the (simple-array (signed-byte 16) (*)) (ms-mv-row ms)))
        (mvc (the (simple-array (signed-byte 16) (*)) (ms-mv-col ms)))
        (nr (make-array 4 :element-type 'fixnum :initial-element 0))
        (nc (make-array 4 :element-type 'fixnum :initial-element 0))
        (cnt (make-array 4 :element-type 'fixnum :initial-element 0))
        (n 0))
    (declare (dynamic-extent nr nc cnt) (type fixnum n))
    (macrolet ((nb (dx dy weight)
                 ;; one neighbour: outside the frame counts as intra and contributes nothing
                 `(let ((bx (+ mbx ,dx)) (by (+ mby ,dy)))
                    (when (and (>= bx 0) (>= by 0))
                      (let* ((i (+ (* by mc) bx)) (r (aref mvr i)) (c (aref mvc i)))
                        (if (or (/= 0 r) (/= 0 c))
                            (progn
                              (when (or (/= r (aref nr n)) (/= c (aref nc n)))
                                (incf n) (setf (aref nr n) r (aref nc n) c))
                              (incf (aref cnt n) ,weight))
                            (incf (aref cnt 0) ,weight)))))))
      (nb 0 -1 2)                              ; above
      (nb -1 0 2)                              ; left
      (nb -1 -1 1))                            ; above-left
    ;; three distinct vectors and the last repeats the first: libvpx merges them
    (when (and (= n 3) (= (aref nr 3) (aref nr 1)) (= (aref nc 3) (aref nc 1)))
      (incf (aref cnt 1)))
    (setf (aref cnt 3) 0)                      ; cnt[CNT_SPLITMV]: we never emit SPLITMV
    (when (> (aref cnt 2) (aref cnt 1))
      (rotatef (aref cnt 1) (aref cnt 2))
      (rotatef (aref nr 1) (aref nr 2)) (rotatef (aref nc 1) (aref nc 2)))
    (when (>= (aref cnt 1) (aref cnt 0))
      (setf (aref nr 0) (aref nr 1) (aref nc 0) (aref nc 1)))
    ;; near_mvs[0] is "best", [1] nearest, [2] near; OUT wants nearest, near, best
    (dotimes (i 3)
      (multiple-value-bind (r c) (%clamp-mv (aref nr i) (aref nc i) mbx mby mc mr)
        (let ((j (mod (* 2 (+ i 2)) 6)))
          (setf (aref out j) r (aref out (1+ j)) c))))
    (dotimes (i 4) (setf (aref probs i) (aref +mode-contexts+ (aref cnt i) i)))
    (values)))

;;; ---- mode + vector coding (libvpx encodemv.c) ------------------------------

(defun %write-mv-ref (bw probs mode)
  "vp8_mv_ref_tree: ZEROMV | NEARESTMV | NEARMV | NEWMV (SPLITMV is never emitted)."
  (declare (type (simple-array (unsigned-byte 8) (4)) probs))
  (ecase mode
    (:zero (bwrite-bit bw (aref probs 0) 0))
    (:nearest (bwrite-bit bw (aref probs 0) 1) (bwrite-bit bw (aref probs 1) 0))
    (:near (bwrite-bit bw (aref probs 0) 1) (bwrite-bit bw (aref probs 1) 1)
           (bwrite-bit bw (aref probs 2) 0))
    (:new (bwrite-bit bw (aref probs 0) 1) (bwrite-bit bw (aref probs 1) 1)
          (bwrite-bit bw (aref probs 2) 1) (bwrite-bit bw (aref probs 3) 0))))

(defun %write-small-mv (bw comp x)
  "vp8_small_mvtree, the balanced 3-level tree for a component magnitude below 8."
  (declare (type (integer 0 7) x))
  (flet ((p (i) (aref +mv-default-probs+ comp (+ 2 i))))
    (bwrite-bit bw (p 0) (ldb (byte 1 2) x))
    (if (< x 4)
        (progn (bwrite-bit bw (p 1) (ldb (byte 1 1) x))
               (bwrite-bit bw (p (if (< x 2) 2 3)) (ldb (byte 1 0) x)))
        (progn (bwrite-bit bw (p 4) (ldb (byte 1 1) x))
               (bwrite-bit bw (p (if (< x 6) 5 6)) (ldb (byte 1 0) x))))))

(defun %write-mv-component (bw comp v)
  "One vector component, in QUARTER-pel wire units (COMP 0 = row, 1 = column).

Mirrors RFC 6386 s17.2 read_mvcomponent: below 8 it is a tree-coded magnitude; at or above 8 it
is bits 0-2, then bits 9 down to 4, and then bit 3 — which is only coded when some higher bit is
set, because otherwise the value would have been small and bit 3 is implicitly 1."
  (declare (type (integer 0 1) comp) (type fixnum v))
  (let ((x (abs v)))
    (assert (< x 1024) () "vp8: motion vector component ~a out of range" v)
    (cond ((< x 8)
           (bwrite-bit bw (aref +mv-default-probs+ comp 0) 0)
           (%write-small-mv bw comp x)
           (when (zerop x) (return-from %write-mv-component)))   ; a zero component has no sign
          (t
           (bwrite-bit bw (aref +mv-default-probs+ comp 0) 1)
           (dotimes (i 3)
             (bwrite-bit bw (aref +mv-default-probs+ comp (+ 9 i)) (ldb (byte 1 i) x)))
           (loop for i from 9 downto 4
                 do (bwrite-bit bw (aref +mv-default-probs+ comp (+ 9 i)) (ldb (byte 1 i) x)))
           (when (logtest x #xfff0)
             (bwrite-bit bw (aref +mv-default-probs+ comp 12) (ldb (byte 1 3) x)))))
    (bwrite-bit bw (aref +mv-default-probs+ comp 1) (if (minusp v) 1 0))))

;;; ---- prediction ------------------------------------------------------------

(defun %copy-pred (ref rw sx sy dst size)
  "Integer-pel prediction: SIZE x SIZE block from REF at (SX,SY) into DST (stride SIZE)."
  (declare (type (simple-array (unsigned-byte 8) (*)) ref dst)
           (type fixnum rw sx sy size) (optimize (speed 3) (safety 0)))
  (dotimes (r size)
    (let ((s (+ (* (+ sy r) rw) sx)) (d (* r size)))
      (replace dst ref :start1 d :end1 (+ d size) :start2 s :end2 (+ s size)))))

(defun %sixtap-8x8 (ref rw sx sy xoff yoff dst tmp)
  "libvpx vp8_sixtap_predict8x8_c: two-pass 6-tap into DST (8x8, stride 8).

Reads REF from (SX-2,SY-2) to (SX+10,SY+10) — the filter taps reach two pixels back and three
forward — so the caller must have checked those bounds.  The intermediate pass is clamped to
0..255 exactly as libvpx does; skipping that clamp would put us a level off the decoder."
  (declare (type (simple-array (unsigned-byte 8) (*)) ref dst)
           (type (simple-array (signed-byte 32) (104)) tmp)
           (type fixnum rw sx sy) (type (integer 0 7) xoff yoff)
           (optimize (speed 3) (safety 0)))
  (let ((h0 (aref +sub-pel-filters+ xoff 0)) (h1 (aref +sub-pel-filters+ xoff 1))
        (h2 (aref +sub-pel-filters+ xoff 2)) (h3 (aref +sub-pel-filters+ xoff 3))
        (h4 (aref +sub-pel-filters+ xoff 4)) (h5 (aref +sub-pel-filters+ xoff 5))
        (v0 (aref +sub-pel-filters+ yoff 0)) (v1 (aref +sub-pel-filters+ yoff 1))
        (v2 (aref +sub-pel-filters+ yoff 2)) (v3 (aref +sub-pel-filters+ yoff 3))
        (v4 (aref +sub-pel-filters+ yoff 4)) (v5 (aref +sub-pel-filters+ yoff 5)))
    (declare (type fixnum h0 h1 h2 h3 h4 h5 v0 v1 v2 v3 v4 v5))
    (dotimes (r 13)                                       ; horizontal, 8 + 5 rows
      (let ((base (+ (* (+ sy r -2) rw) sx)) (o (* r 8)))
        (declare (type fixnum base o))
        (dotimes (c 8)
          (let ((p (+ base c)))
            (setf (aref tmp (+ o c))
                  (max 0 (min 255 (ash (+ 64 (* h0 (aref ref (- p 2))) (* h1 (aref ref (- p 1)))
                                          (* h2 (aref ref p)) (* h3 (aref ref (+ p 1)))
                                          (* h4 (aref ref (+ p 2))) (* h5 (aref ref (+ p 3))))
                                       -7))))))))
    (dotimes (r 8)                                        ; vertical
      (let ((q (* (+ r 2) 8)) (d (* r 8)))
        (declare (type fixnum q d))
        (dotimes (c 8)
          (let ((k (+ q c)))
            (setf (aref dst (+ d c))
                  (max 0 (min 255 (ash (+ 64 (* v0 (aref tmp (- k 16))) (* v1 (aref tmp (- k 8)))
                                          (* v2 (aref tmp k)) (* v3 (aref tmp (+ k 8)))
                                          (* v4 (aref tmp (+ k 16))) (* v5 (aref tmp (+ k 24))))
                                       -7))))))))))

(declaim (inline %uv-mv))
(defun %uv-mv (mv-pel)
  "4:2:0 chroma component of an integer-pel luma vector: (values integer-offset eighth-pel-fraction).
libvpx halves the 1/8-pel luma vector away from zero, so an odd luma pixel is a chroma half-pel."
  (declare (type fixnum mv-pel))
  (let ((uv (* 4 mv-pel)))                     ; = ((8*mv) rounded away from zero) / 2
    (values (ash uv -3) (logand uv 7))))

(defun %mv-fits-p (mbx mby mvx mvy pw ph cw chh)
  "T if the whole prediction for this macroblock — luma block and the chroma filter's tap window —
lies inside the reference planes.  Outside them the decoder would use its replicated border, which
we do not keep; those macroblocks fall back to ZEROMV."
  (declare (type fixnum mbx mby mvx mvy pw ph cw chh))
  (let ((ox (+ (* 16 mbx) mvx)) (oy (+ (* 16 mby) mvy)))
    (and (>= ox 0) (>= oy 0) (<= (+ ox 16) pw) (<= (+ oy 16) ph)
         (multiple-value-bind (ix fx) (%uv-mv mvx)
           (multiple-value-bind (iy fy) (%uv-mv mvy)
             (let ((cx (+ (* 8 mbx) ix)) (cy (+ (* 8 mby) iy)))
               (if (and (zerop fx) (zerop fy))
                   (and (>= cx 0) (>= cy 0) (<= (+ cx 8) cw) (<= (+ cy 8) chh))
                   (and (>= (- cx 2) 0) (>= (- cy 2) 0)
                        (<= (+ cx 11) cw) (<= (+ cy 11) chh)))))))))

(defun %predict-chroma (ref rw mbx mby mvx mvy dst tmp)
  "Build the 8x8 chroma prediction for a macroblock at luma vector (MVX,MVY)."
  (multiple-value-bind (ix fx) (%uv-mv mvx)
    (multiple-value-bind (iy fy) (%uv-mv mvy)
      (let ((cx (+ (* 8 mbx) ix)) (cy (+ (* 8 mby) iy)))
        (if (and (zerop fx) (zerop fy))
            (%copy-pred ref rw cx cy dst 8)
            (%sixtap-8x8 ref rw cx cy fx fy dst tmp))))))

;;; ---- change detection + block distortion -----------------------------------

(defun %mb-unchanged-p (src prev sw sh ox oy size)
  "T if the SIZE x SIZE macroblock at (OX,OY) is pixel-identical between this source frame and the
previous one.  Deliberately compares SOURCE to SOURCE, not source to reconstruction: the
reconstruction is lossy, so comparing against it reports every macroblock as changed and re-codes
the whole screen every frame.  What we want for a desktop is 'did this actually change'."
  (declare (type (simple-array (unsigned-byte 8) (*)) src prev)
           (type fixnum sw sh ox oy size)
           (optimize (speed 3) (safety 0)))
  (dotimes (r size t)
    (let ((sy (+ oy r)))
      (when (< sy sh)
        (let ((row (* sy sw)))
          (declare (type fixnum row))
          (dotimes (c size)
            (let ((sx (+ ox c)))
              (when (< sx sw)
                (unless (= (aref src (+ row sx)) (aref prev (+ row sx)))
                  (return-from %mb-unchanged-p nil))))))))))

(defun %mb-moved-p (src prev sw sh ox oy mvx mvy size)
  "T if this macroblock's pixels are exactly the previous frame's pixels displaced by (MVX,MVY) —
i.e. this part of the screen translated and nothing else happened to it.  Then the vector alone
reproduces it and the macroblock needs no coefficients at all."
  (declare (type (simple-array (unsigned-byte 8) (*)) src prev)
           (type fixnum sw sh ox oy mvx mvy size)
           (optimize (speed 3) (safety 0)))
  (dotimes (r size t)
    (let ((sy (+ oy r)) (ry (+ oy r mvy)))
      (when (< sy sh)
        (unless (and (>= ry 0) (< ry sh)) (return-from %mb-moved-p nil))
        (let ((row (* sy sw)) (rrow (* ry sw)))
          (declare (type fixnum row rrow))
          (dotimes (c size)
            (let ((sx (+ ox c)) (rx (+ ox c mvx)))
              (when (< sx sw)
                (unless (and (>= rx 0) (< rx sw)) (return-from %mb-moved-p nil))
                (unless (= (aref src (+ row sx)) (aref prev (+ rrow rx)))
                  (return-from %mb-moved-p nil))))))))))

(defun %mb-moved-chroma-p (src prev sw sh cx cy mvx mvy tmp buf)
  "T if this macroblock's chroma is the previous frame's chroma displaced by the 4:2:0 halving of
the luma vector (MVX,MVY).

An ODD luma vector puts chroma on a half-pel, where the prediction is the decoder's six-tap filter
and cannot be pixel-identical to any source pixels.  There we accept a small error instead of
paying for coefficients: desktop chroma is nearly flat, so the filter costs a couple of levels at
most, and the macroblock keeps its stale mark so the idle pass can sharpen it."
  (multiple-value-bind (ix fx) (%uv-mv mvx)
    (multiple-value-bind (iy fy) (%uv-mv mvy)
      (if (and (zerop fx) (zerop fy))
          (%mb-moved-p src prev sw sh cx cy ix iy 8)
          (let ((rx (+ cx ix)) (ry (+ cy iy)))
            (and (>= (- rx 2) 0) (>= (- ry 2) 0) (<= (+ rx 11) sw) (<= (+ ry 11) sh)
                 (progn
                   (%sixtap-8x8 prev sw rx ry fx fy buf tmp)
                   (dotimes (r 8 t)
                     (let ((py (+ cy r)))
                       (when (< py sh)
                         (dotimes (c 8)
                           (let ((px (+ cx c)))
                             (when (< px sw)
                               (when (> (abs (- (aref src (+ (* py sw) px))
                                                (aref buf (+ (* r 8) c))))
                                        3)
                                 (return-from %mb-moved-chroma-p nil)))))))))))))))

(defun %block-sad (src sw sh ox oy ref rw rx ry size step)
  "Sum of absolute differences between the source macroblock at (OX,OY) and the reference block at
(RX,RY), sampling every STEP-th row and column.  Source reads clamp at the picture edge, as the
residual coder does."
  (declare (type (simple-array (unsigned-byte 8) (*)) src ref)
           (type fixnum sw sh ox oy rw rx ry size step)
           (optimize (speed 3) (safety 0)))
  (let ((sad 0))
    (declare (type fixnum sad))
    (do ((r 0 (+ r step))) ((>= r size) sad)
      (let ((srow (* (min (1- sh) (+ oy r)) sw)) (rrow (* (+ ry r) rw)))
        (declare (type fixnum srow rrow))
        (do ((c 0 (+ c step))) ((>= c size))
          (incf sad (abs (- (aref src (+ srow (min (1- sw) (+ ox c))))
                            (aref ref (+ rrow rx c))))))))))

(defun %sad16-fast (src sw ox oy ref rw rx ry step)
  "SAD of two 16x16 blocks that are both known to be fully inside their planes — the estimator's
inner loop, run hundreds of thousands of times per searched frame, so it does no clamping."
  (declare (type (simple-array (unsigned-byte 8) (*)) src ref)
           (type fixnum sw ox oy rw rx ry step)
           (optimize (speed 3) (safety 0)))
  (let ((sad 0))
    (declare (type fixnum sad))
    (do ((r 0 (+ r step))) ((>= r 16) sad)
      (let ((s (+ (* (+ oy r) sw) ox)) (q (+ (* (+ ry r) rw) rx)))
        (declare (type fixnum s q))
        (do ((c 0 (+ c step))) ((>= c 16))
          (incf sad (abs (- (aref src (+ s c)) (aref ref (+ q c))))))))))

;;; ---- global motion estimation ---------------------------------------------
;;;
;;; Not a per-macroblock motion search: one vector for the frame, found by asking a dozen
;;; high-detail macroblocks where they went and taking the answer they agree on.  A vote is what
;;; makes it robust — on a window drag the probes in the newly exposed strip disagree with each
;;; other and with everyone, while the probes on the window itself all name the same vector.

(defun %mb-activity (y w h ox oy)
  "Cheap detail measure: gradient energy along BOTH axes, whichever is smaller.  A macroblock that
is flat in one direction — a run of blank terminal line, a smooth wallpaper gradient — matches at
every displacement along it and would vote for nonsense."
  (declare (type (simple-array (unsigned-byte 8) (*)) y) (type fixnum w h ox oy)
           (optimize (speed 3) (safety 0)))
  (let ((ah 0) (av 0))
    (declare (type fixnum ah av))
    (do ((r 0 (+ r 2))) ((>= r 14))
      (let ((row (* (min (1- h) (+ oy r)) w))
            (row2 (* (min (1- h) (+ oy r 2)) w)))
        (declare (type fixnum row row2))
        (do ((c 0 (+ c 2))) ((>= c 14))
          (let ((x (min (1- w) (+ ox c))) (x2 (min (1- w) (+ ox c 2))))
            (incf ah (abs (- (aref y (+ row x)) (aref y (+ row x2)))))
            (incf av (abs (- (aref y (+ row x)) (aref y (+ row2 x)))))))))
    (min ah av)))

(defun %pick-probes (enc y dirty &key (want 16))
  "Choose up to WANT detailed, changed macroblocks as (mbx mby).  Detail first — a flat macroblock
matches everywhere — but spread out too: a dozen probes all on one line of terminal text would
agree on whatever that line's periodicity says rather than on where the screen went."
  (let* ((mc (ve-mb-cols enc)) (mr (ve-mb-rows enc))
         (w (ve-width enc)) (h (ve-height enc))
         (step (max 1 (floor mc 24)))
         (cands '()))
    (loop for mby from 1 below (1- mr) by step do
      (loop for mbx from 1 below (1- mc) by step do
        (let ((i (+ (* mby mc) mbx)))
          (when (and (<= (+ (* 16 mbx) 16) w) (<= (+ (* 16 mby) 16) h)
                     (or (null dirty) (= 1 (aref (the simple-bit-vector dirty) i))))
            (let ((a (%mb-activity y w h (* 16 mbx) (* 16 mby))))
              (when (> a 200) (push (list a mbx mby) cands)))))))
    (let ((taken '()))
      (dolist (c (sort cands #'> :key #'first))
        (when (and (< (length taken) want)
                   (notany (lambda (p) (and (< (abs (- (first p) (second c))) 3)
                                            (< (abs (- (second p) (third c))) 3)))
                           taken))
          (push (cdr c) taken)))
      taken)))

(defun %probe-sad (y prev w h probes mvx mvy)
  "Total SAD of the probe macroblocks against the previous SOURCE frame displaced by (MVX,MVY),
and how many probes matched it near-exactly.  Comparing source to source keeps quantization noise
out of the decision."
  (declare (type (simple-array (unsigned-byte 8) (*)) y prev) (type fixnum w h mvx mvy)
           (optimize (speed 3) (safety 1)))
  (let ((total 0) (hits 0))
    (declare (type fixnum total hits))
    (dolist (p probes (values total hits))
      (let* ((ox (* 16 (first p))) (oy (* 16 (second p)))
             (rx (+ ox mvx)) (ry (+ oy mvy)))
        (declare (type fixnum ox oy rx ry))
        (if (and (>= rx 0) (>= ry 0) (<= (+ rx 16) w) (<= (+ ry 16) h))
            (let ((sad (%sad16-fast y w ox oy prev w rx ry 2)))
              (incf total sad)
              (when (< sad 64) (incf hits)))
            (incf total 65536))))))

(defun %vote (y prev w h probes candidates)
  "Score CANDIDATES over the probes; return the one with the most near-exact matches (ties to the
lower total SAD), or NIL if none convinces."
  (let ((best nil) (best-hits 0) (best-sad most-positive-fixnum))
    (dolist (c candidates)
      (multiple-value-bind (sad hits) (%probe-sad y prev w h probes (car c) (cdr c))
        (when (or (> hits best-hits) (and (= hits best-hits) (< sad best-sad)))
          (setf best c best-hits hits best-sad sad))))
    (when (and best (>= best-hits (max 3 (ceiling (length probes) 3)))
               (or (/= 0 (car best)) (/= 0 (cdr best))))
      best)))

(defun %rank-votes (votes n)
  "The N most-voted keys of the VOTES table, most first."
  (let ((r (sort (loop for k being the hash-keys of votes using (hash-value v) collect (cons v k))
                 #'> :key #'car)))
    (mapcar #'cdr (subseq r 0 (min n (length r))))))

(defun %scan-axis (y prev w h probes axis limit)
  "Displacements along one AXIS (:x or :y) at which probe macroblocks find themselves again.
Exhaustive, because a scroll IS exactly a one-axis displacement and a coarse search on text falls
into the wrong line.

Every near-exact displacement votes, not just each probe's best: a block of terminal text matches
at EVERY multiple of the line pitch, so per-probe winners scatter across the periodicity while the
one displacement the whole screen shares collects a vote from all of them."
  (let ((votes (make-hash-table)))
    (dolist (p probes)
      (let ((ox (* 16 (first p))) (oy (* 16 (second p))))
        (loop for d from (- limit) to limit do
          (unless (zerop d)
            (let ((rx (if (eq axis :x) (+ ox d) ox)) (ry (if (eq axis :y) (+ oy d) oy)))
              (when (and (>= rx 0) (>= ry 0) (<= (+ rx 16) w) (<= (+ ry 16) h))
                (when (< (%sad16-fast y w ox oy prev w rx ry 4) 16)
                  (incf (gethash d votes 0)))))))))
    (mapcar (lambda (d) (if (eq axis :x) (cons d 0) (cons 0 d))) (%rank-votes votes 4))))

(defun %scan-grid (y prev w h probes limit step)
  "Per probe, ITS best 2-D displacement: a coarse grid to find the neighbourhood, then every pixel
inside that neighbourhood.  The fallback for a drag, which moves on both axes at once so nothing
separable finds it — and on detailed content a coarse grid alone lands BESIDE the answer, which is
no use when the point is an exact translation.  Distinct winners, most-named first.

Generation may be noisy; %VOTE is what decides.  A probe in the newly exposed strip names
something arbitrary, and nobody agrees with it."
  (let ((votes (make-hash-table :test #'equal)))
    (dolist (p probes)
      (let ((ox (* 16 (first p))) (oy (* 16 (second p))) (coarse '()))
        (flet ((sad-at (dx dy sample)
                 (let ((rx (+ ox dx)) (ry (+ oy dy)))
                   (if (and (>= rx 0) (>= ry 0) (<= (+ rx 16) w) (<= (+ ry 16) h))
                       (%sad16-fast y w ox oy prev w rx ry sample)
                       most-positive-fixnum))))
          ;; the three most promising coarse neighbourhoods, not just the best: on detailed
          ;; content the grid point next to the answer is not always the grid's own winner
          (loop for dy from (- limit) to limit by step do
            (loop for dx from (- limit) to limit by step do
              (let ((s (sad-at dx dy 4)))
                (when (< s most-positive-fixnum)
                  (push (list s dx dy) coarse)))))
          (dolist (c (subseq (sort coarse #'< :key #'first) 0 (min 3 (length coarse))))
            (let ((s most-positive-fixnum) (bx 0) (by 0))
              (loop for dy from (- (third c) step) to (+ (third c) step) do
                (loop for dx from (- (second c) step) to (+ (second c) step) do
                  (let ((v (sad-at dx dy 2)))
                    (when (< v s) (setf s v bx dx by dy)))))
              (unless (and (zerop bx) (zerop by))
                (incf (gethash (cons bx by) votes 0))))))))
    (%rank-votes votes 6)))

(defun %estimate-global-mv (enc y dirty)
  "Estimate the frame's global motion vector, in whole pixels, or NIL.  The vector is the
REFERENCE offset: the prediction for a block comes from the previous frame at +(mvx,mvy)."
  (let* ((w (ve-width enc)) (h (ve-height enc))
         (prev (the (simple-array (unsigned-byte 8) (*)) (ve-prev-y enc)))
         (probes (%pick-probes enc y dirty))
         (ms (ve-motion enc))
         (limit (min +mv-max-pel+ (max 32 (floor h 2)))))
    (when (< (length probes) 4) (return-from %estimate-global-mv nil))
    ;; last frame's vector first: a scroll or a drag lasts many frames, and confirming it costs
    ;; one pass over the probes instead of a search
    (let ((prior (and ms (ms-last-mv ms))))
      (when prior
        (let ((hit (%vote y prev w h probes (list prior))))
          (when hit (return-from %estimate-global-mv hit)))))
    ;; one axis first (a scroll), then both (a drag).  Scanning is candidate GENERATION and uses
    ;; half the probes; the vote that decides between candidates uses all of them.
    (let ((few (subseq probes 0 (min 8 (length probes)))))
      (or (%vote y prev w h probes (%scan-axis y prev w h few :y limit))
          (%vote y prev w h probes (%scan-axis y prev w h few :x limit))
          (%vote y prev w h probes (%scan-grid y prev w h few (min limit 96) 4))))))

;;; ---- residual coding -------------------------------------------------------

(defun %code-inter-plane (tw probs src sw sh pred cur rw ox oy size dcq acq above left ai0 li0
                          blk co q dq rec)
  "Residual-code one plane region against PRED (a SIZE x SIZE prediction buffer), writing the
reconstruction into CUR.  Returns T if anything was coded."
  (declare (type (simple-array (unsigned-byte 8) (*)) src pred cur)
           (type (simple-array (signed-byte 32) (16)) blk co q dq rec)
           (type fixnum sw sh rw ox oy size ai0 li0)
           (type (integer 1 4096) dcq acq)
           (optimize (speed 3) (safety 0)))
  (let ((any nil) (n (floor size 4)))
    (dotimes (b (* n n))
      (let* ((lx (* 4 (mod b n))) (ly (* 4 (floor b n)))
             (bx (+ ox lx)) (by (+ oy ly)))
        (dotimes (i 16)
          (let* ((px (min (1- sw) (+ bx (mod i 4)))) (py (min (1- sh) (+ by (floor i 4)))))
            (setf (aref blk i) (- (aref src (+ (* py sw) px))
                                  (aref pred (+ (* (+ ly (floor i 4)) size) lx (mod i 4)))))))
        (fdct4x4 blk co)
        (zigzag-quantize co q dcq acq)
        (let* ((ai (+ ai0 (mod b n))) (li (+ li0 (floor b n)))
               (ctx (+ (aref above ai) (aref left li)))
               (nz (write-block tw probs +blk-uv+ q 0 ctx)))
          (when nz (setf any t))
          (setf (aref above ai) (if nz 1 0) (aref left li) (if nz 1 0)))
        (dotimes (i 16) (setf (aref dq (aref +zigzag+ i)) (* (aref q i) (if (zerop i) dcq acq))))
        (idct4x4 dq rec)
        (dotimes (i 16)
          (let ((px (+ bx (mod i 4))) (py (+ by (floor i 4))))
            (setf (aref cur (+ (* py rw) px))
                  (max 0 (min 255 (+ (aref pred (+ (* (+ ly (floor i 4)) size) lx (mod i 4)))
                                     (aref rec i)))))))))
    any))

(defun encode-inter-frame (enc y u v &key (qi 12) dirty force (clean-qi 12) max-mbs motion)
  "Encode one inter frame of Y/U/V against the encoder's reference, updating the reference in
place.  Unchanged macroblocks are skipped outright.  DIRTY, when given, is a per-macroblock bit
vector of what the capture reported as updated — macroblocks it clears are skipped without
touching a pixel, which is far cheaper than rediscovering that by comparison.

MOTION is how far the picture MOVED since the last frame, in whole pixels, as (dx . dy) — the
CopyRect fact.  NIL means work it out (see *MOTION-SEARCH*), :NONE means there is none.  Every
macroblock then chooses between that vector and ZEROMV by which predicts it better, so a scroll
codes as a mode bit per translated macroblock plus coefficients for the strip that is new.

FORCE codes every macroblock DIRTY selects even if its pixels are unchanged: that is the idle
refinement pass, which re-codes at a low qi whatever was last coded coarsely (the residual against
the existing reference sharpens the decoder's copy).  Macroblocks coded above CLEAN-QI are
recorded in the encoder's stale set; those at or below it are cleared.

MAX-MBS bounds how many macroblocks this frame may code.  Beyond it, macroblocks that wanted
coding are left skipped and reported in the returned DEFERRED bit vector so the caller can carry
them into the next frame.  That bounds FRAME SIZE, which is what actually bounds latency: one
143 KB frame is ~570 ms in flight on a 250 KB/s link, and no average-rate control can see that.
A big change then arrives as a couple of progressive frames instead of one long stall.
Returns (values bytes coded-mb-count deferred motion-vector)."
  (declare (type (simple-array (unsigned-byte 8) (*)) y u v)
           (optimize (speed 3) (safety 1)))
  (let* ((mc (ve-mb-cols enc)) (mr (ve-mb-rows enc))
         (w (ve-width enc)) (h (ve-height enc))
         (sw (ceiling w 2)) (sh (ceiling h 2))
         (pw (ve-pw enc)) (ph (ve-ph enc)) (cw (ve-cw enc)) (chh (ve-chh enc))
         (ry (the (simple-array (unsigned-byte 8) (*)) (ve-ref-y enc)))
         (ru (the (simple-array (unsigned-byte 8) (*)) (ve-ref-u enc)))
         (rv (the (simple-array (unsigned-byte 8) (*)) (ve-ref-v enc)))
         (bw (make-bwriter)) (tw (make-bwriter)) (probs +default-coef-probs+)
         (dcq (dc-quant qi)) (acq (ac-quant qi))
         (y2dcq (y2-dc-quant qi)) (y2acq (y2-ac-quant qi))
         (uvdcq (min 132 (dc-quant qi))) (uvacq (ac-quant qi))
         (ctxs (make-contexts mc)) (coded 0) (deferred nil)
         (blk (mkblk)) (co (mkblk)) (y2in (mkblk)) (y2co (mkblk)) (y2q (mkblk))
         (dq (mkblk)) (rec (mkblk)) (y2dq (mkblk)) (y2r (mkblk))
         ;; scratch reused for every macroblock: this loop used to cons ~30 short arrays per
         ;; macroblock, which at 4000 macroblocks is ~120k allocations (and the GC churn) a frame
         (cblk (mkblk)) (cco (mkblk)) (cq (mkblk)) (cdq (mkblk)) (crec (mkblk))
         (acs (let ((a (make-array 16))) (dotimes (i 16 a) (setf (aref a i) (mkblk)))))
         (dcs (make-array 16 :element-type '(signed-byte 32)))
         (py (make-array 256 :element-type '(unsigned-byte 8)))
         (pu (make-array 64 :element-type '(unsigned-byte 8)))
         (pv (make-array 64 :element-type '(unsigned-byte 8)))
         (cbuf (make-array 64 :element-type '(unsigned-byte 8)))
         (ftmp (make-array 104 :element-type '(signed-byte 32)))
         (nearv (make-array 6 :element-type 'fixnum :initial-element 0))
         (mvprobs (make-array 4 :element-type '(unsigned-byte 8) :initial-element 128))
         ;; ---- the frame's global motion vector ----
         (gmv (when (and *motion-vectors* (ve-have-ref enc) (not (eq motion :none)))
                (cond (motion (cons (- (car motion)) (- (cdr motion))))  ; moved by -> reference offset
                      ((and *motion-search*
                            (or (null dirty)
                                (>= (count 1 (the simple-bit-vector dirty))
                                    (* *motion-search-min-dirty* mc mr))))
                       (%motion-state enc)
                       (%estimate-global-mv enc y dirty)))))
         (gmvx (if gmv (car gmv) 0)) (gmvy (if gmv (cdr gmv) 0))
         (use-mv (and gmv (or (/= 0 gmvx) (/= 0 gmvy))
                      (<= (abs gmvx) +mv-max-pel+) (<= (abs gmvy) +mv-max-pel+)))
         (ms (if use-mv (%motion-state enc) (ve-motion enc)))
         ;; motion compensation cannot reconstruct in place: prediction reads the previous
         ;; reconstruction, so this frame's goes into the other buffer and they swap below
         (cy (if use-mv (ms-cur-y ms) ry))
         (cu (if use-mv (ms-cur-u ms) ru))
         (cv (if use-mv (ms-cur-v ms) rv))
         (mvr (when use-mv (ms-mv-row ms))) (mvc (when use-mv (ms-mv-col ms)))
         (was-stale (when use-mv (ms-was-stale ms))))
    (declare (type fixnum gmvx gmvy coded)
             (type (simple-array (unsigned-byte 8) (*)) cy cu cv)
             (type (simple-array (unsigned-byte 8) (256)) py)
             (type (simple-array (unsigned-byte 8) (64)) pu pv cbuf))
    (when use-mv
      (replace (the (simple-array (unsigned-byte 8) (*)) cy) ry)
      (replace (the (simple-array (unsigned-byte 8) (*)) cu) ru)
      (replace (the (simple-array (unsigned-byte 8) (*)) cv) rv)
      (fill (the (simple-array (signed-byte 16) (*)) mvr) 0)
      (fill (the (simple-array (signed-byte 16) (*)) mvc) 0)
      (if (ve-stale-mbs enc)
          (replace (the simple-bit-vector was-stale) (the simple-bit-vector (ve-stale-mbs enc)))
          (fill (the simple-bit-vector was-stale) 0)))
    (when ms (setf (ms-last-mv ms) (and use-mv gmv)))
    ;; ---- frame header (inter variant, RFC 6386 s9) ----
    (bwrite-literal bw 0 1)                      ; segmentation_enabled
    (bwrite-literal bw 0 1)                      ; filter_type
    (bwrite-literal bw 0 6)                      ; loop_filter_level (0 = exact output)
    (bwrite-literal bw 0 3)                      ; sharpness_level
    (bwrite-literal bw 0 1)                      ; loop_filter_adj_enable
    (bwrite-literal bw 0 2)                      ; one token partition
    (bwrite-literal bw qi 7)                     ; y_ac_qi
    (dotimes (i 5) (bwrite-literal bw 0 1))      ; no quantizer deltas
    ;; inter-only reference-buffer flags: refresh everything from this frame, no sign bias
    (bwrite-literal bw 1 1)                      ; refresh_golden_frame
    (bwrite-literal bw 1 1)                      ; refresh_alternate_frame
    (bwrite-literal bw 0 1)                      ; sign_bias_golden
    (bwrite-literal bw 0 1)                      ; sign_bias_alternate
    (bwrite-literal bw 0 1)                      ; refresh_entropy_probs
    (bwrite-literal bw 1 1)                      ; refresh_last
    (loop for p across +coeff-update-probs+ do (bwrite-bit bw p 0))
    (bwrite-literal bw 1 1)                      ; mb_no_skip_coeff = 1 (per-MB skip flags)
    (bwrite-literal bw +prob-skip+ 8)
    (bwrite-literal bw +prob-intra+ 8)
    (bwrite-literal bw +prob-last+ 8)
    (bwrite-literal bw +prob-gf+ 8)
    (bwrite-literal bw 0 1)                      ; no intra_16x16 prob update
    (bwrite-literal bw 0 1)                      ; no intra_chroma prob update
    ;; "no MV probability update" — each flag is coded with its OWN probability from
    ;; vp8_mv_update_probs, NOT at 128.  Writing plain bits here desynchronises the decoder
    ;; immediately (the frame decodes to black), which is exactly what it did.
    (dotimes (comp 2)
      (dotimes (j 19) (bwrite-bit bw (aref +mv-update-probs+ comp j) 0)))
    ;; ---- macroblocks ----
    (dotimes (mby mr)
      (fill (ec-left-y ctxs) 0) (fill (ec-left-u ctxs) 0) (fill (ec-left-v ctxs) 0)
      (setf (ec-left-y2 ctxs) 0)
      (dotimes (mbx mc)
        (let* ((ox (* mbx 16)) (oy (* mby 16))
               (cx (* mbx 8)) (cy2 (* mby 8))
               (mbi (+ (* mby mc) mbx))
               (forced (and force dirty (= 1 (aref (the simple-bit-vector dirty) mbi))))
               (clean (and (ve-have-ref enc)
                           (not forced)
                           (or (and dirty (zerop (aref (the simple-bit-vector dirty) mbi)))
                               (and (%mb-unchanged-p y (ve-prev-y enc) w h ox oy 16)
                                    (%mb-unchanged-p u (ve-prev-u enc) sw sh cx cy2 8)
                                    (%mb-unchanged-p v (ve-prev-v enc) sw sh cx cy2 8)))))
               ;; can this macroblock use the frame's vector at all?
               (mvable (and use-mv (not clean)
                            (%mv-fits-p mbx mby gmvx gmvy pw ph cw chh)))
               ;; ... and does it simply carry translated pixels, needing no coefficients?  This
               ;; overrides FORCE: a macroblock that only moved has nothing to refine in place,
               ;; and re-coding it is exactly the full-screen cost the vector exists to avoid.
               (moved (and mvable
                           (%mb-moved-p y (ve-prev-y enc) w h ox oy gmvx gmvy 16)
                           (%mb-moved-chroma-p u (ve-prev-u enc) sw sh cx cy2 gmvx gmvy ftmp cbuf)
                           (%mb-moved-chroma-p v (ve-prev-v enc) sw sh cx cy2 gmvx gmvy ftmp cbuf)))
               (skip0 (or clean moved))
               ;; out of budget: leave it for the next frame rather than growing this one
               (over (and max-mbs (not skip0) (>= coded (the fixnum max-mbs))))
               (skip (or skip0 over))
               (mvx 0) (mvy 0))
          (declare (type fixnum mvx mvy))
          (when over
            (unless deferred (setf deferred (make-array (* mc mr) :element-type 'bit :initial-element 0)))
            (setf (aref (the simple-bit-vector deferred) mbi) 1))
          ;; choose the prediction: the global vector only if it beats the co-located one
          (cond
            (moved (setf mvx gmvx mvy gmvy))
            ((and mvable (not skip))
             (let ((z (%block-sad y w h ox oy ry pw ox oy 16 2))
                   (g (%block-sad y w h ox oy ry pw (+ ox gmvx) (+ oy gmvy) 16 2)))
               (when (< g (- z 96)) (setf mvx gmvx mvy gmvy)))))
          (let ((moving (or (/= 0 mvx) (/= 0 mvy))))
            (when use-mv
              (setf (aref (the (simple-array (signed-byte 16) (*)) mvr) mbi) (* 8 mvy)
                    (aref (the (simple-array (signed-byte 16) (*)) mvc) mbi) (* 8 mvx)))
            ;; per-MB record: skip flag, then inter / LAST / mode
            (bwrite-bit bw +prob-skip+ (if skip 1 0))
            (bwrite-bit bw +prob-intra+ 1)                     ; is_inter_mb
            (bwrite-bit bw +prob-last+ 0)                      ; ref = LAST_FRAME
            (cond
              ((not use-mv)                                    ; pure-ZEROMV encoder
               (bwrite-bit bw (%mv-ref-p0 mbx mby) 0))
              (t
               (%find-near-mvs ms mbx mby mc mr nearv mvprobs)
               (let ((r (* 8 mvy)) (c (* 8 mvx)))
                 (cond
                   ((not moving) (%write-mv-ref bw mvprobs :zero))
                   ((and (= r (aref nearv 0)) (= c (aref nearv 1)))
                    (%write-mv-ref bw mvprobs :nearest))
                   ((and (= r (aref nearv 2)) (= c (aref nearv 3)))
                    (%write-mv-ref bw mvprobs :near))
                   (t (%write-mv-ref bw mvprobs :new)
                      ;; NEWMV codes a QUARTER-pel delta from the neighbourhood's best vector
                      (%write-mv-component bw 0 (ash (- r (aref nearv 4)) -1))
                      (%write-mv-component bw 1 (ash (- c (aref nearv 5)) -1)))))))
            (cond
              (skip
               ;; a skipped macroblock carries no tokens and RESETS its entropy contexts
               (dotimes (i 4) (setf (aref (ec-above-y ctxs) (+ (* 4 mbx) i)) 0
                                    (aref (ec-left-y ctxs) i) 0))
               (dotimes (i 2) (setf (aref (ec-above-u ctxs) (+ (* 2 mbx) i)) 0
                                    (aref (ec-left-u ctxs) i) 0
                                    (aref (ec-above-v ctxs) (+ (* 2 mbx) i)) 0
                                    (aref (ec-left-v ctxs) i) 0))
               (setf (aref (ec-above-y2 ctxs) mbx) 0 (ec-left-y2 ctxs) 0)
               ;; a skip with a vector still MOVES pixels: reconstruct the displacement, and
               ;; inherit the staleness of wherever they came from so the idle pass still knows
               (when moving
                 (%copy-pred ry pw (+ ox mvx) (+ oy mvy) py 16)
                 (dotimes (r 16)
                   (replace (the (simple-array (unsigned-byte 8) (*)) cy) py
                            :start1 (+ (* (+ oy r) pw) ox) :end1 (+ (* (+ oy r) pw) ox 16)
                            :start2 (* r 16)))
                 (%predict-chroma ru cw mbx mby mvx mvy pu ftmp)
                 (%predict-chroma rv cw mbx mby mvx mvy pv ftmp)
                 (dotimes (r 8)
                   (replace (the (simple-array (unsigned-byte 8) (*)) cu) pu
                            :start1 (+ (* (+ cy2 r) cw) cx) :end1 (+ (* (+ cy2 r) cw) cx 8)
                            :start2 (* r 8))
                   (replace (the (simple-array (unsigned-byte 8) (*)) cv) pv
                            :start1 (+ (* (+ cy2 r) cw) cx) :end1 (+ (* (+ cy2 r) cw) cx 8)
                            :start2 (* r 8)))
                 (let ((st (ve-stale-mbs enc)))
                   (when st
                     (setf (aref st mbi)
                           (let ((s 0))
                             (loop for (dx dy) in '((0 0) (15 0) (0 15) (15 15)) do
                               (let ((bx (floor (+ ox mvx dx) 16)) (by (floor (+ oy mvy dy) 16)))
                                 (when (and (>= bx 0) (< bx mc) (>= by 0) (< by mr))
                                   (setf s (logior s (aref (the simple-bit-vector was-stale)
                                                           (+ (* by mc) bx)))))))
                             s))))))
              (t
               (incf coded)
               (let ((st (ve-stale-mbs enc)))
                 (when st (setf (aref st mbi) (if (> qi clean-qi) 1 0))))
               ;; build the prediction for this macroblock, then code the residual against it
               (%copy-pred ry pw (+ ox mvx) (+ oy mvy) py 16)
               (if moving
                   (progn (%predict-chroma ru cw mbx mby mvx mvy pu ftmp)
                          (%predict-chroma rv cw mbx mby mvx mvy pv ftmp))
                   (progn (%copy-pred ru cw cx cy2 pu 8) (%copy-pred rv cw cx cy2 pv 8)))
               ;; luma: residual against the prediction, Y2 over the sixteen DCs
               (progn
                 (dotimes (b 16)
                   (let ((lx (* 4 (mod b 4))) (ly (* 4 (floor b 4))))
                     (dotimes (i 16)
                       (let* ((px (min (1- w) (+ ox lx (mod i 4))))
                              (py2 (min (1- h) (+ oy ly (floor i 4)))))
                         (setf (aref blk i) (- (aref y (+ (* py2 w) px))
                                               (aref py (+ (* (+ ly (floor i 4)) 16)
                                                           lx (mod i 4)))))))
                     (fdct4x4 blk co)
                     (setf (aref dcs b) (aref co 0))
                     (zigzag-quantize co (aref acs b) dcq acq :first 1)))
                 (dotimes (i 16) (setf (aref y2in i) (aref dcs i)))
                 (fwht4x4 y2in y2co)
                 (zigzag-quantize y2co y2q y2dcq y2acq)
                 (let* ((ctx (+ (aref (ec-above-y2 ctxs) mbx) (ec-left-y2 ctxs)))
                        (nz (write-block tw probs +blk-y2+ y2q 0 ctx)))
                   (setf (aref (ec-above-y2 ctxs) mbx) (if nz 1 0) (ec-left-y2 ctxs) (if nz 1 0)))
                 (dotimes (b 16)
                   (let* ((bc (mod b 4)) (br (floor b 4))
                          (ctx (+ (aref (ec-above-y ctxs) (+ (* 4 mbx) bc)) (aref (ec-left-y ctxs) br)))
                          (nz (write-block tw probs +blk-y-after-y2+ (aref acs b) 1 ctx)))
                     (setf (aref (ec-above-y ctxs) (+ (* 4 mbx) bc)) (if nz 1 0)
                           (aref (ec-left-y ctxs) br) (if nz 1 0))))
                 ;; reconstruct luma into this frame's buffer
                 (dotimes (i 16) (setf (aref y2dq i) (* (aref y2q i) (if (zerop i) y2dcq y2acq))))
                 ;; scatter by zigzag index rather than searching for each raster index (was O(16) per
                 ;; coefficient, i.e. 256 searches per macroblock)
                 (dotimes (j 16) (setf (aref y2in (aref +zigzag+ j)) (aref y2dq j)))
                 (iwht4x4 y2in y2r)
                 (dotimes (b 16)
                   (let ((q (aref acs b)) (lx (* 4 (mod b 4))) (ly (* 4 (floor b 4))))
                     (dotimes (i 16) (setf (aref dq (aref +zigzag+ i)) (* (aref q i) (if (zerop i) dcq acq))))
                     (setf (aref dq 0) (aref y2r b))
                     (idct4x4 dq rec)
                     (dotimes (i 16)
                       (let* ((px (+ ox lx (mod i 4))) (py2 (+ oy ly (floor i 4)))
                              (o (+ (* py2 pw) px)))
                         (setf (aref cy o)
                               (max 0 (min 255 (+ (aref py (+ (* (+ ly (floor i 4)) 16)
                                                              lx (mod i 4)))
                                                  (aref rec i))))))))))
               ;; chroma
               (%code-inter-plane tw probs u sw sh pu cu cw cx cy2 8 uvdcq uvacq
                                  (ec-above-u ctxs) (ec-left-u ctxs) (* 2 mbx) 0
                                  cblk cco cq cdq crec)
               (%code-inter-plane tw probs v sw sh pv cv cw cx cy2 8 uvdcq uvacq
                                  (ec-above-v ctxs) (ec-left-v ctxs) (* 2 mbx) 0
                                  cblk cco cq cdq crec)))))))
    ;; this frame's reconstruction becomes the reference; its source, the change baseline
    (when use-mv
      (setf (ve-ref-y enc) cy (ms-cur-y ms) ry
            (ve-ref-u enc) cu (ms-cur-u ms) ru
            (ve-ref-v enc) cv (ms-cur-v ms) rv))
    (replace (ve-prev-y enc) y) (replace (ve-prev-u enc) u) (replace (ve-prev-v enc) v)
    (setf (ve-have-ref enc) t)
    ;; ---- assemble: 3-byte tag only (no start code / dimensions on an inter frame) ----
    (let* ((p1 (bwrite-finish bw)) (p2 (bwrite-finish tw)) (frame (u8buf))
           (tag (logior 1 (ash 0 1) (ash 1 4) (ash (length p1) 5))))   ; key_frame=1 means NOT key
      (vector-push-extend (logand tag #xff) frame)
      (vector-push-extend (logand (ash tag -8) #xff) frame)
      (vector-push-extend (logand (ash tag -16) #xff) frame)
      (loop for b across p1 do (vector-push-extend b frame))
      (loop for b across p2 do (vector-push-extend b frame))
      (values frame coded deferred (when use-mv (cons (- gmvx) (- gmvy)))))))
