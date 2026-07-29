;;;; vp8-inter.lisp — inter (P) frames: the thing that makes a desktop stream cheap.
;;;;
;;;; A keyframe costs ~76 KB and ~300 ms for a 1280x800 screen, so keyframe-only streaming runs at
;;;; well under 1 fps.  But a desktop is mostly static: with an inter frame every macroblock that
;;;; has not changed is a single skip bit against the previous frame, and only the parts that
;;;; actually moved carry coefficients.
;;;;
;;;; We use exactly one inter mode: ZEROMV against LAST_FRAME.  That means the prediction for a
;;;; macroblock is the co-located pixels of the previous reconstruction — per pixel, which is a far
;;;; better predictor than intra DC — and it needs no motion search, no motion vectors on the wire,
;;;; and no SPLITMV/B_PRED machinery.  (Real motion estimation would help video; for a desktop,
;;;; where things appear and disappear rather than pan, it buys little for a lot of complexity.)
;;;;
;;;; The MV-reference probabilities depend on how many neighbouring macroblocks are inter with a
;;;; non-zero MV.  Since every macroblock we emit is inter with a ZERO MV, those counts collapse to
;;;; a function of position alone (libvpx vp8_find_near_mvs): the borders around the frame count as
;;;; intra and contribute nothing, so cnt[0] is 0 at the top-left, 2 along the top/left edges, and
;;;; 5 in the interior — and cnt[1..3] stay 0.

(in-package #:webrtc-media.vp8)

(defparameter +mode-contexts+                  ; libvpx vp8_mode_contexts[6][4]
  #2A((7 1 1 143) (14 18 14 107) (135 64 57 68) (60 56 128 65)
      (159 134 128 34) (234 188 128 28)))

(defparameter +mv-update-probs+                ; libvpx vp8_mv_update_probs[2][19]
  #2A((237 246 253 253 254 254 254 254 254 254 254 254 254 254 250 250 252 254 254)
      (231 243 245 253 254 254 254 254 254 254 254 254 254 254 251 251 254 254 254)))

(defconstant +prob-intra+ 8)                   ; P(intra); we always send inter, so keep it low
(defconstant +prob-last+ 254)                  ; P(LAST); we always reference LAST
(defconstant +prob-gf+ 128)
(defconstant +prob-skip+ 128)

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
  (have-ref nil) (frames 0))

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

(defun %mv-ref-p0 (mbx mby)
  "Probability for the ZEROMV branch of vp8_mv_ref_tree at this macroblock (see file header)."
  (aref +mode-contexts+ (cond ((and (zerop mbx) (zerop mby)) 0)
                              ((or (zerop mbx) (zerop mby)) 2)
                              (t 5))
        0))

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

(defun %code-inter-plane (tw probs src sw sh ref rw ox oy size dcq acq above left ai0 li0 blocks
                          blk co q dq rec)
  "Residual-code one plane region against the reference (ZEROMV: prediction is the co-located
pixels).  Returns T if anything was coded.  BLOCKS is 4 (chroma 8x8) or ignored for luma."
  (declare (ignore blocks)
           (type (simple-array (unsigned-byte 8) (*)) src ref)
           (type (simple-array (signed-byte 32) (16)) blk co q dq rec)
           (type fixnum sw sh rw ox oy size ai0 li0)
           (type (integer 1 4096) dcq acq)
           (optimize (speed 3) (safety 0)))
  (let ((any nil) (n (floor size 4)))
    (dotimes (b (* n n))
      (let* ((bx (+ ox (* 4 (mod b n)))) (by (+ oy (* 4 (floor b n)))))
        (dotimes (i 16)
          (let* ((px (min (1- sw) (+ bx (mod i 4)))) (py (min (1- sh) (+ by (floor i 4)))))
            (setf (aref blk i) (- (aref src (+ (* py sw) px))
                                  (aref ref (+ (* (+ by (floor i 4)) rw) (+ bx (mod i 4))))))))
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
            (setf (aref ref (+ (* py rw) px))
                  (max 0 (min 255 (+ (aref ref (+ (* py rw) px)) (aref rec i)))))))))
    any))

(defun encode-inter-frame (enc y u v &key (qi 12) dirty force (clean-qi 12) max-mbs)
  "Encode one inter frame of Y/U/V against the encoder's reference, updating the reference in
place.  Every macroblock is ZEROMV/LAST; unchanged ones are skipped outright.  DIRTY, when given,
is a per-macroblock bit vector of what the capture reported as updated — macroblocks it clears are
skipped without touching a pixel, which is far cheaper than rediscovering that by comparison.

FORCE codes every macroblock DIRTY selects even if its pixels are unchanged: that is the idle
refinement pass, which re-codes at a low qi whatever was last coded coarsely (the residual against
the existing reference sharpens the decoder's copy).  Macroblocks coded above CLEAN-QI are
recorded in the encoder's stale set; those at or below it are cleared.

MAX-MBS bounds how many macroblocks this frame may code.  Beyond it, macroblocks that wanted
coding are left skipped and reported in the returned DEFERRED bit vector so the caller can carry
them into the next frame.  That bounds FRAME SIZE, which is what actually bounds latency: one
143 KB frame is ~570 ms in flight on a 250 KB/s link, and no average-rate control can see that.
A big change then arrives as a couple of progressive frames instead of one long stall.
Returns (values bytes coded-mb-count deferred)."
  (declare (type (simple-array (unsigned-byte 8) (*)) y u v)
           (optimize (speed 3) (safety 1)))
  (let* ((mc (ve-mb-cols enc)) (mr (ve-mb-rows enc))
         (w (ve-width enc)) (h (ve-height enc))
         (sw (ceiling w 2)) (sh (ceiling h 2))
         (pw (ve-pw enc)) (cw (ve-cw enc))
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
         (acs (let ((a (make-array 16))) (dotimes (i 16 a) (setf (aref a i) (mkblk))))))
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
               (cx (* mbx 8)) (cy (* mby 8))
               (mbi (+ (* mby mc) mbx))
               (forced (and force dirty (= 1 (aref (the simple-bit-vector dirty) mbi))))
               (skip0 (and (ve-have-ref enc)
                           (not forced)
                           (or (and dirty (zerop (aref (the simple-bit-vector dirty) mbi)))
                               (and (%mb-unchanged-p y (ve-prev-y enc) w h ox oy 16)
                                    (%mb-unchanged-p u (ve-prev-u enc) sw sh cx cy 8)
                                    (%mb-unchanged-p v (ve-prev-v enc) sw sh cx cy 8)))))
               ;; out of budget: leave it for the next frame rather than growing this one
               (over (and max-mbs (not skip0) (>= coded (the fixnum max-mbs))))
               (skip (or skip0 over)))
          (when over
            (unless deferred (setf deferred (make-array (* mc mr) :element-type 'bit :initial-element 0)))
            (setf (aref (the simple-bit-vector deferred) mbi) 1))
          ;; per-MB record: skip flag, then inter / LAST / ZEROMV
          (bwrite-bit bw +prob-skip+ (if skip 1 0))
          (bwrite-bit bw +prob-intra+ 1)                       ; is_inter_mb
          (bwrite-bit bw +prob-last+ 0)                        ; ref = LAST_FRAME
          (bwrite-bit bw (%mv-ref-p0 mbx mby) 0)               ; ZEROMV
          (cond
            (skip
             ;; a skipped macroblock carries no tokens and RESETS its entropy contexts
             (dotimes (i 4) (setf (aref (ec-above-y ctxs) (+ (* 4 mbx) i)) 0
                                  (aref (ec-left-y ctxs) i) 0))
             (dotimes (i 2) (setf (aref (ec-above-u ctxs) (+ (* 2 mbx) i)) 0
                                  (aref (ec-left-u ctxs) i) 0
                                  (aref (ec-above-v ctxs) (+ (* 2 mbx) i)) 0
                                  (aref (ec-left-v ctxs) i) 0))
             (setf (aref (ec-above-y2 ctxs) mbx) 0 (ec-left-y2 ctxs) 0))
            (t
             (incf coded)
             (let ((st (ve-stale-mbs enc)))
               (when st (setf (aref st mbi) (if (> qi clean-qi) 1 0))))
             ;; luma: residual against the co-located reference pixels, Y2 over the sixteen DCs
             (let ((dcs (make-array 16 :element-type '(signed-byte 32))))
               (dotimes (b 16)
                 (let ((bx (+ ox (* 4 (mod b 4)))) (by (+ oy (* 4 (floor b 4)))))
                   (dotimes (i 16)
                     (let* ((px (min (1- w) (+ bx (mod i 4)))) (py (min (1- h) (+ by (floor i 4)))))
                       (setf (aref blk i) (- (aref y (+ (* py w) px))
                                             (aref ry (+ (* (+ by (floor i 4)) pw) (+ bx (mod i 4))))))))
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
               ;; reconstruct luma into the reference
               (progn
                 (dotimes (i 16) (setf (aref y2dq i) (* (aref y2q i) (if (zerop i) y2dcq y2acq))))
                 ;; scatter by zigzag index rather than searching for each raster index (was O(16) per
                 ;; coefficient, i.e. 256 searches per macroblock)
                 (dotimes (j 16) (setf (aref y2in (aref +zigzag+ j)) (aref y2dq j)))
                 (iwht4x4 y2in y2r)
                 (dotimes (b 16)
                   (let ((q (aref acs b)) (bx (+ ox (* 4 (mod b 4)))) (by (+ oy (* 4 (floor b 4)))))
                     (dotimes (i 16) (setf (aref dq (aref +zigzag+ i)) (* (aref q i) (if (zerop i) dcq acq))))
                     (setf (aref dq 0) (aref y2r b))
                     (idct4x4 dq rec)
                     (dotimes (i 16)
                       (let* ((px (+ bx (mod i 4))) (py (+ by (floor i 4))) (o (+ (* py pw) px)))
                         (setf (aref ry o) (max 0 (min 255 (+ (aref ry o) (aref rec i)))))))))))
             ;; chroma
             (%code-inter-plane tw probs u sw sh ru cw cx cy 8 uvdcq uvacq
                                (ec-above-u ctxs) (ec-left-u ctxs) (* 2 mbx) 0 4
                                cblk cco cq cdq crec)
             (%code-inter-plane tw probs v sw sh rv cw cx cy 8 uvdcq uvacq
                                (ec-above-v ctxs) (ec-left-v ctxs) (* 2 mbx) 0 4
                                cblk cco cq cdq crec))))))
    ;; this frame's source becomes the baseline for deciding what changed next time
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
      (values frame coded deferred))))
