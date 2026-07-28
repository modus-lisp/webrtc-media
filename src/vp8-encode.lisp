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
;;;; STATUS: WIP — structurally complete but the bitstream still desyncs.  ffmpeg decodes the frame
;;;; and recovers the FIRST block's DC (output tracks the quantizer), but everything after it comes
;;;; out uniform, i.e. the decoder stops recovering our coefficients almost immediately.  Ruled out:
;;;; the sign bit (spec polarity and the inverse both give the same result), so the divergence is in
;;;; band/context selection or block ordering rather than the coefficient values.
;;;;
;;;; NEXT: write a VP8 bitstream PARSER on top of the (already bit-exact) bool decoder and read back
;;;; our own frame, comparing token-by-token against what we intended to write.  That bisects the
;;;; divergence precisely instead of guessing — the same trick that made the bool coder and the
;;;; uncompressed header verifiable.

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
initial entropy context CTX.  Returns T if the block had any non-zero coefficient."
  (let ((last -1))
    (loop for i from first-coef below 16 when (/= 0 (aref coeffs i)) do (setf last i))
    (if (< last 0)
        (progn (write-token bw probs type (aref +coef-bands+ first-coef) ctx +tok-eob+) nil)
        (let ((c ctx))
          (loop for i from first-coef to last
                for v = (aref coeffs i)
                do (write-coef bw probs type (aref +coef-bands+ i) c v)
                   (setf c (cond ((zerop v) 0) ((= 1 (abs v)) 1) (t 2))))
          (when (< last 15)                     ; EOB unless the block ran to the end
            (write-token bw probs type (aref +coef-bands+ (1+ last)) c +tok-eob+))
          t))))

(defun zigzag-quantize (coeffs out dcq acq &key (first 0))
  "Quantize COEFFS (raster 4x4 order) into OUT in ZIGZAG order.  Positions below FIRST are zeroed."
  (dotimes (i 16 out)
    (let* ((src (aref +zigzag+ i))
           (q (if (zerop i) dcq acq))
           (v (if (< i first) 0 (quant1 (aref coeffs src) q))))
      (setf (aref out i) v))))

;;; ---- prediction + reconstruction -------------------------------------------
(defun dc-pred (plane stride x y size)
  "VP8 DC_PRED for a SIZE x SIZE block at (X,Y) in PLANE: average of the reconstructed row above
and column to the left; 128 when neither exists."
  (let ((sum 0) (n 0))
    (when (plusp y) (dotimes (i size) (incf sum (aref plane (+ (* (1- y) stride) x i))) (incf n)))
    (when (plusp x) (dotimes (i size) (incf sum (aref plane (+ (* (+ y i) stride) (1- x)))) (incf n)))
    (if (zerop n) 128 (floor (+ sum (floor n 2)) n))))

(defun encode-gray-frame (src width height &key (qi 30))
  "Encode an 8-bit grayscale image SRC (WIDTH*HEIGHT bytes, row-major) as a VP8 keyframe with real
DC+AC coefficients.  Chroma is left at neutral (128) so the picture is greyscale.  Returns bytes."
  (let* ((mb-cols (ceiling width 16)) (mb-rows (ceiling height 16))
         (pw (* mb-cols 16)) (ph (* mb-rows 16))
         (bw (make-bwriter)) (tw (make-bwriter))
         (probs +default-coef-probs+)
         (dcq (dc-quant qi)) (acq (ac-quant qi)) (y2dcq (* 2 (dc-quant qi))) (y2acq (round (* 155 (ac-quant qi)) 100))
         (recon (make-array (* pw ph) :element-type '(unsigned-byte 8) :initial-element 128))
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
            ;; chroma: neutral, so every block is empty — but still coded (EOB) with context
            (dotimes (b 4)
              (let* ((ctx (+ (aref (ec-above-u ctxs) (+ (* 2 mbx) (mod b 2))) (aref (ec-left-u ctxs) (floor b 2)))))
                (write-token tw probs +blk-uv+ 0 ctx +tok-eob+)
                (setf (aref (ec-above-u ctxs) (+ (* 2 mbx) (mod b 2))) 0 (aref (ec-left-u ctxs) (floor b 2)) 0)))
            (dotimes (b 4)
              (let* ((ctx (+ (aref (ec-above-v ctxs) (+ (* 2 mbx) (mod b 2))) (aref (ec-left-v ctxs) (floor b 2)))))
                (write-token tw probs +blk-uv+ 0 ctx +tok-eob+)
                (setf (aref (ec-above-v ctxs) (+ (* 2 mbx) (mod b 2))) 0 (aref (ec-left-v ctxs) (floor b 2)) 0)))
            ;; ---- reconstruct this MB exactly as the decoder will (feeds later predictions) ----
            (dotimes (i 16) (setf (aref y2dq i) (* (aref y2q i) (if (zerop i) y2dcq y2acq))))
            (let ((y2r (mkblk)))
              (dotimes (i 16) (setf (aref y2in i) (aref y2dq (position i +zigzag+))))
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
      frame)))
