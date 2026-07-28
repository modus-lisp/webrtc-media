;;;; vp8.lisp — VP8 keyframe encoder (RFC 6386 §9,§19.2).
;;;;
;;;; Assembles a decodable VP8 keyframe: the 10-byte uncompressed chunk (frame tag + start code +
;;;; dimensions), the bool-coded first partition (frame header + per-macroblock modes), and the
;;;; DCT-token partition.  Built up + validated against ffmpeg's libvpx decoder one layer at a time.
;;;;
;;;; STATUS: uncompressed header done + byte-verified vs a real ffmpeg keyframe.  The bool-coded
;;;; first partition (token-prob updates, quantizer, per-MB intra modes) and the token partition
;;;; are next — they need VP8's large constant probability tables (RFC 6386 §13).

(in-package #:webrtc-media.vp8)

(defun u8buf () (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun write-uncompressed-header (out width height first-part-size &key (version 0) (show t))
  "Append the 10-byte VP8 keyframe uncompressed data chunk to OUT (adjustable u8 vector):
3-byte little-endian frame tag, 3-byte start code 9d 01 2a, 2-byte width, 2-byte height.
KEY_FRAME is 0 (inverted flag); scaling is 0.  RFC 6386 §9.1."
  (let ((tag (logior 0                                   ; key_frame = 0 (this IS a key frame)
                     (ash (logand version 7) 1)
                     (ash (if show 1 0) 4)
                     (ash first-part-size 5))))
    (vector-push-extend (logand tag #xff) out)
    (vector-push-extend (logand (ash tag -8) #xff) out)
    (vector-push-extend (logand (ash tag -16) #xff) out)
    (dolist (b '(#x9d #x01 #x2a)) (vector-push-extend b out))
    (vector-push-extend (logand width #xff) out)
    (vector-push-extend (logand (ash width -8) #x3f) out)       ; low 14 bits = width, high 2 = h-scale (0)
    (vector-push-extend (logand height #xff) out)
    (vector-push-extend (logand (ash height -8) #x3f) out)      ; low 14 bits = height, high 2 = v-scale (0)
    out))

;;; kf intra-mode coding paths (from vp8_kf_ymode_tree / vp8_kf_uv_mode_tree + their probs).
;;; DC_PRED only for now — the whole point of a flat first frame.
(defun write-ymode-dc (bw)                      ; DC_PRED path: right,left,left through the y tree
  (bwrite-bit bw (aref +kf-ymode-prob+ 0) 1)
  (bwrite-bit bw (aref +kf-ymode-prob+ 1) 0)
  (bwrite-bit bw (aref +kf-ymode-prob+ 2) 0))
(defun write-uvmode-dc (bw) (bwrite-bit bw (aref +kf-uv-mode-prob+ 0) 0))  ; DC_PRED: left

(defun encode-keyframe-gray (width height &key (qi 40) (prob-skip 128))
  "Encode a solid mid-gray (128) VP8 keyframe: every macroblock is 16x16 DC-predicted with no
residual (mb_skip_coeff=1), so the whole frame decodes to the DC-prediction default, 128.  The
simplest decodable VP8 frame — proves the first partition + header end to end.  Returns frame bytes."
  (let* ((mb-cols (ceiling width 16)) (mb-rows (ceiling height 16))
         (bw (make-bwriter)))
    ;; --- frame header (RFC 6386 §9.2-9.11, key frame) ---
    (bwrite-literal bw 0 1)                     ; color_space (YUV)
    (bwrite-literal bw 0 1)                     ; clamping_type
    (bwrite-literal bw 0 1)                     ; segmentation_enabled = 0
    (bwrite-literal bw 0 1)                     ; filter_type
    (bwrite-literal bw 0 6)                     ; loop_filter_level = 0 (no filtering, exact output)
    (bwrite-literal bw 0 3)                     ; sharpness_level
    (bwrite-literal bw 0 1)                     ; loop_filter_adj_enable = 0
    (bwrite-literal bw 0 2)                     ; log2_nbr_of_DCT_partitions = 0 (1 token partition)
    (bwrite-literal bw qi 7)                    ; y_ac_qi
    (dotimes (i 5) (bwrite-literal bw 0 1))     ; y_dc/y2_dc/y2_ac/uv_dc/uv_ac delta: none
    (bwrite-literal bw 0 1)                     ; refresh_entropy_probs = 0
    (loop for p across +coeff-update-probs+ do (bwrite-bit bw p 0))  ; token_prob_update: no updates
    (bwrite-literal bw 1 1)                     ; mb_no_skip_coeff = 1
    (bwrite-literal bw prob-skip 8)             ; prob_skip_false
    ;; --- per-macroblock prediction records ---
    (dotimes (i (* mb-cols mb-rows))
      (bwrite-bit bw prob-skip 1)              ; mb_skip_coeff = 1 (no coefficients)
      (write-ymode-dc bw)
      (write-uvmode-dc bw))
    ;; --- assemble: uncompressed header + first partition + (empty) token partition ---
    (let ((part1 (bwrite-finish bw))
          (part2 (bwrite-finish (make-bwriter)))   ; token partition: nothing to code, just flush
          (frame (u8buf)))
      (write-uncompressed-header frame width height (length part1))
      (loop for b across part1 do (vector-push-extend b frame))
      (loop for b across part2 do (vector-push-extend b frame))
      frame)))

;;; block types (RFC 6386 §13.3): 0 = Y beginning at coef 1 (i.e. Y after Y2), 1 = Y2,
;;; 2 = chroma, 3 = Y with DC (no Y2).  A 16x16-predicted MB uses Y2 (type 1) + type 0 luma.
(defconstant +blk-y-after-y2+ 0) (defconstant +blk-y2+ 1) (defconstant +blk-uv+ 2)

(defun encode-keyframe-solid (width height y u v &key (qi 40))
  "Encode a solid-colour VP8 keyframe of luma Y and chroma U/V (0-255 each).  Every macroblock is
16x16 DC-predicted; the prediction is 128 (no neighbours on a keyframe's first row/col, and DC
prediction propagates), so we code the DIFFERENCE as a DC coefficient: Y via the Y2/WHT path,
chroma directly.  Only DC is non-zero, so this is exactly the token coder's simplest case."
  (let* ((mb-cols (ceiling width 16)) (mb-rows (ceiling height 16))
         (nmb (* mb-cols mb-rows))
         (bw (make-bwriter)) (tw (make-bwriter))
         (dcq (dc-quant qi)) (acq (ac-quant qi))
         (y2dcq (* 2 (dc-quant qi)))            ; Y2 DC uses 2x the luma DC quantizer (§14.1)
         (uvdcq (dc-quant qi)))
    (declare (ignorable acq))
    ;; --- frame header (same as the gray case) ---
    (bwrite-literal bw 0 1) (bwrite-literal bw 0 1) (bwrite-literal bw 0 1)  ; colorspace, clamp, seg
    (bwrite-literal bw 0 1) (bwrite-literal bw 0 6) (bwrite-literal bw 0 3)  ; filter type/level/sharp
    (bwrite-literal bw 0 1)                     ; lf adj
    (bwrite-literal bw 0 2)                     ; 1 token partition
    (bwrite-literal bw qi 7)
    (dotimes (i 5) (bwrite-literal bw 0 1))     ; no quant deltas
    (bwrite-literal bw 0 1)                     ; refresh_entropy_probs = 0
    (loop for p across +coeff-update-probs+ do (bwrite-bit bw p 0))
    (bwrite-literal bw 0 1)                     ; mb_no_skip_coeff = 0 (every MB has coefficients)
    ;; --- per-MB modes (no skip flag, since mb_no_skip_coeff = 0) ---
    (dotimes (i nmb) (write-ymode-dc bw) (write-uvmode-dc bw))
    ;; --- token partition: per MB, Y2 DC then 16 empty luma blocks, then chroma DC ---
    (let* ((probs +default-coef-probs+)
           ;; DC prediction is 128, so code (target - 128) transformed.  For a flat block the
           ;; forward DCT's DC = 16*diff/8 = diff*... — compute exactly via fdct on a flat block.
           (ydiff (flat-dc-coeff (- y 128)))
           (udiff (flat-dc-coeff (- u 128)))
           (vdiff (flat-dc-coeff (- v 128)))
           ;; the sixteen luma DCs are themselves WHT'd into Y2; a flat MB puts everything in Y2 DC
           (y2dc (flat-wht-dc ydiff))
           (qy2 (quant1 y2dc y2dcq)) (qu (quant1 udiff uvdcq)) (qv (quant1 vdiff uvdcq)))
      (dotimes (i nmb)
        (write-dc-only-block tw probs +blk-y2+ qy2 0)                 ; Y2 (the luma DC block)
        (dotimes (b 16) (write-token tw probs +blk-y-after-y2+ 1 0 +tok-eob+))  ; luma: no AC
        (dotimes (b 4) (write-dc-only-block tw probs +blk-uv+ qu 0))  ; U
        (dotimes (b 4) (write-dc-only-block tw probs +blk-uv+ qv 0))))
    (let ((part1 (bwrite-finish bw)) (part2 (bwrite-finish tw)) (frame (u8buf)))
      (write-uncompressed-header frame width height (length part1))
      (loop for b across part1 do (vector-push-extend b frame))
      (loop for b across part2 do (vector-push-extend b frame))
      frame)))

(defun flat-dc-coeff (diff)
  "Forward-DCT DC coefficient of a flat 4x4 block of value DIFF."
  (let ((in (mkblk diff)) (out (mkblk))) (fdct4x4 in out) (aref out 0)))
(defun flat-wht-dc (dc)
  "Forward-WHT DC of sixteen identical luma DC values."
  (let ((in (mkblk dc)) (out (mkblk))) (fwht4x4 in out) (aref out 0)))
(defun quant1 (v q) (if (minusp v) (- (floor (+ (- v) (floor q 2)) q)) (floor (+ v (floor q 2)) q)))
