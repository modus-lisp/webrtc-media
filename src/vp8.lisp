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
