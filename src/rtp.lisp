;;;; rtp.lisp — RTP packet framing (RFC 3550 §5).
;;;;
;;;; Fixed 12-byte header + payload.  We don't emit CSRCs or header extensions, but PARSE tolerates
;;;; them (skips CSRC list + one extension block) so inbound packets from the browser are handled.
;;;;
;;;;   0                   1                   2                   3
;;;;   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;;;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;;;  |V=2|P|X|  CC   |M|     PT      |       sequence number         |
;;;;  |                           timestamp                           |
;;;;  |                             SSRC                              |

(in-package #:webrtc-media)

(defstruct (rtp (:constructor make-rtp (&key pt marker seq timestamp ssrc payload)))
  (pt 0 :type (unsigned-byte 7))
  (marker nil)
  (seq 0 :type (unsigned-byte 16))
  (timestamp 0 :type (unsigned-byte 32))
  (ssrc 0 :type (unsigned-byte 32))
  (payload #() :type (vector (unsigned-byte 8))))

(declaim (inline u8v))
(defun u8v (n) (make-array n :element-type '(unsigned-byte 8)))

(defun rtp-pack (p)
  "Serialize an RTP struct to bytes: 12-byte header (V=2, no padding/ext/CSRC) + payload."
  (let* ((pl (rtp-payload p)) (n (length pl))
         (buf (u8v (+ 12 n))))
    (setf (aref buf 0) #x80                                     ; V=2, P=0, X=0, CC=0
          (aref buf 1) (logior (if (rtp-marker p) #x80 0) (logand (rtp-pt p) #x7f)))
    (setf (aref buf 2) (ldb (byte 8 8) (rtp-seq p)) (aref buf 3) (ldb (byte 8 0) (rtp-seq p)))
    (loop for i below 4 do (setf (aref buf (+ 4 i)) (ldb (byte 8 (* 8 (- 3 i))) (rtp-timestamp p))))
    (loop for i below 4 do (setf (aref buf (+ 8 i)) (ldb (byte 8 (* 8 (- 3 i))) (rtp-ssrc p))))
    (replace buf pl :start1 12)
    buf))

(defun %be (buf off n)
  (let ((v 0)) (dotimes (i n v) (setf v (logior (ash v 8) (aref buf (+ off i)))))))

(defun rtp-parse (buf)
  "Parse RTP bytes -> an RTP struct.  Skips any CSRC list and one header extension."
  (let* ((b0 (aref buf 0)) (b1 (aref buf 1))
         (cc (logand b0 #x0f)) (ext (logbitp 4 b0))
         (off (+ 12 (* 4 cc))))
    (when ext                                                   ; extension: [id(2)][len words(2)] + len*4
      (let ((words (%be buf (+ off 2) 2))) (incf off (+ 4 (* 4 words)))))
    (make-rtp :pt (logand b1 #x7f) :marker (logbitp 7 b1)
              :seq (%be buf 2 2) :timestamp (%be buf 4 4) :ssrc (%be buf 8 4)
              :payload (subseq buf off))))
