;;;; vp8-rtp.lisp — VP8 RTP payload format (RFC 7741) + the video sender.
;;;;
;;;; A VP8 frame is split across RTP packets, each carrying a payload descriptor:
;;;;
;;;;   0 1 2 3 4 5 6 7
;;;;  +-+-+-+-+-+-+-+-+
;;;;  |X|R|N|S|R| PID |   X=extended control bits, S=start of partition, PID=partition index
;;;;
;;;; We emit the minimal form: one byte, X=0, no PictureID.  S=1 on the first packet of the frame,
;;;; PID=0, and the RTP marker bit set on the LAST packet of the frame.  Timestamps run at 90 kHz.

(in-package #:webrtc-media)

(defconstant +vp8-rtp-clock+ 90000)
(defparameter *vp8-max-payload* 1100)     ; keep the whole SRTP packet under a safe path MTU

(defun vp8-packetize (frame seq ts ssrc pt &key (max-payload *vp8-max-payload*))
  "Split a VP8 FRAME into a list of packed RTP packets (RFC 7741 minimal descriptor).
SEQ is the starting sequence number; returns (values packets next-seq)."
  (let* ((n (length frame)) (out '()) (off 0) (s seq))
    (loop while (< off n) do
      (let* ((chunk (min max-payload (- n off)))
             (first (zerop off))
             (last (>= (+ off chunk) n))
             (payload (make-array (1+ chunk) :element-type '(unsigned-byte 8))))
        (setf (aref payload 0) (if first #x10 #x00))     ; S=1 on the first packet, PID=0
        (replace payload frame :start1 1 :start2 off :end2 (+ off chunk))
        (push (rtp-pack (make-rtp :pt pt :marker last :seq (logand s #xffff)
                                  :timestamp (logand ts #xffffffff) :ssrc ssrc :payload payload))
              out)
        (incf s) (incf off chunk)))
    (values (nreverse out) s)))

(defun start-video (agent session ctx &key (pt 96) (width 64) (height 64) (fps 5)
                                           (ssrc #x5e5e0001) log)
  "Stream VP8 keyframes to the peer over SRTP.  CTX is the SRTP context (shared with audio — same
DTLS keys, different SSRC).  Sends a solid colour that cycles, so motion is obvious on screen.
Returns a STOP thunk."
  (let ((running t))
    (bt:make-thread
     (lambda ()
       (let ((seq 0) (ts 0) (n 0))
         (loop while running do
           (handler-case
               ;; alternate two ffmpeg-verified frames so motion is unmistakable:
               ;; even = uniform mid-gray (all macroblocks skipped), odd = bright/gradient
               ;; pattern (first column left at the DC prediction, the rest coded bright).
               (let ((frame (if (evenp (floor n 3))
                                (vp8::encode-keyframe-gray width height)
                                (vp8::encode-keyframe-checker width height :phase 1))))
                 (multiple-value-bind (pkts next) (vp8-packetize frame seq ts ssrc pt)
                   (setf seq next)
                   (dolist (p pkts)
                     (webrtc-data:ice-send-to-peer agent (srtp-protect ctx p)))
                   (when (and log (zerop (mod n 25)))
                     (funcall log (format nil "video: frame ~a, ~a bytes, ~a pkt(s)" n (length frame) (length pkts))))
                   (incf n)
                   (incf ts (floor +vp8-rtp-clock+ fps))))
             (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
           (sleep (/ 1.0 fps)))))
     :name "wm-video-tx")
    (lambda () (setf running nil))))
