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

;;; ---- pipeline timing -------------------------------------------------------
;;; Choppiness can come from anywhere in capture -> convert -> encode -> send, and guessing which
;;; is expensive.  Each stage keeps min/max/mean so a report says where the time actually goes.
(defstruct (pstat (:conc-name ps-)) (name "") (n 0) (min 1d9) (max 0d0) (total 0d0))

(defun ps-note (st ms)
  (incf (ps-n st)) (incf (ps-total st) ms)
  (when (< ms (ps-min st)) (setf (ps-min st) ms))
  (when (> ms (ps-max st)) (setf (ps-max st) ms)))

(defmacro with-timing ((st) &body body)
  (let ((t0 (gensym)) (r (gensym)))
    `(let* ((,t0 (get-internal-real-time)) (,r (progn ,@body)))
       (ps-note ,st (/ (* 1000d0 (- (get-internal-real-time) ,t0))
                       internal-time-units-per-second))
       ,r)))

(defun ps-line (st)
  (if (zerop (ps-n st)) (format nil "~a: -" (ps-name st))
      (format nil "~a ~,1f/~,1f/~,1f" (ps-name st)
              (ps-min st) (/ (ps-total st) (ps-n st)) (ps-max st))))

(defun ps-reset (st) (setf (ps-n st) 0 (ps-min st) 1d9 (ps-max st) 0d0 (ps-total st) 0d0))
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
                                           (ssrc #x5e5e0001) (qi 12) (key-interval 120) source log)
  "Stream VP8 keyframes to the peer over SRTP.  CTX is the SRTP context (shared with audio — same
DTLS keys, different SSRC).  SOURCE, when given, is a thunk returning (values Y U V w h) for the
next frame or NIL if nothing changed since last time — so an idle screen costs no bandwidth.
Without a SOURCE it sends the built-in test pattern.  The first frame (and every KEY-INTERVAL
frames, so a late or recovering viewer resynchronises) is a keyframe; the rest are inter frames
where an unchanged macroblock costs a single bit.  Returns a STOP thunk."
  (declare (ignore session))
  (let ((running t))
    (bt:make-thread
     (lambda ()
       (let ((seq 0) (ts 0) (n 0) (sent 0) (enc nil) (skipped 0)
             (st-take (make-pstat :name "take")) (st-enc (make-pstat :name "encode"))
             (st-send (make-pstat :name "send")) (st-loop (make-pstat :name "loop"))
             (last-report (get-internal-real-time)) (bytes 0))
         (loop while running do
           (handler-case
               (multiple-value-bind (y u v w h dirty)
                   (with-timing (st-take) (if source (funcall source) (values nil nil nil nil nil nil)))
                 (let* ((need-key (or (null enc) (zerop (mod n key-interval))))
                        (frame
                          (cond
                            ((null y) (when (null source)     ; built-in test pattern
                                        (if (evenp (floor n 3))
                                            (vp8::encode-keyframe-gray width height)
                                            (vp8::encode-keyframe-checker width height :phase 1))))
                            ;; first frame, or a periodic refresh: keyframe, and it seeds the
                            ;; reference the following inter frames predict from
                            ((or need-key (/= w (vp8::ve-width enc)) (/= h (vp8::ve-height enc)))
                             (setf enc (vp8::make-encoder w h))
                             (multiple-value-bind (f ry ru rv) (vp8:encode-gray-frame y w h :qi qi :u u :v v)
                               (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru (vp8::ve-ref-v enc) rv)
                               (replace (vp8::ve-prev-y enc) y) (replace (vp8::ve-prev-u enc) u)
                               (replace (vp8::ve-prev-v enc) v)
                               (setf (vp8::ve-have-ref enc) t)
                               f))
                            ;; otherwise an inter frame: unchanged macroblocks are one skip bit
                            (t (with-timing (st-enc) (vp8::encode-inter-frame enc y u v :qi qi :dirty dirty))))))
                   (if frame
                       (with-timing (st-send)
                         (multiple-value-bind (pkts next) (vp8-packetize frame seq ts ssrc pt)
                           (setf seq next)
                           (dolist (p pkts)
                             (webrtc-data:ice-send-to-peer agent (srtp-protect ctx p)))
                           (incf sent) (incf bytes (length frame))))
                       (incf skipped))
                   ;; every ~5 s: min/mean/max per stage, so choppiness can be attributed
                   (let ((dt (/ (* 1000d0 (- (get-internal-real-time) last-report))
                                internal-time-units-per-second)))
                     (when (and log (> dt 5000))
                       (funcall log (format nil "~a | ~a | ~a | sent ~a idle ~a | ~,1f KB/s | ~,1f fps"
                                            (ps-line st-take) (ps-line st-enc) (ps-line st-send)
                                            sent skipped (/ bytes 1.024 dt) (/ (* 1000d0 sent) dt)))
                       (ps-reset st-take) (ps-reset st-enc) (ps-reset st-send) (ps-reset st-loop)
                       (setf sent 0 skipped 0 bytes 0 last-report (get-internal-real-time))))
                   (incf n)
                   (incf ts (floor +vp8-rtp-clock+ fps))))
             (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
           (sleep (/ 1.0 fps)))))
     :name "wm-video-tx")
    (lambda () (setf running nil))))
