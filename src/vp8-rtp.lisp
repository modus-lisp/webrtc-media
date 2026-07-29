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
  "Time BODY and return ALL of its values — multiple-value-prog1, not a let: the frame source
returns (values y u v w h dirty), and collapsing that to one value silently nils the rest."
  (let ((t0 (gensym)))
    `(let ((,t0 (get-internal-real-time)))
       (multiple-value-prog1 (progn ,@body)
         (ps-note ,st (/ (* 1000d0 (- (get-internal-real-time) ,t0))
                         internal-time-units-per-second))))))

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
                                           (ssrc #x5e5e0001) (qi 12) (key-interval 120) (poll-secs 0.010)
                                           (max-qi 44) (target-kbs 150) (cleanup-ms 700)
                                           (max-frame-kb 24) source log)
  "Stream VP8 keyframes to the peer over SRTP.  CTX is the SRTP context (shared with audio — same
DTLS keys, different SSRC).  SOURCE, when given, is a thunk returning (values Y U V w h) for the
next frame or NIL if nothing changed since last time — so an idle screen costs no bandwidth.
Without a SOURCE it sends the built-in test pattern.  The first frame (and every KEY-INTERVAL
frames, so a late or recovering viewer resynchronises) is a keyframe; the rest are inter frames
where an unchanged macroblock costs a single bit.

Quality adapts to the link: QI is the best (sharpest) quantizer, MAX-QI the worst we will fall
back to, and TARGET-KBS the rate we steer toward — a busy screen coarsens rather than building a
queue.  Then, once the screen has been quiet for CLEANUP-MS, whatever was coded coarsely is
re-coded at QI, so a burst of motion leaves no lasting mush behind.  Returns a STOP thunk."
  (declare (ignore session))
  (let ((running t))
    (bt:make-thread
     (lambda ()
       (let ((seq 0) (ts 0) (n 0) (sent 0) (enc nil) (skipped 0)
             (st-take (make-pstat :name "take")) (st-enc (make-pstat :name "encode"))
             (st-send (make-pstat :name "send")) (st-loop (make-pstat :name "loop"))
             ;; the adaptive path gets its own counters: a refinement is a different animal from a
             ;; live frame, and per-FRAME size matters as much as average rate — one 90 KB frame is
             ;; ~360 ms of transmission on a 250 KB/s link, which reads as a freeze no matter what
             ;; the 5-second average says.
             (st-clean (make-pstat :name "refine")) (st-kb (make-pstat :name "frameKB"))
             (st-mbs (make-pstat :name "codedMB")) (st-qi (make-pstat :name "qi"))
             (st-lag (make-pstat :name "lag"))
             (last-report (get-internal-real-time)) (last-tick (get-internal-real-time)) (bytes 0)
             (cur-qi qi) (rc-bytes 0) (rc-since (get-internal-real-time))
             (idle-since (get-internal-real-time)) (cleanups 0)
             ;; macroblocks that wanted coding but did not fit in a frame's byte budget, carried
             ;; forward so a big change refreshes progressively instead of in one huge frame
             (pending nil) (bpm 110d0))
         (loop while running do
           (handler-case
               (multiple-value-bind (y u v w h dirty)
                   (with-timing (st-take) (if source (funcall source) (values nil nil nil nil nil nil)))
                 (let* ((need-key (or (null enc) (zerop (mod n key-interval))))
                        (frame
                          (cond
                            ((null y)
                             (if (null source)                 ; built-in test pattern
                                 (if (evenp (floor n 3))
                                     (vp8::encode-keyframe-gray width height)
                                     (vp8::encode-keyframe-checker width height :phase 1))
                                 ;; nothing changed.  If the screen has been quiet a moment and we
                                 ;; owe a refinement, re-code the coarsely-coded macroblocks at the
                                 ;; best qi: the residual against the existing reference sharpens
                                 ;; the decoder's copy without anything having moved.
                                 (let ((st (and enc (vp8::ve-stale-mbs enc))))
                                   (when (and st (position 1 st)
                                              (> (/ (* 1000d0 (- (get-internal-real-time) idle-since))
                                                    internal-time-units-per-second)
                                                 cleanup-ms))
                                     (incf cleanups)
                                     (setf cur-qi qi)
                                     (ps-note st-mbs (count 1 st))
                                     (with-timing (st-clean)
                                       (vp8::encode-inter-frame enc (vp8::ve-prev-y enc)
                                                                (vp8::ve-prev-u enc) (vp8::ve-prev-v enc)
                                                                :qi qi :dirty (copy-seq st)
                                                                :force t :clean-qi qi
                                                                ;; budget the refinement too: an
                                                                ;; uncoded macroblock stays stale,
                                                                ;; so it simply resumes next pass
                                                                :max-mbs (max 48 (floor (* max-frame-kb 1024)
                                                                                        (max 1d0 bpm)))))))))
                            ;; first frame, or a periodic refresh: keyframe, and it seeds the
                            ;; reference the following inter frames predict from
                            ((or need-key (/= w (vp8::ve-width enc)) (/= h (vp8::ve-height enc)))
                             (setf enc (vp8::make-encoder w h))
                             (multiple-value-bind (f ry ru rv) (vp8:encode-gray-frame y w h :qi cur-qi :u u :v v)
                               (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru (vp8::ve-ref-v enc) rv)
                               (replace (vp8::ve-prev-y enc) y) (replace (vp8::ve-prev-u enc) u)
                               (replace (vp8::ve-prev-v enc) v)
                               (setf (vp8::ve-have-ref enc) t)
                               f))
                            ;; otherwise an inter frame: unchanged macroblocks are one skip bit
                            (t (setf idle-since (get-internal-real-time))
                               (let ((want (cond ((null pending) dirty)
                                                 ((null dirty) pending)
                                                 (t (bit-ior pending dirty)))))
                                 (multiple-value-bind (f nmb defer)
                                     (with-timing (st-enc)
                                       (vp8::encode-inter-frame
                                        enc y u v :qi cur-qi :dirty want :force t :clean-qi qi
                                        :max-mbs (max 48 (floor (* max-frame-kb 1024) (max 1d0 bpm)))))
                                   (setf pending defer)
                                   (when nmb
                                     (ps-note st-mbs nmb)
                                     (when (and f (plusp nmb))    ; keep the cost estimate honest
                                       (setf bpm (+ (* 0.7d0 bpm) (* 0.3d0 (/ (length f) nmb))))))
                                   f))))))
                   (if frame
                       (with-timing (st-send)
                         (multiple-value-bind (pkts next) (vp8-packetize frame seq ts ssrc pt)
                           (setf seq next)
                           (dolist (p pkts)
                             (webrtc-data:ice-send-to-peer agent (srtp-protect ctx p)))
                           (incf sent) (incf bytes (length frame)) (incf rc-bytes (length frame))
                           (ps-note st-kb (/ (length frame) 1024d0))
                           (ps-note st-qi cur-qi)))
                       (incf skipped))
                   ;; rate control: every ~500 ms compare the achieved rate with the target and
                   ;; nudge the quantizer.  Coarsening under load keeps latency down instead of
                   ;; queueing frames the link cannot carry.
                   (let ((rdt (/ (* 1000d0 (- (get-internal-real-time) rc-since))
                                 internal-time-units-per-second)))
                     (when (> rdt 500)
                       (let ((kbs (/ rc-bytes 1.024 rdt)))
                         (cond ((> kbs target-kbs) (setf cur-qi (min max-qi (+ cur-qi 4))))
                               ((< kbs (* 0.6 target-kbs)) (setf cur-qi (max qi (- cur-qi 2))))))
                       (setf rc-bytes 0 rc-since (get-internal-real-time))))
                   ;; every ~5 s: min/mean/max per stage, so choppiness can be attributed
                   (let ((dt (/ (* 1000d0 (- (get-internal-real-time) last-report))
                                internal-time-units-per-second)))
                     (when (and log (> dt 5000))
                       (funcall log (format nil "~a | ~a | ~a | ~a | ~a | ~a | sent ~a idle ~a | ~,1f KB/s | ~,1f fps | qi ~a b/mb ~,0f | pend ~a | cleans ~a"
                                            (ps-line st-take) (ps-line st-enc) (ps-line st-clean)
                                            (ps-line st-send) (ps-line st-kb) (ps-line st-mbs)
                                            sent skipped (/ bytes 1.024 dt) (/ (* 1000d0 sent) dt)
                                            cur-qi bpm (if pending (count 1 pending) 0) cleanups))
                       (ps-reset st-take) (ps-reset st-enc) (ps-reset st-send) (ps-reset st-loop)
                       (ps-reset st-clean) (ps-reset st-kb) (ps-reset st-mbs) (ps-reset st-qi) (ps-reset st-lag)
                       (setf sent 0 skipped 0 bytes 0 cleanups 0 last-report (get-internal-real-time))))
                   (incf n)
                   ;; advance the RTP clock by time actually elapsed: frames appear when the
                   ;; desktop changes, not on a fixed cadence, so a nominal 1/fps step would drift
                   (let ((now (get-internal-real-time)))
                     (incf ts (max 1 (floor (* +vp8-rtp-clock+ (- now last-tick))
                                            internal-time-units-per-second)))
                     (setf last-tick now))))
             (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
           ;; poll on a short tick rather than 1/fps: capture-take returns NIL when nothing
           ;; changed (cheap), so this only affects how quickly a change is noticed, and the
           ;; encode itself is what paces us on a busy screen.
           (sleep poll-secs))))
     :name "wm-video-tx")
    (lambda () (setf running nil))))
