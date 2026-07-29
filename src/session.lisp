;;;; session.lisp — glue that runs full-duplex G.711 audio over a webrtc-data ICE/DTLS transport.
;;;;
;;;; This is the layer that ties the pure media plane (g711/rtp/srtp) to the live connection:
;;;; derive SRTP keys from the finished DTLS handshake, install an ON-MEDIA handler that
;;;; decrypts+decodes inbound RTP (reporting a level), and run a sender thread.  For the first
;;;; bring-up the sender was a *fake source* — a beeping tone — to prove the channel end to end.
;;;; That is done, so the default is now silence (:tone t restores the beep) until a real audio
;;;; device on the box feeds it.

(in-package #:webrtc-media)

(defconstant +audio-frame-samples+ 160)          ; 20 ms @ 8 kHz
(defparameter *audio-frame-secs* 0.02d0)

(defun %rms-level (pcm)
  "RMS magnitude of a (signed-byte 16) frame, normalized to 0.0-1.0."
  (let ((sum 0d0) (n (length pcm)))
    (dotimes (i n) (let ((s (aref pcm i))) (incf sum (* (float s 1d0) s))))
    (min 1d0 (/ (sqrt (/ sum (max 1 n))) 8000d0))))

(defun start-audio (agent session &key on-rx-level on-tx-level (tone-hz 880d0) (ssrc #x1a2b3c4d)
                                       (tone nil) log)
  "Wire full-duplex G.711/SRTP audio onto webrtc-data AGENT and its finished DTLS SESSION.
Installs an ON-MEDIA handler that decrypts+decodes inbound SRTP (calling ON-RX-LEVEL with a
0-1 level).  The outbound side sends PCMU silence, keeping the stream (and its timing) alive
without making noise; TONE t restores the bring-up beep, which was only ever a liveness probe.
Returns (values stop-thunk srtp-context)."
  (let ((ctx (srtp-context-from-dtls session :role :client))
        (running t))
    ;; inbound: SRTP -> RTP -> PCMU -> level  (with bring-up diagnostics)
    (let ((rx 0) (dec 0) (bad 0))
      (setf (webrtc-data:ice-agent-on-media agent)
            (lambda (pkt a b)
              (declare (ignore a b))
              (incf rx)
              (when (and log (= rx 1)) (funcall log (format nil "rx: first SRTP packet (~a bytes)" (length pkt))))
              (handler-case
                  (let ((rtp (srtp-unprotect ctx pkt)))
                    (cond
                      (rtp (incf dec)
                           (when (and log (= dec 1)) (funcall log "rx: SRTP DECRYPTED ok — keying good, box hears you"))
                           (when on-rx-level (funcall on-rx-level (%rms-level (pcmu-decode (rtp-payload (rtp-parse rtp)))))))
                      (t (incf bad)
                         (when (and log (= bad 1)) (funcall log "rx: SRTP arriving but DECRYPT FAILS -> keying mismatch")))))
                (error (e) (when log (funcall log (format nil "audio rx err: ~a" e))))))))
    ;; outbound: PCMU silence by default (keeps the stream + its clock alive); :tone t
    ;; brings back the 880 Hz bring-up beep
    (bt:make-thread
     (lambda ()
       (let ((seq 0) (ts 0) (frame 0) (phase 0d0)
             (dphase (/ (* 2d0 pi tone-hz) 8000d0))
             (pcm (make-array +audio-frame-samples+ :element-type '(signed-byte 16))))
         (loop while running do
           (handler-case
               (let ((beep (and tone (< (mod frame 30) 8))))
                 (dotimes (i +audio-frame-samples+)
                   (setf (aref pcm i) (if beep (round (* 9000d0 (sin phase))) 0))
                   (incf phase dphase) (when (> phase (* 2d0 pi)) (decf phase (* 2d0 pi))))
                 (when on-tx-level (funcall on-tx-level (if beep 0.55d0 0d0)))
                 (webrtc-data:ice-send-to-peer
                  agent
                  (srtp-protect ctx (rtp-pack (make-rtp :pt 0 :marker (zerop frame)
                                                        :seq (logand seq #xffff)
                                                        :timestamp (logand ts #xffffffff)
                                                        :ssrc ssrc :payload (pcmu-encode pcm)))))
                 (incf seq) (incf ts +audio-frame-samples+) (incf frame))
             (error (e) (when log (funcall log (format nil "audio tx: ~a" e)))))
           (sleep *audio-frame-secs*))))
     :name "wm-audio-tx")
    ;; return both a stop thunk and the SRTP context (video reuses it — same DTLS keys, own SSRC)
    (values (lambda () (setf running nil (webrtc-data:ice-agent-on-media agent) nil)) ctx)))
