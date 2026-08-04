;;;; session.lisp — glue that runs full-duplex G.711 audio over a webrtc-data ICE/DTLS transport.
;;;;
;;;; This is the layer that ties the pure media plane (g711/rtp/srtp) to the live connection:
;;;; derive SRTP keys from the finished DTLS handshake, install an ON-MEDIA handler that
;;;; decrypts+decodes inbound RTP, and run a sender thread on a 20 ms clock.
;;;;
;;;; Both ends of it are open, and that is the point of the file.  Outbound, SOURCE is asked for
;;;; a frame every 20 ms (audio.lisp says what a frame is); inbound, ON-RX-PCM is handed the
;;;; samples that arrived.  Without those two the channel could only prove itself — it beeped a
;;;; tone at the peer and reported an RMS number about whatever came back, and threw the audio
;;;; away.  That was the right shape for bring-up and the wrong shape for carrying anything.
;;;;
;;;; What is still NOT here, so nobody plans around it: no jitter buffer, no packet-loss
;;;; concealment, no RTCP sender/receiver reports, and no A/V clock sync (this sender keeps its
;;;; own 20 ms deadline; the video sender paces itself separately).  A real source will be
;;;; audible over a clean link and will sound broken over a lossy one, and that order of work is
;;;; deliberate: there was nothing to conceal the loss OF until now.

(in-package #:webrtc-media)

(defun start-audio (agent session &key source on-rx-pcm on-rx-level on-tx-level
                                       (tone-hz 880d0) (ssrc #x1a2b3c4d) (tone nil) log)
  "Wire full-duplex G.711/SRTP audio onto webrtc-data AGENT and its finished DTLS SESSION.

Outbound: SOURCE is asked for one 20 ms frame of 8 kHz mono (signed-byte 16) samples every
20 ms — see FILL-AUDIO-FRAME for what it may return.  It runs ON THE SENDER'S THREAD, which is
the stream's clock, so it must not block: read from a queue a capture thread fills, do not wait
on a device.  With no SOURCE the sender emits silence, which keeps the stream and its timing
alive without making noise; TONE t substitutes the MAKE-TONE-SOURCE beep instead, the old
bring-up probe.  SOURCE wins over TONE.

Inbound: every packet that decrypts is decoded, and the samples go to ON-RX-PCM as
(PCM RTP) — the frame, plus the parsed RTP header a jitter buffer would need (seq, timestamp,
marker).  The frame is freshly allocated per packet, so a consumer may keep it.  ON-RX-LEVEL
still gets the 0-1 RMS of the same frame, for meters that want a number and not audio.

Returns (values stop-thunk srtp-context)."
  (let ((ctx (srtp-context-from-dtls session :role :client))
        (src (or source (and tone (make-tone-source :hz tone-hz))))
        (running t))
    ;; inbound: SRTP -> RTP -> PCMU -> samples  (with bring-up diagnostics)
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
                           (when (or on-rx-pcm on-rx-level)
                             (let* ((r (rtp-parse rtp))
                                    (pcm (pcmu-decode (rtp-payload r))))
                               ;; the sink first: it is the consumer that has somewhere to put
                               ;; this, and the meter is derived from the same frame anyway.
                               (when on-rx-pcm (funcall on-rx-pcm pcm r))
                               (when on-rx-level (funcall on-rx-level (rms-level pcm))))))
                      (t (incf bad)
                         (when (and log (= bad 1)) (funcall log "rx: SRTP arriving but DECRYPT FAILS -> keying mismatch")))))
                (error (e) (when log (funcall log (format nil "audio rx err: ~a" e))))))))
    ;; outbound: one frame per 20 ms, on a deadline rather than on (sleep 0.02).
    (bt:make-thread
     (lambda ()
       (let* ((seq 0) (ts 0) (frame 0) (warned nil)
              (period (max 1 (round (* *audio-frame-secs* internal-time-units-per-second))))
              (next (+ (get-internal-real-time) period))
              (pcm (make-audio-frame)))
         (loop while running do
           (multiple-value-bind (voiced n)
               ;; A source that throws is a silent frame, not a dead stream: its own bug should
               ;; not take the clock down with it.
               (handler-case (fill-audio-frame pcm src)
                 (error (e)
                   (when log (funcall log (format nil "audio source: ~a" e)))
                   (fill pcm 0)
                   (values nil 0)))
             (when (and voiced (/= n +audio-frame-samples+) (not warned))
               (setf warned t)
               (when log (funcall log (format nil "audio source gave ~d samples, want ~d — padding/truncating"
                                              n +audio-frame-samples+))))
             (handler-case
                 (progn
                   (when on-tx-level (funcall on-tx-level (if voiced (rms-level pcm) 0d0)))
                   (webrtc-data:ice-send-to-peer
                    agent
                    (srtp-protect ctx (rtp-pack (make-rtp :pt 0 :marker (zerop frame)
                                                          :seq (logand seq #xffff)
                                                          :timestamp (logand ts #xffffffff)
                                                          :ssrc ssrc :payload (pcmu-encode pcm))))))
               (error (e) (when log (funcall log (format nil "audio tx: ~a" e))))))
           ;; The RTP clock advances whether or not that send worked.  Sequence number and
           ;; timestamp describe WHEN a frame was sampled, not whether we managed to post it; a
           ;; failed send is a lost packet, and repeating a sequence number after one tells the
           ;; peer's jitter buffer a lie it cannot recover from.
           (incf seq) (incf ts +audio-frame-samples+) (incf frame)
           ;; Sleep to the next deadline, not for a fixed 20 ms — FRAME-DEADLINE says why.
           (multiple-value-bind (ticks new-next)
               (frame-deadline next (get-internal-real-time) period)
             (setf next new-next)
             (when (plusp ticks)
               (sleep (/ ticks (float internal-time-units-per-second 1d0))))))))
     :name "wm-audio-tx")
    ;; return both a stop thunk and the SRTP context (video reuses it — same DTLS keys, own SSRC)
    (values (lambda () (setf running nil (webrtc-data:ice-agent-on-media agent) nil)) ctx)))
