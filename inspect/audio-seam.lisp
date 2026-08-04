;;;; inspect/audio-seam.lisp — does a SIGNAL survive the audio path, or only bytes?
;;;;
;;;; The media plane could always be checked for liveness: packets went out, packets decrypted.
;;;; That says the channel is up.  It does not say what came out the other end SOUNDS like what
;;;; went in, and until the sender had a real source there was nothing to ask that about.
;;;;
;;;; So this gate puts a tone of a known frequency in at SOURCE — the thunk START-AUDIO asks for
;;;; a frame every 20 ms — and runs it through everything except ICE: G.711 encode, RTP pack,
;;;; SRTP protect, then the inbound half (unprotect, parse, decode) with a SEPARATE context
;;;; keyed the mirror way, exactly as the two ends of a call are.  Then it measures the result
;;;; with a Goertzel filter and insists the energy is at the frequency we sent, not merely
;;;; somewhere.  A path that dropped, doubled, reversed or half-rate-ed the samples still
;;;; delivers plausible bytes; it does not still deliver 440 Hz.
;;;;
;;;; ICE is left out deliberately: it is webrtc-data's, it is proven, and needing a live peer to
;;;; run this would mean nobody runs it.
;;;;
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load inspect/audio-seam.lisp

(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :webrtc-media)))

(defpackage #:wm-audio-seam (:use #:cl) (:local-nicknames (#:wm #:webrtc-media)))
(in-package #:wm-audio-seam)

(defvar *pass* 0) (defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (progn (incf *pass*) (format t "  ok   ~a = ~s~%" name got))
      (progn (incf *fail*) (format t "  FAIL ~a: got ~s, want ~s~%" name got want)))
  (finish-output))
(defun check-that (name ok &optional detail)
  (if ok (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

;;; ---- measurement -----------------------------------------------------------
(defun goertzel (pcm hz &optional (rate 8000))
  "Energy in PCM at HZ.  One bin of a DFT, computed without one — this is the whole reason the
gate can name a frequency instead of eyeballing a waveform."
  (let* ((n (length pcm))
         (k (round (/ (* n hz) rate)))
         (w (/ (* 2d0 pi k) n))
         (coeff (* 2d0 (cos w)))
         (s0 0d0) (s1 0d0) (s2 0d0))
    (dotimes (i n)
      (setf s0 (+ (aref pcm i) (* coeff s1) (- s2)) s2 s1 s1 s0))
    (sqrt (max 0d0 (+ (* s1 s1) (* s2 s2) (- (* s1 s2 coeff)))))))

(defun snr-db (orig recon)
  "Signal-to-noise of RECON against ORIG, in dB.  G.711 is lossy by design (8 bits of companded
µ-law), so this is not expected to be huge — only to be the ~35 dB µ-law actually gives."
  (let ((sig 0d0) (err 0d0))
    (dotimes (i (length orig))
      (let ((o (float (aref orig i) 1d0)) (r (float (aref recon i) 1d0)))
        (incf sig (* o o)) (incf err (expt (- o r) 2))))
    (if (zerop err) 999d0 (* 10d0 (log (/ sig (max 1d-9 err)) 10)))))

;;; ---- a two-ended SRTP pair, keyed the way a call is -------------------------
(defun key-pair ()
  "Two contexts whose send/recv keys mirror, as DTLS-SRTP would give the two peers.  Fixed bytes:
this gate is about the audio surviving, and a random key would only make a failure unrepeatable."
  (let ((k1 (make-array 16 :element-type '(unsigned-byte 8)
                           :initial-contents (loop for i below 16 collect (+ 1 i))))
        (s1 (make-array 12 :element-type '(unsigned-byte 8)
                           :initial-contents (loop for i below 12 collect (+ 100 i))))
        (k2 (make-array 16 :element-type '(unsigned-byte 8)
                           :initial-contents (loop for i below 16 collect (+ 200 i))))
        (s2 (make-array 12 :element-type '(unsigned-byte 8)
                           :initial-contents (loop for i below 12 collect (+ 50 i)))))
    (values (wm::make-srtp-context-keys k1 s1 k2 s2)      ; the sender
            (wm::make-srtp-context-keys k2 s2 k1 s1))))   ; the receiver

(defstruct rx-frame pcm rtp)

(defun run-call (source frames &key (ssrc #x1a2b3c4d) (drop nil))
  "Send FRAMES frames from SOURCE through the full media path and collect what the far end
decodes.  This is START-AUDIO's tx loop and its on-media handler, minus the transport and the
thread — the same calls in the same order.  DROP is a list of frame indexes to lose in flight."
  (multiple-value-bind (tx rx) (key-pair)
    (let ((pcm (wm:make-audio-frame))
          (seq 0) (ts 0) (frame 0) (sent nil) (got nil))
      (dotimes (i frames)
        (multiple-value-bind (voiced n) (wm:fill-audio-frame pcm source)
          (declare (ignore voiced n))
          (push (copy-seq pcm) sent)
          (let ((wire (wm:srtp-protect
                       tx (wm:rtp-pack (wm:make-rtp :pt 0 :marker (zerop frame)
                                                    :seq (logand seq #xffff)
                                                    :timestamp (logand ts #xffffffff)
                                                    :ssrc ssrc
                                                    :payload (wm:pcmu-encode pcm))))))
            (unless (member i drop)
              (let ((clear (wm:srtp-unprotect rx wire)))
                (when clear
                  (let ((r (wm:rtp-parse clear)))
                    (push (make-rx-frame :pcm (wm:pcmu-decode (wm:rtp-payload r)) :rtp r)
                          got))))))
          (incf seq) (incf ts wm:+audio-frame-samples+) (incf frame)))
      (values (nreverse got) (nreverse sent)))))

(defun splice (frames key)
  (let* ((parts (mapcar key frames))
         (out (make-array (reduce #'+ parts :key #'length)
                          :element-type '(signed-byte 16)))
         (at 0))
    (dolist (p parts out) (replace out p :start1 at) (incf at (length p)))))

;;; ---- 1. the frame: what a SOURCE may hand back -----------------------------
(format t "~&=== the audio frame ===~%")
(let ((pcm (wm:make-audio-frame)))
  (check "no source at all is silence" (multiple-value-list (wm:fill-audio-frame pcm nil))
         '(nil 0))
  (check-that "and the frame really is zeroed" (every #'zerop pcm))

  (setf (aref pcm 0) 999)                        ; dirty it, to prove the next fill clears it
  (check "a source with nothing to say is silence too"
         (multiple-value-list (wm:fill-audio-frame pcm (lambda () nil))) '(nil 0))
  (check-that "and that stale sample is gone" (zerop (aref pcm 0)))

  ;; a short answer is padded, not refused: the clock matters more than the source's arithmetic
  (let ((short (make-array 40 :element-type '(signed-byte 16) :initial-element 700)))
    (check "a short frame is accepted, and says how short"
           (multiple-value-list (wm:fill-audio-frame pcm (lambda () short))) '(t 40))
    (check-that "its samples landed at the front" (= 700 (aref pcm 0) (aref pcm 39)))
    (check-that "and the rest is silence, not garbage"
                (every #'zerop (subseq pcm 40))))

  ;; a long answer is truncated the same way, and still reports its true length so the sender
  ;; can say so once rather than silently swallowing 20 ms of audio every frame
  (let ((long (make-array 400 :element-type '(signed-byte 16) :initial-element -300)))
    (check "a long frame is accepted, and says how long"
           (multiple-value-list (wm:fill-audio-frame pcm (lambda () long))) '(t 400))
    (check-that "truncated to the frame size" (every (lambda (s) (= s -300)) pcm))))

;;; ---- 2. the tone source ----------------------------------------------------
(format t "~&~%=== the tone source ===~%")
(let ((s (wm:make-tone-source :hz 440d0 :on 1 :period 1)))
  (check-that "on >= period is a continuous tone"
              (loop repeat 10 always (funcall s))))
(let* ((s (wm:make-tone-source :hz 440d0 :on 2 :period 5))
       (pattern (loop repeat 10 collect (if (funcall s) 1 0))))
  (check "a duty cycle beeps and rests" pattern '(1 1 0 0 0 1 1 0 0 0)))
(let* ((s (wm:make-tone-source :hz 440d0 :on 1 :period 3))
       (a (copy-seq (funcall s))))
  (funcall s) (funcall s)                        ; two silent frames pass
  (let ((b (copy-seq (funcall s))))
    ;; phase runs through the gap, so the tone does not restart at zero and click
    (check-that "phase is continuous across the silence"
                (not (equalp a b))
                (format nil "first sample of beep 1 = ~d, of beep 2 = ~d" (aref a 0) (aref b 0)))))

;;; ---- 3. a tone through the whole path --------------------------------------
;;; 5 frames = 800 samples; at 8 kHz that puts 440 Hz exactly on bin 44, so the measurement is
;;; not fighting spectral leakage as well as the codec.
(format t "~&~%=== 440 Hz through encode -> RTP -> SRTP -> decode ===~%")
(multiple-value-bind (got sent) (run-call (wm:make-tone-source :hz 440d0 :on 1 :period 1) 5)
  (check "every frame arrived" (length got) 5)
  (let* ((out (splice got #'rx-frame-pcm))
         (in  (splice sent #'identity))
         (at440 (goertzel out 440d0)))
    (check "800 samples came back" (length out) 800)
    ;; Compare against the bins that actually carry µ-law's distortion — its ODD harmonics.  The
    ;; obvious choice (some quiet bin like 660 Hz) reads as a ratio of 10^14, because a sine
    ;; sitting exactly on a DFT bin leaks nothing into any other exact bin: a true number, and a
    ;; useless one.  It would still read 10^14 with the path badly distorted.  3rd and 5th
    ;; harmonic is where degradation would actually show up, so that is the ratio worth having.
    (let* ((harm (mapcar (lambda (f) (cons f (goertzel out f))) '(1320d0 2200d0)))
           (loudest (reduce #'max harm :key #'cdr)))
      (check-that "440 Hz dominates the distortion it creates"
                  (> at440 (* 100 loudest))
                  (format nil "440 Hz = ~,0f, worst of 3rd/5th harmonic = ~,0f (~,0fx)"
                          at440 loudest (/ at440 (max 1d-9 loudest)))))
    (check-that "and nothing leaked into the bins between"
                (every (lambda (f) (< (goertzel out f) (* 1d-3 at440))) '(220d0 300d0 660d0 880d0))
                "220/300/660/880 Hz all clear")
    (let ((snr (snr-db in out)))
      ;; µ-law is 8 bits of companded amplitude — ~35 dB on a sine is what it gives, and a gate
      ;; that demanded more would be gating the codec's spec, not our path.
      (check-that "the round trip is µ-law-clean, not merely non-empty" (> snr 30d0)
                  (format nil "SNR ~,1f dB" snr)))
    (check-that "and it is not silence" (> (wm:rms-level out) 0.5d0)
                (format nil "rms level ~,2f" (wm:rms-level out)))

    ;; A gate that cannot fail measures nothing, so here is the failure it exists to catch.
    ;; Deliver the same audio at half rate — the byte count is identical, the sequence numbers
    ;; and timestamps are identical, the RMS level is identical, and every liveness check in
    ;; this file still passes.  Only the frequency notices, which is the whole argument for
    ;; measuring one.
    (let ((halved (make-array 800 :element-type '(signed-byte 16))))
      (dotimes (i 800) (setf (aref halved i) (aref out (floor i 2))))
      (check-that "a half-rate path IS caught — the tone moves to 220 Hz"
                  (and (< (goertzel halved 440d0) (* 0.2d0 at440))
                       (> (goertzel halved 220d0) (* 10 (goertzel halved 440d0))))
                  (format nil "440 Hz falls to ~,0f while 220 Hz rises to ~,0f"
                          (goertzel halved 440d0) (goertzel halved 220d0)))
      (check-that "...and every non-frequency check still passes on it, as advertised"
                  (and (= (length halved) (length out))
                       (> (wm:rms-level halved) 0.5d0))
                  (format nil "same 800 samples, rms ~,2f" (wm:rms-level halved))))))

;;; ---- 4. the RTP clock ------------------------------------------------------
(format t "~&~%=== the clock the far side reads ===~%")
(multiple-value-bind (got) (run-call (wm:make-tone-source :hz 440d0 :on 1 :period 1) 6)
  (check "sequence numbers are consecutive" (mapcar (lambda (f) (wm:rtp-seq (rx-frame-rtp f))) got)
         '(0 1 2 3 4 5))
  (check "timestamps advance one frame each" (mapcar (lambda (f) (wm:rtp-timestamp (rx-frame-rtp f))) got)
         '(0 160 320 480 640 800))
  (check "payload type is PCMU" (remove-duplicates (mapcar (lambda (f) (wm:rtp-pt (rx-frame-rtp f))) got))
         '(0))
  (check "only the first frame is marked"
         (mapcar (lambda (f) (and (wm:rtp-marker (rx-frame-rtp f)) t)) got)
         '(t nil nil nil nil nil)))

;;; A lost packet must leave a HOLE in the sequence, not a renumbering.  This is the property a
;;; jitter buffer is entitled to and the reason the sender advances seq/ts even when the send
;;; failed: if a drop silently shifted every later number down, the far side could not tell loss
;;; from reorder, and concealment would have nothing to trigger on.
(multiple-value-bind (got) (run-call (wm:make-tone-source :hz 440d0 :on 1 :period 1) 6 :drop '(2 3))
  (check "loss leaves a hole in the sequence, not a renumbering"
         (mapcar (lambda (f) (wm:rtp-seq (rx-frame-rtp f))) got) '(0 1 4 5))
  (check "and the timestamps keep the real elapsed time"
         (mapcar (lambda (f) (wm:rtp-timestamp (rx-frame-rtp f))) got) '(0 160 640 800)))

;;; ---- 5. the sender's clock -------------------------------------------------
(format t "~&~%=== the 20 ms deadline ===~%")
(let ((period 20))
  (multiple-value-bind (sleep next) (wm::frame-deadline 1000 990 period)
    (check "early: sleep the remainder" sleep 10)
    (check "and the next deadline is one period on" next 1020))
  (multiple-value-bind (sleep next) (wm::frame-deadline 1000 1005 period)
    (check "a little late: do not sleep" sleep 0)
    (check "but keep the ORIGINAL cadence, do not restart it" next 1020))
  (multiple-value-bind (sleep next) (wm::frame-deadline 1000 1200 period)
    ;; more than a frame late — forgive the debt rather than fire 9 frames back to back
    (check "very late: still no sleep" sleep 0)
    (check "and the debt is forgiven, not burst" next 1220)))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
