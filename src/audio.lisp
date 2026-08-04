;;;; audio.lisp — the audio frame, and where one comes from.
;;;;
;;;; Everything above this file (session.lisp) is about getting a frame onto the wire: encode it
;;;; to G.711, pack it into RTP, protect it with SRTP, hand it to ICE.  This file is about the
;;;; frame itself — 20 ms of 8 kHz mono PCM — and about the SOURCE, the thunk the sender asks for
;;;; one every 20 ms.
;;;;
;;;; It lives in the base system rather than the session glue on purpose.  A source produces
;;;; samples; it has nothing to do with ICE, DTLS or a live peer, and neither does checking that
;;;; a tone survives the encode/decode round trip.  Keeping it here means that check needs no
;;;; transport (see inspect/audio-seam.lisp).
;;;;
;;;; The unit is fixed at 160 samples of (signed-byte 16) at 8 kHz because PCMU is: payload type 0
;;;; is 8 kHz mono, and 20 ms is what every RTP stack on the far side expects to pace against.
;;;; A source that wants to speak at 44.1 or 48 kHz has to rate-convert first — there is no
;;;; resampler in this stack, and pretending otherwise here would just move the aliasing.

(in-package #:webrtc-media)

(defconstant +audio-frame-samples+ 160)          ; 20 ms @ 8 kHz
(defparameter *audio-frame-secs* 0.02d0)

(deftype audio-frame () '(simple-array (signed-byte 16) (*)))

(defun make-audio-frame (&optional (n +audio-frame-samples+))
  (make-array n :element-type '(signed-byte 16) :initial-element 0))

(defun rms-level (pcm)
  "RMS magnitude of a (signed-byte 16) frame, normalized to 0.0-1.0.
Full scale here is 8000, not 32767: normal speech sits well below the sample range, and a meter
that only twitches when the peer clips is not a meter."
  (let ((sum 0d0) (n (length pcm)))
    (dotimes (i n) (let ((s (aref pcm i))) (incf sum (* (float s 1d0) s))))
    (min 1d0 (/ (sqrt (/ sum (max 1 n))) 8000d0))))

(defun fill-audio-frame (pcm source)
  "Fill PCM — one +AUDIO-FRAME-SAMPLES+ frame — from SOURCE and say what happened.

SOURCE is a thunk of no arguments returning a sequence of (signed-byte 16) samples at 8 kHz,
or NIL when it has nothing to say.  NIL is not an error and not a gap: the sender fills the
frame with silence and sends it anyway, because the stream's timing is carried by the packets
themselves and a sender that goes quiet stops telling the far side what time it is.

Returns (values VOICED N-SUPPLIED) — VOICED is NIL when the frame is silence.

A short answer is zero-padded and a long one truncated, rather than refused.  The peer reads
20 ms of arrival timing out of every packet, so a source that miscounts should sound wrong for
one frame, not stall the clock; the sender reports the mismatch once instead."
  (fill pcm 0)
  (let ((buf (and source (funcall source))))
    (if (and buf (plusp (length buf)))
        (progn (replace pcm buf) (values t (length buf)))
        (values nil 0))))

(defun frame-deadline (next now period)
  "The sender's clock, as arithmetic: given the deadline NEXT the frame just sent was due at,
the time NOW, and the frame PERIOD (all in internal time units), return (values SLEEP-TICKS
NEXT-DEADLINE).

Sleeping a fixed PERIOD between frames is not a 20 ms clock — encoding and encryption take real
time, so the rate lands slightly under 50 frames/s and the sender drifts steadily behind
whatever is feeding it.  Sleeping to a deadline fixes that.

The one judgement in here: if we are already late past a whole frame, the debt is FORGIVEN
rather than paid.  Firing the missed frames back to back would deliver them as a burst, and a
burst is exactly the jitter the far side's buffer exists to absorb — better to have dropped
them and be on time than to arrive all at once and be sorry."
  (let ((sleep (max 0 (- next now)))
        (n (+ next period)))
    (values sleep (if (< n now) (+ now period) n))))

(defun make-tone-source (&key (hz 880d0) (amplitude 9000) (on 8) (period 30))
  "A SOURCE thunk that beeps: ON frames of an HZ sine out of every PERIOD frames.

This was the bring-up probe, and it is kept for the reason it was useful then — a tone whose
frequency you know is the one signal you can check the far end actually received, as opposed to
merely received bytes for.  ON >= PERIOD gives a continuous tone.

Phase is continuous across the silent frames, so the tone does not click back on.  The returned
vector is reused between calls; FILL-AUDIO-FRAME copies out of it."
  (let ((phase 0d0) (frame 0)
        (dphase (/ (* 2d0 pi hz) 8000d0))
        (pcm (make-audio-frame)))
    (lambda ()
      (let ((beep (< (mod frame (max 1 period)) on)))
        (incf frame)
        (dotimes (i +audio-frame-samples+)
          (setf (aref pcm i) (if beep (round (* amplitude (sin phase))) 0))
          (incf phase dphase)
          (when (> phase (* 2d0 pi)) (decf phase (* 2d0 pi))))
        (and beep pcm)))))
