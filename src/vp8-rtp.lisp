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

;;; ---- fitting a frame to a budget ------------------------------------------
;;;
;;; There are two ways to answer "this change does not fit in the bytes I am allowed":
;;;
;;;   1. code part of it at full quality and defer the rest.  The viewer then holds a picture
;;;      that is partly current and partly minutes old, and the boundary between them moves
;;;      around.  A scroll under this rule crawls: each frame repaints a slice, and the slice is
;;;      smaller the slower the link.  Lowering the bitrate visibly slows the SCREEN down.
;;;
;;;   2. code ALL of it, coarser.  The viewer gets the whole change at once, soft, and the idle
;;;      refinement pass sharpens it as soon as the motion stops.  Lowering the bitrate makes the
;;;      picture blurrier, not slower — which is what a rate control is for.
;;;
;;;   3. and when the change IS the whole screen, send a keyframe.  An inter macroblock cannot
;;;      use intra prediction, so coding four thousand unrelated macroblocks as a delta costs
;;;      about twice what the same picture costs as a keyframe.  The sender compares the two
;;;      estimates and takes the cheaper; on a window switch that is always the keyframe.
;;;
;;; We do (2) and (3), and (1) only survives for the case where even the coarsest quantizer the
;;; format has cannot cover the change inside the ceiling below.  Choosing between quantizers
;;; needs a cost model, and the model is one number times a fixed curve:
;;;
;;;     frame bytes  ~=  overhead  +  translating * MPU  +  new * BPU * (QI-COST qi)
;;;
;;; BPU is learned from the encoder's own output (how expensive THIS screen's content is, in
;;; bytes per macroblock, normalised to qi 63) and QI-COST is the shape of the rate/quantizer
;;; curve, which barely depends on content.  OVERHEAD is the per-macroblock skip flag every inter
;;; frame pays whether it codes anything or not — ~500 bytes at 1280x800, which at the bottom of
;;; the ladder is most of a frame's budget and cannot be left out of the arithmetic.
;;;
;;; THE CHANGED REGION HAS TWO POPULATIONS AND THEY DO NOT COST THE SAME, which is the whole
;;; reason there are two terms.  BPU prices COEFFICIENTS; a macroblock that merely translated
;;; codes none.  It spends a mode and a vector reference — MPU, about a byte, and the same byte at
;;; every quantizer, because a quantizer is a price per coefficient and there are none.
;;;
;;; Splitting them is not a refinement, it is the difference between a drag costing a keyframe and
;;; a drag costing nothing.  Dragging or scrolling a window dirties most of the screen and
;;; translates almost all of what it dirties.  Priced on the dirty count alone — which is what
;;; this model used to do, because BPU was applied to every macroblock the frame was asked to
;;; cover — a drag reads as four thousand macroblocks of fresh content, the estimate comes out
;;; above a keyframe's, and rule (3) fires: ~22 KB, 4.5 s of a 40 kbps link, to say "the window is
;;; 300 pixels to the right".  Worse, the SAME arithmetic chooses the quantizer, so the frames
;;; that were emitted came out needlessly coarse as well.
;;;
;;; The counts come from VP8::PREDICT-MOTION, which runs the encoder's own translation test over a
;;; sample of the changed macroblocks before the frame is coded, and the vector it finds is then
;;; handed to the encoder so the search is paid for once.  So this is still two MEASUREMENTS
;;; compared, not two guesses — and rule (3) still fires for a window switch or a page load, where
;;; there is no vector to find and every macroblock genuinely is new content.

(defparameter *qi-cost*
  '((8 . 2.90d0) (16 . 2.29d0) (24 . 1.92d0) (32 . 1.66d0) (44 . 1.38d0) (56 . 1.16d0)
    (63 . 1.00d0) (72 . 0.85d0) (84 . 0.71d0) (96 . 0.58d0) (108 . 0.45d0) (118 . 0.37d0)
    (127 . 0.32d0))
  "Bytes per coded macroblock at each quantizer index, RELATIVE to qi 63.  Measured: this
encoder's own output on a real 1280x800 desktop capture, 203137 bytes at qi 8 down to 22116 at
qi 127 over 4000 macroblocks.  Only the shape matters here — the scale is BPU, which is learned
per stream — and the shape is a property of the quantizer ladder, not of the picture.")

(defun qi-cost (qi)
  "What a macroblock costs at QI, relative to qi 63.  Linear between the measured points."
  (let ((tbl *qi-cost*) (qi (max 0 (min 127 qi))))
    (cond ((<= qi (car (first tbl))) (cdr (first tbl)))
          ((>= qi (car (car (last tbl)))) (cdr (car (last tbl))))
          (t (loop for (a b) on tbl
                   when (and b (<= (car a) qi) (<= qi (car b)))
                     do (return (+ (cdr a) (* (- (cdr b) (cdr a))
                                              (/ (float (- qi (car a)) 1d0)
                                                 (- (car b) (car a)))))))))))

(defun qi-step-cost (q &optional from)
  "What coding a macroblock at Q costs.  FROM, when given, says the viewer ALREADY holds that
macroblock at that (coarser) quantizer, and then the cost is DIFFERENTIAL — the picture at Q minus
what was already sent at FROM.  That is why the first step down from a coarse keyframe is the
expensive one and every step after it is cheaper, and why a refinement pass modelled with the flat
cost asks for a whole keyframe's worth of bytes every time it runs."
  (if from (max 0.05d0 (- (qi-cost q) (qi-cost from))) (qi-cost q)))

(defun fit-quantizer (n bytes bpu overhead qi max-qi &optional from)
  "The FINEST quantizer at which N macroblocks are predicted to cost no more than BYTES.
Returns (values quantizer fits-p); FITS-P is NIL when even the coarsest quantizer allowed overruns
— the only case that still defers macroblocks.  FROM has the meaning it has in QI-STEP-COST, and
bounds the search: a refinement that came out coarser than what is already on screen is not one."
  (let ((room (- bytes overhead))
        (top (if from (min max-qi (1- from)) max-qi)))
    (cond ((<= n 0) (values qi t))
          ((or (<= room 0) (< top qi)) (values (or from max-qi) nil))
          (t (loop for q from qi to top
                   when (<= (* n bpu (qi-step-cost q from)) room) do (return (values q t))
                   finally (return (values top nil)))))))

(defun mbs-fitting (bytes bpu overhead q &optional from)
  "How many macroblocks fit in BYTES at quantizer Q.  Floored at 8: a frame that codes nothing
still pays its skip flags, so emitting one is pure loss."
  (max 8 (floor (max 0 (- bytes overhead)) (max 0.5d0 (* bpu (qi-step-cost q from))))))

(defparameter *frame-coverage-x* 4
  "The LATENCY ceiling on one frame, as a multiple of the rung's MAX-FRAME-KB (1.5 s of link time)
— ~6 s in flight at every rung.  Only past this does anything defer.

This used to be the ceiling every frame was fitted to, and that is what made the picture stand
still.  A ceiling stated in SECONDS OF LINK gives one frame every few seconds at EVERY rung: raise
the rate and the same cadence buys a bigger, sharper frame, which is exactly the complaint that
320 kbps looks like 40 kbps with better letters.  A live frame is now sized from a FRAME RATE (see
MOTION-FRAME-BUDGET) and this stays as two things only: the ceiling for frames with nothing
waiting behind them — a refinement, a deferred-macroblock drain, both of which run while the
screen is idle — and the backstop a live frame is allowed when its change cannot be covered inside
the frame-rate budget at any quantizer (see FIT-FRAME).")

(defparameter *frame-overhead-x* 3
  "The smallest a per-frame budget may be, as a multiple of the frame's FIXED overhead.

Every inter frame spends a skip flag per macroblock whether it codes anything or not: 64 bytes of
header plus one bit per macroblock, ~564 bytes at 1280x800.  Asking for a frame rate whose budget
approaches that does not buy motion, it buys flags — at 40 kbps, ten frames a second would be 500
byte frames of which 96% is the flags and nothing is left for a picture.  So the requested rate is
clamped to what the overhead permits (FRAME-FPS-CAP) and the clamp is REPORTED, because a rung
sitting on it is a rung where the RESOLUTION, not the bitrate, is what stands between the viewer
and fluid motion.")

(defun frame-fps-cap (bytes-per-sec overhead)
  "The most frames per second BYTES-PER-SEC can carry at this resolution before the skip flags are
more than 1/*FRAME-OVERHEAD-X* of every frame.  A property of the RATE and the RESOLUTION and
nothing else — no content, no quantizer — which is why it is the honest ceiling to quote: at
1280x800 it is ~1.5 fps at 20 kbps, ~3 at 40, ~6 at 80, ~12 at 160 and ~24 at 320."
  (if (plusp overhead)
      (/ (float bytes-per-sec 1d0) (* *frame-overhead-x* (float overhead 1d0)))
      60d0))

(defun motion-frame-budget (bytes-per-sec target-fps overhead)
  "The bytes a LIVE frame is allowed: a second of the link divided by the frame rate we are trying
to hold, rather than a fixed slice of link time.  Returns (values budget effective-fps cap).

THIS IS THE RATE/MOTION TRADE, and it is the whole point of the number.  Sized in link-seconds, a
frame is the same NUMBER OF SECONDS at every rung, so more bitrate is spent entirely on making
that one frame sharper and the screen updates just as rarely at 320 kbps as at 40.  Sized from a
frame rate, the extra bytes go into extra FRAMES, and how sharp a frame of that size can be is
left where it belongs — with the quantizer fit, which is unchanged and still covers the whole
change rather than part of it."
  (let* ((cap (frame-fps-cap bytes-per-sec overhead))
         (eff (max 0.1d0 (min (float (or target-fps 10) 1d0) cap))))
    (values (max 256 (floor bytes-per-sec eff)) eff cap)))

(defun fit-frame (n bpu overhead qi max-qi budget ceiling)
  "Fit N macroblocks into BUDGET if they can be covered there, and into CEILING if they cannot.
Returns (values quantizer allowed-bytes fits-p).

THE BUDGET IS A TARGET AND COVERAGE IS A CONSTRAINT, in that order.  BUDGET is the size that holds
the frame rate; when even MAX-QI overruns it the answer is NOT to code a slice and defer the rest
— that is the half-current, half-stale patchwork this sender spent two changes getting rid of — it
is to code the whole change at the coarsest quantizer allowed and spend up to CEILING, the latency
bound.  So motion is bought where motion is cheap, which is where it actually lives: a scroll or a
drag is mostly translation, its frames are small, and it gets many of them.  A change that is
genuinely new content still arrives whole, in one slow frame, exactly as it did before."
  (multiple-value-bind (q ok) (fit-quantizer n budget bpu overhead qi max-qi)
    (if ok
        (values q budget t)
        (values q ceiling (<= (* n bpu (qi-cost q)) (max 0 (- ceiling overhead)))))))

;;; ---- switching quality under a running sender ------------------------------
;;; Every knob START-VIDEO takes is consulted on each pass of the encode loop, not baked into the
;;; frame pipeline at startup — so a profile can be swapped while the stream runs.  That is the
;;; whole point: restarting the sender would restart its RTP sequence and timestamp, which a
;;; receiver reads as a different stream, and the picture stops for as long as it takes to
;;; resynchronise.  Comparing two profiles by feel is impossible if switching costs you the frame.
;;;
;;; Bind or SETF this to a plist of any of (:qi :max-qi :target-kbs :max-frame-kb :cleanup-ms);
;;; whatever it names overrides the corresponding argument from the next pass onward, and NIL
;;; leaves the sender on the values it was started with.
(defvar *video-profile* nil)

;; Set by anything that knows the decoder needs a fresh start — the phone asking after it comes back
;; from the background, say.  The sender clears it when the keyframe actually goes out, so a request
;; costs exactly one keyframe however many times it is set in the meantime.  Same mechanism as
;; *VIDEO-PROFILE*: a special the loop re-reads each pass, so nothing has to reach into the thread.
(defvar *force-keyframe* nil)
(defvar *video-profile-applied* nil)      ; last plist logged, so a switch is reported once

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
                                           (ssrc #x5e5e0001) (qi 12) (key-secs 30.0) (poll-secs 0.010)
                                           (max-qi 44) (target-kbs 150) (cleanup-ms 700)
                                           (max-frame-kb 32) (capture-secs 0.040) (target-fps 10)
                                           key-qi source log on-stats sink)
  "Stream VP8 to the peer over SRTP.  CTX is the SRTP context (shared with audio — same DTLS keys,
different SSRC).  SOURCE, when given, is a thunk returning (values Y U V w h dirty) for the next
frame or NIL if nothing changed since last time — so an idle screen costs no bandwidth.  Without a
SOURCE it sends the built-in test pattern.

THREE CLOCKS, DELIBERATELY SEPARATE.  The desktop is LOOKED AT every CAPTURE-SECS; a frame is
ENCODED whenever the link is free; packets are SENT at TARGET-KBS.  They used to be one clock —
the pacing sleep ran in the same loop that sampled the screen — so lowering the rate lowered the
CAPTURE rate with it, and at 40 kbps the desktop was sampled about once a second.  That is most of
why 'reducing the rate slows page updates down': the sender was not looking.

KEYFRAMES ARE THE EXPENSIVE THING, and on a slow link they are the ONLY thing that matters.  A
1280x800 keyframe is ~22 KB even at the coarsest quantizer the format has — 35 s of channel time
at 5 kbps, 4 s at 40 kbps.  So we send as few as we possibly can: one at session start, one
whenever the frame size changes, and thereafter only a wall-clock safety net every KEY-SECS for a
receiver that missed the first one.  KEY-SECS is meant to be derived from TARGET-KBS by the
caller, so the resync costs a fixed FRACTION of the link rather than a fixed number of seconds;
at 5 kbps a 30 s resync would be permanent saturation.  Everything else is an inter frame, where
an unchanged macroblock costs a single bit.

KEY-QI, when given, is the quantizer keyframes are coded at, independently of QI.  Coding the
keyframe coarser than the stream's own quality is how the first image ARRIVES AT ALL on a slow
link: the whole screen lands quickly but soft, every macroblock is then marked stale, and the
idle-refinement pass sharpens it to QI progressively over the following frames.

INTER FRAMES NOW WORK THE SAME WAY, which is the change the rest of this file exists for.  A
frame's quantizer is CHOSEN so that the whole dirty region fits the byte budget (FIT-QUANTIZER),
instead of the budget being spent on as many macroblocks as fit at a fixed quality and the
remainder deferred.  So a change arrives coarse and COMPLETE, and gets sharper while the screen
is quiet.  Deferral is still there, but only for the case the quantizer ladder cannot cover: the
whole screen changed and even MAX-QI overruns *FRAME-COVERAGE-X* frames' worth of budget.

AND THE BUDGET IS A FRAME RATE, which is what decides whether more bitrate buys MOTION or only
SHARPNESS.  TARGET-FPS is the cadence we are trying to hold; a live frame gets a second of the
link divided by it (MOTION-FRAME-BUDGET), so at 320 kbps a frame is a few kilobytes and there are
several a second, where a budget stated in link-seconds gave one big sharp frame every couple of
seconds at every rung alike.  The rate is CLAMPED by what the per-macroblock skip flags permit —
below that clamp a frame is mostly flags and no picture — and both the request and the clamp are
reported, so a rung sitting on its ceiling is visible as such.  Frames with nothing waiting behind
them (refinement, deferred drain) keep the old link-time ceiling: they are idle-time work, and
shrinking them buys nobody anything.

Quality adapts to the link: QI is the best (sharpest) quantizer, MAX-QI the worst we will fall
back to, and TARGET-KBS the rate the pacer emits at.  The quantizer controller is driven by the
ENCODER'S OWN OUTPUT against the frame budget, measured BEFORE pacing — the old one compared
post-pacing throughput with the target, which the pacer produces by construction, so it read
'on target' for ever and never coarsened at all.  Then, once the screen has been quiet for
CLEANUP-MS, whatever was coded coarsely is re-coded finer, a whole screen at a time.

SINK, when given, replaces the SRTP send with (funcall SINK frame) once per frame, after that
frame's packets have been paced — so a measurement harness sees exactly what a receiver would
have reassembled, when it would have had it.  Returns a STOP thunk."
  (declare (ignore session))
  (let* ((running t)
         ;; ---- the send queue: the ONLY thing that sleeps at the link rate ----
         (queue '()) (pacing nil) (qlock (bt:make-lock "wm-video-pace"))
         ;; ---- pipeline timing, shared by both threads ----
         (st-take (make-pstat :name "take")) (st-enc (make-pstat :name "encode"))
         (st-send (make-pstat :name "send")) (st-loop (make-pstat :name "loop"))
         ;; the adaptive path gets its own counters: a refinement is a different animal from a
         ;; live frame, and per-FRAME size matters as much as average rate — one 90 KB frame is
         ;; ~360 ms of transmission on a 250 KB/s link, which reads as a freeze no matter what
         ;; the 5-second average says.
         (st-clean (make-pstat :name "refine")) (st-kb (make-pstat :name "frameKB"))
         (st-mbs (make-pstat :name "codedMB")) (st-qi (make-pstat :name "qi"))
         ;; macroblocks the frame's motion vector carried outright.  Worth its own counter: it is
         ;; the difference between "the screen changed" and "the screen moved", and without it a
         ;; drag and a window switch look identical in every number the sender reports.
         (st-mv (make-pstat :name "movedMB"))
         (st-lag (make-pstat :name "lag"))
         (sent 0) (bytes 0) (skipped 0) (takes 0))
    (labels ((link-busy-p ()
               "Is anything still on its way out?  The encoder waits for this rather than for a
sleep, which is what keeps the capture clock independent of the link rate."
               (bt:with-lock-held (qlock) (or queue pacing)))
             (enqueue (frame ts)
               (bt:with-lock-held (qlock) (setf queue (nconc queue (list (cons frame ts)))))))
      ;; ---- the pacer -----------------------------------------------------------------------
      ;; A keyframe is ~75 packets; handing them all to the socket at once is a microburst that
      ;; overruns a queue somewhere on the relay path, and a VP8 keyframe missing ONE fragment is
      ;; undecodable — which looks exactly like "connected, receiving, no first frame".  So every
      ;; frame is spread over the time its bytes are worth at TARGET-KBS.  Frames are never
      ;; dropped: an inter frame is a delta, and skipping one desynchronises the decoder until the
      ;; next keyframe.  The queue holds at most the frame being sent plus one, and the encoder
      ;; simply does not run while it is occupied — that is the backpressure, and it bounds
      ;; latency to one frame without touching how often the desktop is sampled.
      (bt:make-thread
       (lambda ()
         (let ((seq 0))
           (loop
             (let ((item (bt:with-lock-held (qlock)
                           (let ((i (pop queue))) (setf pacing (and i t)) i))))
               (cond
                 (item
                  (handler-case
                      (let ((frame (car item)) (fts (cdr item)))
                        (with-timing (st-send)
                          (multiple-value-bind (pkts next) (vp8-packetize frame seq fts ssrc pt)
                            (setf seq next)
                            ;; the gap is derived from the frame's ACTUAL bytes rather than a whole
                            ;; packet each, so a 700-byte frame costs 700 bytes of time, not 1100
                            (let ((gap (and (plusp (length pkts))
                                            (/ (length frame)
                                               (* (length pkts) 1024d0 target-kbs)))))
                              (dolist (p pkts)
                                (unless sink
                                  (webrtc-data:ice-send-to-peer agent (srtp-protect ctx p)))
                                (when gap (sleep gap)))
                              (when sink (funcall sink frame))))))
                    (error (e) (when log (funcall log (format nil "video pace: ~a" e)))))
                  (bt:with-lock-held (qlock) (setf pacing nil)))
                 ((not running) (return))
                 (t (sleep 0.002)))))))
       :name "wm-video-pace")
      ;; ---- capture and encode --------------------------------------------------------------
      (bt:make-thread
       (lambda ()
         (let ((seq-ts 0) (n 0) (enc nil)
               (last-report (get-internal-real-time)) (last-tick (get-internal-real-time))
               (last-take 0)
               (idle-since (get-internal-real-time)) (cleanups 0)
               ;; the newest desktop we have been handed but not yet coded, and the UNION of
               ;; everything that changed since the last frame went out.  Sampling faster than we
               ;; can encode is the point: what gets coded is then the LATEST screen, with the
               ;; whole accumulated change, instead of a queue of stale ones.
               (cap-y nil) (cap-u nil) (cap-v nil) (cap-w nil) (cap-h nil)
               (cap-dirty nil) (cap-scan nil)
               ;; macroblocks that wanted coding and did not fit even at MAX-QI — rare now, and
               ;; genuinely a fallback rather than the normal path
               (pending nil)
               ;; the cost model (see FIT-QUANTIZER), in bytes per macroblock at qi 63.  Three
               ;; kinds of frame, three estimates, because they are not the same animal:
               ;;   BPU  live inter content.  Measured at ~35 — about twice the intra cost,
               ;;        because an inter macroblock cannot use intra prediction and a screen
               ;;        that changed completely gives it nothing to predict from.
               ;;   RPU  refinement.  Its cost is differential (see QI-STEP-COST), so this is
               ;;        on the same scale as KPU: the FULL price of the picture per macroblock,
               ;;        of which a refinement pays only the part not already sent.
               ;;   KPU  intra.  Measured: 22116 bytes for 4000 macroblocks at qi 127, i.e.
               ;;        17.5 at qi 63 — the keyframe cost table this whole ladder is built on.
               ;;   MPU  a translating macroblock: a mode and a vector reference, no coefficients.
               ;;        NOT normalised to a quantizer, because it does not have one — this is the
               ;;        one term in the model that a coarser frame does not make cheaper.
               (bpu 35d0) (rpu 17.5d0) (kpu 17.5d0) (mpu 1.0d0)
               ;; the frame-rate plan the last pass computed, kept out here only so the periodic
               ;; report can quote it: what we asked for, what the skip flags allow, and the byte
               ;; size those two produce
               (fps-want 0d0) (fps-cap 0d0) (frame-budget 0)
               (cur-qi qi) (qi-bias 0) (stale-qi qi)
               ;; RESYNC ON A WALL CLOCK, not on source activity, and at a RATE-SCALED interval.
               ;; A keyframe is ~22 KB whatever we do to it, so at 40 kbps a 3 s resync is 16 s of
               ;; channel time per 3 s of wall clock.  That is not "overhead", it is a stream that
               ;; can never catch up.  KEY-SECS carries the caller's budget for this; the viewer
               ;; asks explicitly when it actually needs one.
               (last-key 0))
           (loop while running do
             ;; adopt a profile switched under us — see *VIDEO-PROFILE*.  Every knob below is read
             ;; again on the next pass, so this is the entire cost of switching quality live: no
             ;; restart, no new keyframe, no gap in the sequence the receiver is following.
             ;;
             ;; BEFORE the frame, not after it.  The rung is chosen at load time, long before any
             ;; session exists, so adopting it at the END of the pass meant the very first frame —
             ;; the keyframe, the single most expensive thing on the whole ladder — was coded at
             ;; START-VIDEO's arguments instead of the rung's.
             (let ((p *video-profile*))
               (when (consp p)
                 (setf qi           (or (getf p :qi) qi)
                       max-qi       (or (getf p :max-qi) max-qi)
                       target-kbs   (or (getf p :target-kbs) target-kbs)
                       max-frame-kb (or (getf p :max-frame-kb) max-frame-kb)
                       cleanup-ms   (or (getf p :cleanup-ms) cleanup-ms)
                       key-qi       (or (getf p :key-qi) key-qi)
                       key-secs     (or (getf p :key-secs) key-secs)
                       target-fps   (or (getf p :target-fps) target-fps)
                       cur-qi       (min (max cur-qi qi) max-qi))
                 (unless (equal p *video-profile-applied*)
                   (setf *video-profile-applied* p)
                   (when log
                     (funcall log (format nil "profile ~@[~a ~]applied live: qi ~a/~a key-qi ~a target ~,2f KB/s ~,1f fps frame<=~,1f KB cleanup ~a ms resync ~,0f s"
                                          (getf p :name) qi max-qi (or key-qi qi) target-kbs
                                          target-fps max-frame-kb cleanup-ms key-secs))))))
             (handler-case
                 (progn
                   ;; ---- 1. LOOK AT THE DESKTOP, on the capture clock ----------------------
                   ;; This happens whether or not the link is free.  A source that reports
                   ;; nothing changed is cheap, and one that does hands us the newest pixels;
                   ;; either way the rate the link runs at has no say in it.
                   (let ((now (get-internal-real-time)))
                     (when (and source
                                (>= (/ (float (- now last-take)) internal-time-units-per-second)
                                    capture-secs))
                       (setf last-take now)
                       (multiple-value-bind (y u v w h dirty)
                           (with-timing (st-take) (funcall source))
                         (when y
                           (incf takes)
                           (setf cap-y y cap-u u cap-v v cap-w w cap-h h
                                 idle-since now)
                           ;; the dirty sets ACCUMULATE: several screen updates may land between
                           ;; two frames, and the one we code has to cover all of them
                           (if dirty
                               (if cap-dirty
                                   (bit-ior cap-dirty dirty cap-dirty)
                                   (setf cap-dirty (copy-seq dirty)))
                               (setf cap-scan t))))))
                   ;; ---- 2. ENCODE, but only when the link is free -------------------------
                   (when (not (link-busy-p))
                     (let* ((y cap-y) (u cap-u) (v cap-v) (w cap-w) (h cap-h)
                            (dirty (if cap-scan nil cap-dirty))
                            ;; PROG1, not (and flag (setf flag nil) t): SETF returns what it
                            ;; stored, so the AND short-circuits on the NIL and FORCED is never
                            ;; true — the request is consumed and silently dropped.
                            (forced (prog1 *force-keyframe* (setf *force-keyframe* nil)))
                            ;; the quantizer a KEYFRAME is coded at — coarser than the stream's
                            ;; own, so the first whole picture arrives in seconds not tens of them
                            (kqi (max qi (or key-qi qi)))
                            ;; every macroblock of a keyframe coded above QI is left stale on
                            ;; purpose: that is the handoff to the refinement pass
                            (kstale (if (> kqi qi) 1 0))
                            (key-due (> (/ (float (- (get-internal-real-time) last-key))
                                           internal-time-units-per-second)
                                        key-secs))
                            (mbs (if enc (* (vp8::ve-mb-cols enc) (vp8::ve-mb-rows enc)) 0))
                            ;; the skip flag every macroblock pays whether it codes or not
                            (overhead (+ 64 (floor mbs 8)))
                            ;; the LATENCY ceiling, in seconds of link — what a frame with nothing
                            ;; waiting behind it may cost, and the backstop for one that cannot be
                            ;; covered inside the frame-rate budget below
                            (link-budget (floor (* max-frame-kb 1024)))
                            (link-ceiling (* *frame-coverage-x* link-budget))
                            ;; THE FRAME-RATE BUDGET: a second of the link divided by the cadence
                            ;; we are trying to hold, clamped to what the skip flags leave room
                            ;; for.  This is the number that decides whether the next rung up buys
                            ;; more pictures or just a better one.
                            (bps (* target-kbs 1024))
                            (plan (multiple-value-list
                                   (motion-frame-budget bps target-fps overhead)))
                            (budget (first plan)) (feff (second plan)) (fcap (third plan))
                            ;; what this frame is being asked to cover, and the finest quantizer
                            ;; that covers ALL of it inside the ceiling
                            (want (cond ((null pending) dirty)
                                        ((null dirty) pending)
                                        (t (bit-ior pending dirty))))
                            (nwant (if want (count 1 want) mbs))
                            ;; HOW MUCH OF THE CHANGE IS MERELY A TRANSLATION.  Asked before the
                            ;; frame is coded, because it is what decides HOW to code it — and
                            ;; asked of the encoder's own translation test rather than guessed, so
                            ;; the two terms below are both measurements.  The vector goes back
                            ;; into the encode below, so the search happens once per frame.
                            (mvp (when (and enc y)
                                   (multiple-value-list
                                    (vp8::predict-motion enc y u v :dirty want))))
                            (gmv (first mvp))
                            (nmv (if gmv (min nwant (or (second mvp) 0)) 0))
                            (nnew (max 0 (- nwant nmv)))
                            ;; the translating part costs the same at every quantizer, so it is
                            ;; not something FIT-QUANTIZER can trade against sharpness: it comes
                            ;; off the top, exactly like the skip flags do.
                            (mv-bytes (* nmv mpu))
                            (live-overhead (+ overhead mv-bytes))
                            ;; A BUDGET MAY NOT REFUSE THE MOTION ITSELF.  The frame's unavoidable
                            ;; cost is its skip flags plus a mode and a vector reference for every
                            ;; macroblock that merely moved, and none of that gets cheaper at a
                            ;; coarser quantizer.  A budget under it would leave FIT-QUANTIZER no
                            ;; room at all and defer the whole change — deferring the very thing
                            ;; the frame exists to carry.  So the frame always gets what it cannot
                            ;; avoid plus half a budget to code with, and the frame rate that
                            ;; implies is the honest one for this much screen in motion.
                            (mo-ceiling (min link-ceiling
                                             (max budget
                                                  (+ (ceiling live-overhead) (floor budget 2)))))
                            (fit (multiple-value-list
                                  (fit-frame nnew bpu live-overhead qi max-qi
                                             mo-ceiling link-ceiling)))
                            (fq (first fit))
                            (ceiling-bytes (second fit))
                            (fits (third fit))
                            (useq (max qi (min max-qi (+ fq qi-bias))))
                            ;; WHEN THE WHOLE SCREEN CHANGED, A KEYFRAME IS THE CHEAP WAY TO
                            ;; COVER IT.  An inter macroblock cannot use intra prediction, so
                            ;; coding 4000 unrelated macroblocks as a delta costs about twice
                            ;; what the same picture costs as a keyframe — and the keyframe
                            ;; needs no reference, so it also cannot inherit anyone's drift.
                            ;;
                            ;; UNRELATED is the word doing the work.  Four thousand macroblocks
                            ;; that all moved together are not unrelated and not expensive: they
                            ;; are the case a keyframe handles WORST, since it re-sends as new
                            ;; content the very pixels the viewer already holds.  So the inter
                            ;; estimate prices the two populations separately (see the model at
                            ;; the top of this file) and a translation can no longer talk itself
                            ;; into a keyframe just by being large.
                            ;;
                            ;; AND THE FIRST TEST IS STRUCTURAL, NOT AN ESTIMATE.  When the whole
                            ;; screen changed and almost none of it translated, the inter frame
                            ;; must re-code every macroblock with no intra prediction to help it —
                            ;; that is the case a keyframe wins BY CONSTRUCTION, and it needs no
                            ;; arithmetic.  Which is just as well, because BPU is the one number
                            ;; that cannot be trusted here: it is learned from live frames, and a
                            ;; drag's frames are almost all translation with a thin residual, so a
                            ;; few seconds of dragging teaches it that a macroblock costs about
                            ;; nothing.  Apply that to a screenful of genuinely new content and the
                            ;; estimate comes out under a keyframe's when the frame is really twice
                            ;; it — measured at 320 kbps: a 177 KB inter frame, 4.5 s of link, in
                            ;; place of a 105 KB keyframe that was sharper.
                            (key-cheaper (and enc y (> nwant (* 0.8 mbs))
                                              (or (< nmv (floor nwant 4))
                                                  (> (+ mv-bytes (* nnew bpu (qi-cost useq)))
                                                     (* mbs kpu (qi-cost kqi))))))
                            (need-key (or (null enc) forced key-due key-cheaper))
                            ;; A KEYFRAME SENT BECAUSE THE SCREEN IS MOVING IS NOT THE SAME FRAME
                            ;; AS ONE SENT BECAUSE THE SESSION STARTED, and until now they were
                            ;; coded identically.  KEY-QI is chosen for the FIRST PICTURE — the
                            ;; sharpest that lands in a couple of seconds — which is right for a
                            ;; frame the viewer will hold while it reads.  A keyframe sent because
                            ;; the screen is SCROLLING is held for one frame time and then
                            ;; replaced, so its budget is the frame rate's, not the first
                            ;; picture's: at 320 kbps that is 22 KB at qi 127, 0.55 s of link,
                            ;; instead of 105 KB at qi 44 and 2.7 s.
                            ;;
                            ;; This is the frame the fps budget could not otherwise reach, and on
                            ;; a full-screen scroll it is the ONLY frame there is.  A scroll that
                            ;; outruns the motion search (the displacement between two frames is
                            ;; past the format's +-255) has no vector, so every frame of it is a
                            ;; whole-screen recode priced as a keyframe — and a keyframe sized for
                            ;; a first picture is what pinned that scroll to one frame every three
                            ;; seconds however much bandwidth it was given.  Coarse and moving,
                            ;; then sharp when it stops: the trade the rest of this sender already
                            ;; makes, applied to the one frame still exempt from it.  A RESYNC
                            ;; keyframe is not this frame and keeps KEY-QI — it exists precisely
                            ;; for a viewer that has nothing, and handing it the coarsest picture
                            ;; the format has would be answering the wrong question.
                            (mkqi (if (and enc key-cheaper (not forced) (not key-due))
                                      (fit-quantizer mbs budget kpu overhead kqi 127)
                                      kqi))
                            (mkstale (if (> mkqi qi) 1 0))
                            (frame
                              (cond
                                ;; a keyframe is owed and the desktop is idle: re-encode one from
                                ;; the last source we saw, so a late receiver can start decoding
                                ((and (null y) source enc (vp8::ve-have-ref enc)
                                      (or forced key-due))
                                 (setf last-key (get-internal-real-time) pending nil
                                       cur-qi kqi stale-qi (if (plusp kstale) kqi qi))
                                 (let ((py (vp8::ve-prev-y enc)) (pu (vp8::ve-prev-u enc))
                                       (pv (vp8::ve-prev-v enc))
                                       (w (vp8::ve-width enc)) (h (vp8::ve-height enc)))
                                   (setf enc (vp8::make-encoder w h))
                                   (multiple-value-bind (f ry ru rv)
                                       (with-timing (st-enc)
                                         (vp8:encode-gray-frame py w h :qi kqi :u pu :v pv))
                                     (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru
                                           (vp8::ve-ref-v enc) rv)
                                     (replace (vp8::ve-prev-y enc) py)
                                     (replace (vp8::ve-prev-u enc) pu)
                                     (replace (vp8::ve-prev-v enc) pv)
                                     (setf (vp8::ve-have-ref enc) t (vp8::ve-stale-mbs enc)
                                           (make-array (* (vp8::ve-mb-cols enc)
                                                          (vp8::ve-mb-rows enc))
                                                       :element-type 'bit :initial-element kstale))
                                     (let ((kn (* (vp8::ve-mb-cols enc) (vp8::ve-mb-rows enc))))
                                       (when (plusp kn)
                                         (setf kpu (/ (length f) kn (qi-cost kqi)))))
                                     f)))
                                ((null y)
                                 (if (null source)                 ; built-in test pattern
                                     (if (evenp (floor n 3))
                                         (vp8::encode-keyframe-gray width height)
                                         (vp8::encode-keyframe-checker width height :phase 1))
                                     ;; Nothing new from the desktop.  Two jobs can still be owed,
                                     ;; in this order:
                                     ;;   1. deferred macroblocks — content the viewer has NOT
                                     ;;      been shown because it did not fit.  This must drain
                                     ;;      here: once the drag stops there are no more source
                                     ;;      frames, and without this the un-sent remainder never
                                     ;;      arrives at all.
                                     ;;   2. refinement — re-code coarsely-coded macroblocks
                                     ;;      finer, once the screen has been quiet a moment.
                                     (let ((st (and enc (vp8::ve-stale-mbs enc))))
                                       (cond
                                         ;; NOTE the ceiling: both jobs below run while the desktop
                                         ;; is IDLE, so nothing is waiting behind them and there is
                                         ;; no motion for a smaller frame to buy.  They keep the
                                         ;; link-time ceiling; only LIVE frames are sized from the
                                         ;; frame rate.
                                         ((and enc pending (position 1 pending))
                                          (let* ((ns (count 1 pending)))
                                            (multiple-value-bind (dq fits)
                                                (fit-quantizer ns link-ceiling bpu overhead
                                                               qi max-qi)
                                              (multiple-value-bind (f nmb defer mv eqi)
                                                  (with-timing (st-enc)
                                                    (vp8::encode-inter-frame
                                                     enc (vp8::ve-prev-y enc) (vp8::ve-prev-u enc)
                                                     (vp8::ve-prev-v enc)
                                                     :qi dq :dirty pending :force t :clean-qi qi
                                                     :max-mbs (unless fits
                                                                (mbs-fitting link-ceiling bpu
                                                                             overhead dq))))
                                                (declare (ignore mv))
                                                (setf pending defer)
                                                (when eqi
                                                  (setf cur-qi eqi
                                                        stale-qi (if (> eqi qi)
                                                                     (max stale-qi eqi) stale-qi)))
                                                (when nmb (ps-note st-mbs nmb))
                                                f))))
                                         ((and st (position 1 st)
                                               (> (/ (* 1000d0 (- (get-internal-real-time)
                                                                  idle-since))
                                                     internal-time-units-per-second)
                                                  cleanup-ms))
                                          (incf cleanups)
                                          ;; REFINE THE WHOLE STALE SET A STEP FINER whenever a
                                          ;; step fits — the picture then improves everywhere at
                                          ;; once and keeps improving, which is the second half of
                                          ;; "coarse but complete, then sharp".  Only when no step
                                          ;; would be an improvement do we fall back to sharpening
                                          ;; a slice at the target quality.
                                          ;;
                                          ;; HALF the ceiling a live frame gets, because a
                                          ;; refinement is the one frame that must not be in the
                                          ;; way: it is already committed to the link when the
                                          ;; desktop changes, and whatever it costs is added to
                                          ;; how long the NEXT real change waits.  Measured at
                                          ;; 160 kbps: a 100 KB refinement held the link for 5 s
                                          ;; across a window switch the sender already knew
                                          ;; about.  Smaller steps, more of them.
                                          (let* ((ns (count 1 st))
                                                 ;; what the viewer already has, when it has
                                                 ;; anything: a macroblock stale at QI itself was
                                                 ;; never sent at all, and costs full price
                                                 (from (and (> stale-qi qi) stale-qi)))
                                            (multiple-value-bind (rq rfits)
                                                (fit-quantizer ns (floor link-ceiling 2) rpu
                                                               overhead qi max-qi from)
                                              (let* ((whole (and rfits (<= rq (- stale-qi 2))))
                                                     (useq (if whole rq qi))
                                                     (cap (unless whole
                                                            (mbs-fitting link-budget rpu overhead
                                                                         qi from))))
                                                (setf cur-qi useq)
                                                (when whole (setf stale-qi rq))
                                                (ps-note st-mbs (if cap (min ns cap) ns))
                                                (multiple-value-bind (f nmb)
                                                    (with-timing (st-clean)
                                                      (vp8::encode-inter-frame
                                                       enc (vp8::ve-prev-y enc)
                                                       (vp8::ve-prev-u enc) (vp8::ve-prev-v enc)
                                                       :qi useq :dirty (copy-seq st) :force t
                                                       :clean-qi qi :max-mbs cap))
                                                  ;; refinement pays for its own estimate: its
                                                  ;; residual is nothing like a live frame's
                                                  (when (and f nmb (>= nmb 16))
                                                    (let ((m (/ (max 1d0 (- (length f) overhead))
                                                                nmb
                                                                (qi-step-cost useq from))))
                                                      (setf rpu (if (> m rpu)
                                                                    (+ (* 0.4d0 rpu) (* 0.6d0 m))
                                                                    (+ (* 0.85d0 rpu)
                                                                       (* 0.15d0 m))))))
                                                  f)))))))))
                                ;; first frame, or a periodic refresh: keyframe, and it seeds the
                                ;; reference the following inter frames predict from
                                ((or need-key (/= w (vp8::ve-width enc))
                                     (/= h (vp8::ve-height enc)))
                                 ;; LAST-KEY was never set here, only in the idle branch — so a
                                 ;; session that started while the desktop was busy measured its
                                 ;; resync age from time zero and considered a keyframe overdue
                                 ;; for ever after.
                                 ;; MKQI, not KQI: a keyframe the CONTENT asked for is sized by the
                                 ;; frame rate, a resync or a first picture by KEY-QI.  See MKQI.
                                 (setf last-key (get-internal-real-time) pending nil
                                       cur-qi mkqi stale-qi (if (plusp mkstale) mkqi qi))
                                 (setf enc (vp8::make-encoder w h))
                                 (multiple-value-bind (f ry ru rv)
                                     (with-timing (st-enc)
                                       (vp8:encode-gray-frame y w h :qi mkqi :u u :v v))
                                   (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru
                                         (vp8::ve-ref-v enc) rv)
                                   (replace (vp8::ve-prev-y enc) y)
                                   (replace (vp8::ve-prev-u enc) u)
                                   (replace (vp8::ve-prev-v enc) v)
                                   (setf (vp8::ve-have-ref enc) t
                                         (vp8::ve-stale-mbs enc)
                                         (make-array (* (vp8::ve-mb-cols enc)
                                                        (vp8::ve-mb-rows enc))
                                                     :element-type 'bit :initial-element mkstale))
                                   (let ((kn (* (vp8::ve-mb-cols enc) (vp8::ve-mb-rows enc))))
                                       (when (plusp kn)
                                         (setf kpu (/ (length f) kn (qi-cost mkqi)))))
                                   f))
                                ;; otherwise an inter frame, and this is the whole point: pick the
                                ;; quantizer the CHANGE fits in, and code all of it.
                                (t
                                 (let ((useq useq)
                                       ;; deferral survives for exactly one case: even MAX-QI
                                       ;; overruns the ceiling, so code what fits and carry the
                                       ;; rest.  On this ladder that is now rare rather than the
                                       ;; normal path.  MAX-MBS bounds macroblocks that CODE, so
                                       ;; the translating part is charged to the overhead here
                                       ;; too and is not rationed.
                                       (cap (unless fits
                                              (mbs-fitting ceiling-bytes bpu live-overhead useq))))
                                   (multiple-value-bind (f nmb defer mv eqi nmoved)
                                           (with-timing (st-enc)
                                             (vp8::encode-inter-frame
                                              enc y u v :qi useq :dirty want :force t
                                              :clean-qi qi :max-mbs cap
                                              ;; the vector the estimate was made on.  :NONE when
                                              ;; the estimate found none — the encoder's own
                                              ;; search runs on the same pixels behind the same
                                              ;; gates, so it would find none either, and saying
                                              ;; so saves it the pass.
                                              :motion (if mvp (or gmv :none) nil)))
                                         (declare (ignore mv))
                                         (setf pending defer)
                                         (when eqi (setf useq eqi))
                                         (setf cur-qi useq
                                               stale-qi (if (> useq qi)
                                                            (max stale-qi useq) stale-qi))
                                         (when nmoved (ps-note st-mv nmoved))
                                         (when nmb
                                           (ps-note st-mbs nmb)
                                           ;; KEEP BOTH ESTIMATES HONEST, from the counts the
                                           ;; encoder actually reports rather than the ones we
                                           ;; predicted.  Two terms, one measurement, so they are
                                           ;; learned from different frames: a frame that is
                                           ;; almost entirely translation is the only one that
                                           ;; says what a translation costs, and every other
                                           ;; frame has its (now known) translation subtracted
                                           ;; before it says what a coefficient costs.
                                           (let ((payload (and f (max 1d0 (- (length f) overhead))))
                                                 (nm (or nmoved 0)))
                                             (cond
                                               ((null payload))
                                               ((and (> nm 64) (< nmb (max 8 (floor nm 8))))
                                                (let ((m (/ payload nm)))
                                                  (setf mpu (max 0.1d0
                                                                 (min 8d0
                                                                      (+ (* 0.7d0 mpu)
                                                                         (* 0.3d0 m)))))))
                                               ;; asymmetric on purpose: a frame that came out
                                               ;; bigger than predicted has already cost the link
                                               ;; the overrun, so believe it immediately; one that
                                               ;; came out smaller only means we were cautious.
                                               ((>= nmb 16)
                                                (let ((m (/ (max 1d0 (- payload (* nm mpu)))
                                                            nmb (qi-cost useq))))
                                                  (setf bpu (if (> m bpu)
                                                                (+ (* 0.4d0 bpu) (* 0.6d0 m))
                                                                (+ (* 0.85d0 bpu)
                                                                   (* 0.15d0 m)))))))))
                                         ;; THE QUANTIZER CONTROLLER, and it reads the encoder's
                                         ;; OWN OUTPUT, before pacing.  The old one compared
                                         ;; post-pacing throughput with the target — but the pacer
                                         ;; emits at the target by construction, so it measured
                                         ;; its own output and never moved.  What can actually be
                                         ;; wrong is the cost model, and this is where that shows.
                                         (when f
                                           (cond ((> (length f) ceiling-bytes)
                                                  (setf qi-bias (min 40 (+ qi-bias 4))))
                                                 ((< (length f) (floor budget 2))
                                                  (setf qi-bias (max 0 (1- qi-bias))))))
                                     f))))))
                       ;; whatever the branch above did with them, the captured screen has now
                       ;; been coded (or was not needed): start accumulating again
                       (setf cap-y nil cap-u nil cap-v nil cap-dirty nil cap-scan nil)
                       ;; carry the frame-rate plan out to the report.  FPS-CAP is the number
                       ;; worth watching: when the cadence we asked for is above it, the rung is
                       ;; not short of BITS, it is short of macroblocks to spend them on, and no
                       ;; amount of bitrate will make the picture move any faster at this size.
                       (setf fps-want feff fps-cap fcap frame-budget budget)
                       (if frame
                           (progn
                             (enqueue frame seq-ts)
                             (incf sent) (incf bytes (length frame))
                             (ps-note st-kb (/ (length frame) 1024d0))
                             (ps-note st-qi cur-qi))
                           (incf skipped))
                       (incf n)
                       ;; advance the RTP clock by time actually elapsed: frames appear when the
                       ;; desktop changes, not on a fixed cadence, so a nominal 1/fps step drifts
                       (let ((now (get-internal-real-time)))
                         (incf seq-ts (max 1 (floor (* +vp8-rtp-clock+ (- now last-tick))
                                                    internal-time-units-per-second)))
                         (setf last-tick now))))
                   ;; ---- 3. reports -------------------------------------------------------
                   (let ((dt (/ (* 1000d0 (- (get-internal-real-time) last-report))
                                internal-time-units-per-second)))
                     (when (and on-stats (> dt 5000))
                       ;; the same numbers as the log line, but as data — so a UI can present them
                       ;; instead of a human grepping a log
                       (funcall on-stats
                                (list :at (- (get-universal-time) 2208988800)
                                      :take-ms (list (ps-min st-take) (/ (ps-total st-take) (max 1 (ps-n st-take))) (ps-max st-take))
                                      :encode-ms (list (ps-min st-enc) (/ (ps-total st-enc) (max 1 (ps-n st-enc))) (ps-max st-enc))
                                      :refine-ms (list (ps-min st-clean) (/ (ps-total st-clean) (max 1 (ps-n st-clean))) (ps-max st-clean))
                                      :send-ms (list (ps-min st-send) (/ (ps-total st-send) (max 1 (ps-n st-send))) (ps-max st-send))
                                      :frame-kb (list (ps-min st-kb) (/ (ps-total st-kb) (max 1 (ps-n st-kb))) (ps-max st-kb))
                                      :coded-mb (list (ps-min st-mbs) (/ (ps-total st-mbs) (max 1 (ps-n st-mbs))) (ps-max st-mbs))
                                      :sent sent :idle skipped :captured takes
                                      :kbs (/ bytes 1.024 dt) :fps (/ (* 1000d0 sent) dt)
                                      :capture-fps (/ (* 1000d0 takes) dt)
                                      ;; the cadence asked for, the one the skip flags allow at
                                      ;; this rate and resolution, and the frame size they imply
                                      :target-fps fps-want :fps-cap fps-cap
                                      :frame-budget frame-budget
                                      :qi cur-qi :qi-bias qi-bias
                                      :bytes-per-mb (* bpu (qi-cost cur-qi))
                                      :moved-mb (list (ps-min st-mv) (/ (ps-total st-mv) (max 1 (ps-n st-mv))) (ps-max st-mv))
                                      :bytes-per-moved-mb mpu
                                      :pending (if pending (count 1 pending) 0)
                                      :cleanups cleanups)))
                     (when (and log (> dt 5000))
                       (funcall log (format nil "~a | ~a | ~a | ~a | ~a | ~a | ~a | sent ~a idle ~a seen ~a | ~,1f KB/s | ~,1f fps (want ~,1f, flags allow ~,1f, cap ~,1f) frame<=~a B | qi ~a~@[+~a~] b/mb ~,0f mv-b/mb ~,1f | pend ~a | cleans ~a"
                                            (ps-line st-take) (ps-line st-enc) (ps-line st-clean)
                                            (ps-line st-send) (ps-line st-kb) (ps-line st-mbs)
                                            (ps-line st-mv)
                                            sent skipped takes (/ bytes 1.024 dt)
                                            (/ (* 1000d0 sent) dt)
                                            fps-want fps-cap (/ (* 1000d0 takes) dt) frame-budget
                                            cur-qi (and (plusp qi-bias) qi-bias)
                                            (* bpu (qi-cost cur-qi)) mpu
                                            (if pending (count 1 pending) 0) cleanups))
                       (ps-reset st-take) (ps-reset st-enc) (ps-reset st-send) (ps-reset st-loop)
                       (ps-reset st-clean) (ps-reset st-kb) (ps-reset st-mbs) (ps-reset st-qi)
                       (ps-reset st-mv) (ps-reset st-lag)
                       (setf sent 0 skipped 0 bytes 0 takes 0 cleanups 0
                             last-report (get-internal-real-time)))))
               (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
             ;; poll on a short tick: the capture clock and the link are both checked here, and
             ;; neither of them is allowed to become the other's pacing.
             (sleep poll-secs))))
       :name "wm-video-tx")
      (lambda () (setf running nil)))))
