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
;;;     frame bytes  ~=  overhead  +  coded-macroblocks * BPU * (QI-COST qi)
;;;
;;; BPU is learned from the encoder's own output (how expensive THIS screen's content is, in
;;; bytes per macroblock, normalised to qi 63) and QI-COST is the shape of the rate/quantizer
;;; curve, which barely depends on content.  OVERHEAD is the per-macroblock skip flag every inter
;;; frame pays whether it codes anything or not — ~500 bytes at 1280x800, which at the bottom of
;;; the ladder is most of a frame's budget and cannot be left out of the arithmetic.

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
  "How far over the steady-state per-frame budget a single frame may go when the payoff is
covering the WHOLE change instead of part of it.  The rung's MAX-FRAME-KB is 1.5 s of link time,
so this is a uniform ceiling of ~6 s in flight at every rung — the same bound the per-frame cap
was there to give, spent on coverage rather than on sharpness.  Only past this does anything
defer.")

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
                                           (max-frame-kb 32) (capture-secs 0.040)
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
               (bpu 35d0) (rpu 17.5d0) (kpu 17.5d0)
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
                       cur-qi       (min (max cur-qi qi) max-qi))
                 (unless (equal p *video-profile-applied*)
                   (setf *video-profile-applied* p)
                   (when log
                     (funcall log (format nil "profile ~@[~a ~]applied live: qi ~a/~a key-qi ~a target ~,2f KB/s frame<=~,1f KB cleanup ~a ms resync ~,0f s"
                                          (getf p :name) qi max-qi (or key-qi qi) target-kbs
                                          max-frame-kb cleanup-ms key-secs))))))
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
                            (budget (floor (* max-frame-kb 1024)))
                            (ceiling-bytes (* *frame-coverage-x* budget))
                            ;; what this frame is being asked to cover, and the finest quantizer
                            ;; that covers ALL of it inside the ceiling
                            (want (cond ((null pending) dirty)
                                        ((null dirty) pending)
                                        (t (bit-ior pending dirty))))
                            (nwant (if want (count 1 want) mbs))
                            (fq (fit-quantizer nwant ceiling-bytes bpu overhead qi max-qi))
                            (fits (<= (* nwant bpu (qi-cost fq)) (- ceiling-bytes overhead)))
                            (useq (max qi (min max-qi (+ fq qi-bias))))
                            ;; WHEN THE WHOLE SCREEN CHANGED, A KEYFRAME IS THE CHEAP WAY TO
                            ;; COVER IT.  An inter macroblock cannot use intra prediction, so
                            ;; coding 4000 unrelated macroblocks as a delta costs about twice
                            ;; what the same picture costs as a keyframe — and the keyframe
                            ;; needs no reference, so it also cannot inherit anyone's drift.
                            ;; Both estimates are learned from this encoder's own output, so
                            ;; this compares two measurements rather than two guesses.
                            (key-cheaper (and enc y (> nwant (* 0.8 mbs))
                                              (> (* nwant bpu (qi-cost useq))
                                                 (* mbs kpu (qi-cost kqi)))))
                            (need-key (or (null enc) forced key-due key-cheaper))
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
                                         ((and enc pending (position 1 pending))
                                          (let* ((ns (count 1 pending)))
                                            (multiple-value-bind (dq fits)
                                                (fit-quantizer ns ceiling-bytes bpu overhead
                                                               qi max-qi)
                                              (multiple-value-bind (f nmb defer mv eqi)
                                                  (with-timing (st-enc)
                                                    (vp8::encode-inter-frame
                                                     enc (vp8::ve-prev-y enc) (vp8::ve-prev-u enc)
                                                     (vp8::ve-prev-v enc)
                                                     :qi dq :dirty pending :force t :clean-qi qi
                                                     :max-mbs (unless fits
                                                                (mbs-fitting ceiling-bytes bpu
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
                                                (fit-quantizer ns (floor ceiling-bytes 2) rpu
                                                               overhead qi max-qi from)
                                              (let* ((whole (and rfits (<= rq (- stale-qi 2))))
                                                     (useq (if whole rq qi))
                                                     (cap (unless whole
                                                            (mbs-fitting budget rpu overhead
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
                                 (setf last-key (get-internal-real-time) pending nil
                                       cur-qi kqi stale-qi (if (plusp kstale) kqi qi))
                                 (setf enc (vp8::make-encoder w h))
                                 (multiple-value-bind (f ry ru rv)
                                     (with-timing (st-enc)
                                       (vp8:encode-gray-frame y w h :qi kqi :u u :v v))
                                   (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru
                                         (vp8::ve-ref-v enc) rv)
                                   (replace (vp8::ve-prev-y enc) y)
                                   (replace (vp8::ve-prev-u enc) u)
                                   (replace (vp8::ve-prev-v enc) v)
                                   (setf (vp8::ve-have-ref enc) t
                                         (vp8::ve-stale-mbs enc)
                                         (make-array (* (vp8::ve-mb-cols enc)
                                                        (vp8::ve-mb-rows enc))
                                                     :element-type 'bit :initial-element kstale))
                                   (let ((kn (* (vp8::ve-mb-cols enc) (vp8::ve-mb-rows enc))))
                                       (when (plusp kn)
                                         (setf kpu (/ (length f) kn (qi-cost kqi)))))
                                   f))
                                ;; otherwise an inter frame, and this is the whole point: pick the
                                ;; quantizer the CHANGE fits in, and code all of it.
                                (t
                                 (let ((useq useq)
                                       ;; deferral survives for exactly one case: even MAX-QI
                                       ;; overruns the ceiling, so code what fits and carry the
                                       ;; rest.  On this ladder that is now rare rather than the
                                       ;; normal path.
                                       (cap (unless fits
                                              (mbs-fitting ceiling-bytes bpu overhead useq))))
                                   (multiple-value-bind (f nmb defer mv eqi)
                                           (with-timing (st-enc)
                                             (vp8::encode-inter-frame
                                              enc y u v :qi useq :dirty want :force t
                                              :clean-qi qi :max-mbs cap))
                                         (declare (ignore mv))
                                         (setf pending defer)
                                         (when eqi (setf useq eqi))
                                         (setf cur-qi useq
                                               stale-qi (if (> useq qi)
                                                            (max stale-qi useq) stale-qi))
                                         (when nmb
                                           (ps-note st-mbs nmb)
                                           ;; keep the cost estimate honest, at the quantizer the
                                           ;; encoder actually used
                                           (when (and f (>= nmb 16))
                                             ;; asymmetric on purpose: a frame that came out
                                             ;; bigger than predicted has already cost the link
                                             ;; the overrun, so believe it immediately; one that
                                             ;; came out smaller only means we were cautious.
                                             (let ((m (/ (max 1d0 (- (length f) overhead))
                                                         nmb (qi-cost useq))))
                                               (setf bpu (if (> m bpu)
                                                             (+ (* 0.4d0 bpu) (* 0.6d0 m))
                                                             (+ (* 0.85d0 bpu) (* 0.15d0 m)))))))
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
                                      :qi cur-qi :qi-bias qi-bias
                                      :bytes-per-mb (* bpu (qi-cost cur-qi))
                                      :pending (if pending (count 1 pending) 0)
                                      :cleanups cleanups)))
                     (when (and log (> dt 5000))
                       (funcall log (format nil "~a | ~a | ~a | ~a | ~a | ~a | sent ~a idle ~a seen ~a | ~,1f KB/s | ~,1f fps (cap ~,1f) | qi ~a~@[+~a~] b/mb ~,0f | pend ~a | cleans ~a"
                                            (ps-line st-take) (ps-line st-enc) (ps-line st-clean)
                                            (ps-line st-send) (ps-line st-kb) (ps-line st-mbs)
                                            sent skipped takes (/ bytes 1.024 dt)
                                            (/ (* 1000d0 sent) dt) (/ (* 1000d0 takes) dt)
                                            cur-qi (and (plusp qi-bias) qi-bias)
                                            (* bpu (qi-cost cur-qi))
                                            (if pending (count 1 pending) 0) cleanups))
                       (ps-reset st-take) (ps-reset st-enc) (ps-reset st-send) (ps-reset st-loop)
                       (ps-reset st-clean) (ps-reset st-kb) (ps-reset st-mbs) (ps-reset st-qi)
                       (ps-reset st-lag)
                       (setf sent 0 skipped 0 bytes 0 takes 0 cleanups 0
                             last-report (get-internal-real-time)))))
               (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
             ;; poll on a short tick: the capture clock and the link are both checked here, and
             ;; neither of them is allowed to become the other's pacing.
             (sleep poll-secs))))
       :name "wm-video-tx")
      (lambda () (setf running nil)))))
