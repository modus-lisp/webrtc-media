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

(defparameter *motion-qi-ease* 8
  "How many quantizer steps a LIVE frame may SHARPEN by, per frame, while the screen is moving.

Coarsening is never rate-limited — the budget binds now, and delaying the response to it is the
lag this sender spends most of its rules removing.  Sharpening is, and only during motion.  The
quantizer a live frame is fitted to is a function of how much of the screen changed in it, and
that number is not smooth: a scroll's frames alternate between ones the motion vector carries
almost entirely and ones with a band of genuinely new content, so the fit alternates too.  Once
the motion ceiling is unclamped (see MOTION-QI) the swing is the full ladder rather than the
15 steps MAX-QI used to allow, and a picture that jumps between 127 and 8 and back while the
user is still scrolling reads as PUMPING — which is a different complaint from the one being
fixed, not an improvement on it.

Eight steps is about one 4:3 change in coefficient magnitude per frame, so a frame that comes
out cheap converges on its quantizer over three or four frames — under a second at any rung
where motion is fluid at all, and invisible against the refinement pass that follows.  0 turns
the limit off entirely and restores the raw fit.")

;;; ---- idle work is not exempt from the frame budget --------------------------
;;;
;;; Two frames carry nothing the viewer is waiting for: the REFINEMENT pass, which re-codes finer
;;; what was already delivered coarse, and the DRAIN, which finishes what a live frame could not
;;; fit.  Both run while the desktop is idle, and both used to keep the LINK-TIME ceiling that live
;;; frames were freed from — on the reasoning that nothing is waiting behind them, so a smaller
;;; frame would buy nobody anything.
;;;
;;; THAT REASONING HOLDS FOR EXACTLY AS LONG AS THE SCREEN STAYS IDLE.  A frame handed to the pacer
;;; cannot be recalled: the moment the user scrolls or clicks, whatever is in flight is added to how
;;; long the response waits, and an idle frame sized in link-seconds is measured in seconds.  From a
;;; live session at the 320 kbps rung: refinement frames of 50 to 107 KB, 2.8 to 4.4 s each on a
;;; 39 KB/s link, against a live frame budget of 3333 bytes.  The user's report was "still laggy at
;;; 320 kbps", and it is worse at the TOP of the ladder, not the bottom, because a faster link lets
;;; the refinement pick a finer quantizer and so build a bigger frame.
;;;
;;; So an idle frame gets the LIVE frame's budget too, times a small multiple, and the refinement
;;; converges over many small frames instead of one huge one.  There is nothing to trade away in
;;; doing so: refinement is DIFFERENTIAL (see QI-STEP-COST), so it can stop anywhere and resume from
;;; where it stopped.  What it costs is the fixed per-frame overhead, once per extra frame — which
;;; is why the multiple is not 1.

(defparameter *idle-frame-x* 2
  "How many LIVE frame budgets one IDLE frame — a refinement pass, a deferred drain — may spend.

This is the LATENCY half of the bound, and it is stated in frames because that is the unit the
delay is felt in: an idle frame already on the wire delays the next live one by at most this many
frame times.

WHY NOT 1.  Every inter frame pays its skip flags whatever it codes, and each extra frame the
refinement arc is split into pays them again.  Measured, a whole screen refined from a coarse
keyframe to the rung's own quantizer, total bytes at 1 / 2 / 4 live budgets per frame: 24.0 / 17.6 /
15.7 KB at 20 kbps, 82.3 / 57.9 / 50.7 at 40, 317.8 / 137.0 / 117.3 at 80, 368.4 / 199.6 / 168.8 at
160, 99.7 / 85.2 / 79.8 at 320.  One is a third to a half more expensive than two, everywhere.")

(defparameter *idle-overhead-x* 9
  "The other half of the bound: the fewest times its own fixed overhead an idle frame may be.

*IDLE-FRAME-X* alone is not enough, because BELOW 320 kbps THE LIVE BUDGET IS ITSELF AT A FLOOR.
FRAME-FPS-CAP will not let a live frame be smaller than *FRAME-OVERHEAD-X* — three — times the skip
flags, and at 20 through 160 kbps the requested cadence is above that cap, so the live budget IS
three times the flags exactly.  Two of those is six, and a sixth of every refinement frame is then
overhead.  This says nine instead: three live-frame floors, so an idle frame is at most a ninth
flags whatever the rung.

Measured on the session bench, time from the scroll stopping to within 1 dB of the picture the
UNBOUNDED refinement settled on, at 2 / 3 / 4 live budgets per idle frame, against the unbounded
arm: 25.2 / 25.0 / 24.1 against 26.7 s at 20 kbps; 23.7 / 21.5 / 21.3 against 23.4 at 40; 22.9 /
20.2 / 19.0 against 16.5 at 80; 18.7 / 16.8 / 15.5 against 14.9 at 160; 8.4 / 8.1 / 8.0 against 7.7
at 320.  At 320, where the live budget is set by the cadence and not by the flags, two is already
worth as much as four and costs half the stall; below it, the extra room is worth two to four
seconds of the arc and costs a tenth of a second of stall.  Nine is where those two readings meet:
it is three live budgets at every rung the flags bind, and it is under two at 320, where they do
not.")

(defun idle-frame-budget (budget overhead)
  "The bytes an idle frame may spend: *IDLE-FRAME-X* live frames' worth, or *IDLE-OVERHEAD-X* times
its own skip flags, whichever is more.  The first bounds the delay it can impose, the second keeps
it from being mostly flags — and which one binds says which of those two the rung is short of."
  (max (* *idle-frame-x* (max 256 budget))
       (* *idle-overhead-x* (max 64 (ceiling overhead)))))

;;; ---- resolution follows damage ---------------------------------------------
;;;
;;; The quantizer is not the last lever, and by this point it is not the binding one either.  Two
;;; ceilings are left, and NEITHER of them is a bitrate — both are counts of macroblocks:
;;;
;;;   1. THE KEYFRAME FLOOR.  A change with no usable vector has to be coded as a keyframe (see
;;;      KEY-CHEAPER), and the cheapest keyframe VP8 can code is ~5.5 bytes per macroblock at
;;;      qi 127, the coarsest quantizer the format has.  At 1280x800 that is 22 KB, so a
;;;      full-screen scroll cannot exceed 1.8 frames a second at 320 kbps, 0.9 at 160, 0.45 at 80,
;;;      0.23 at 40, whatever else is done to it.  Measured scroll: 1.17 fps of a possible 1.8 —
;;;      sitting on this ceiling, not on the byte budget.
;;;
;;;   2. MOTION-SEARCH REACH.  A vector component is +-255 px.  When frames are seconds apart a
;;;      scroll displaces ~400 px between the ones actually delivered, so no vector exists, so the
;;;      frame is priced as a keyframe, so the next gap is longer and the next displacement bigger.
;;;      It reinforces itself, and no amount of bitrate breaks it.
;;;
;;; Halving the resolution quarters the macroblocks and takes all three numbers with it: the
;;; keyframe, the per-frame skip flags, AND the displacement — 400 px becomes 200, back inside the
;;; search range where a scroll costs a mode bit instead of a picture.
;;;
;;; SO WHY NOT SIMPLY RUN SMALL.  Because a resolution switch is a keyframe (VP8's size is a
;;; per-frame property; there is no way to code a small region inside a large frame), and because
;;; most damage is not global.  A menu opening, a cursor, a line of text: those are twenty
;;; macroblocks, the other 3980 are already correct at full resolution in the viewer's decoder, and
;;; they cost one skip bit each to keep.  Dropping resolution for that destroys delivered detail
;;; AND pays a keyframe to destroy it.  Whereas a page load or a scroll the search cannot track has
;;; no preserved detail to protect: everything is changing anyway, so the only question is how many
;;; macroblocks the change is spread over, and that is the one thing we can choose.
;;;
;;; Hence: FULL RESOLUTION BY DEFAULT, drop when the damage is genuinely global, restore when the
;;; screen goes quiet.  And MEASURE THE DAMAGE BY MACROBLOCK COUNT, SPLIT INTO TRANSLATION AND NEW
;;; CONTENT — never by bounding box.  A window dragged across the screen has an enormous bounding
;;; box and almost no new content; it is already cheap, because the vector carries it.  A rule
;;; written on bbox area would drop resolution for exactly the case that had stopped needing it.

(defvar *video-scale* 1
  "The resolution divisor the frame source is being asked for: 1, 2 or 4.  The SENDER owns this —
it is set from the damage the encoder actually saw — and the source reads it when it next produces
a frame.  A source that ignores it simply never changes resolution, which is the old behaviour.")

(defun video-scale () *video-scale*)

(defparameter *keyframe-floor-per-mb* 5.53d0
  "Bytes per macroblock in the cheapest keyframe VP8 can produce: measured, 22116 bytes over the
4000 macroblocks of a 1280x800 screen at qi 127.  Stated per macroblock because that is what it is
a function of — this number is why resolution, and only resolution, moves the keyframe floor.")

(defparameter *motion-frame-secs* 0.75d0
  "The longest a whole-screen frame may take on the wire and still read as MOTION.  Past about this
the screen is a slideshow and the individual pictures being sharper does not help; it is also
roughly where a scroll's inter-frame displacement passes VP8's +-255 px vector range, so the frames
stop being able to reference each other at all.")

(defparameter *scale-drop-dwell-secs* 0.5d0
  "The least time between one resolution switch and the next drop.  A drop is cheap — its keyframe
is at the SMALLER size — so this is short.")

(defparameter *scale-lost-frames* 2
  "How many whole-screen frames in a row may find NO motion vector before resolution drops,
whatever the rate says.

This is the second ceiling caught in the act rather than predicted.  A vector component reaches
+-255 px, and — because a probe can only vote for a displacement that keeps it inside the frame —
the practical reach is well under that: measured, a scroll is tracked at 96 px and lost at 170 on a
640x400 picture.  So when frames are far apart the picture moves further than the search can follow,
the frame prices as a screenful of new content, it goes out as a keyframe, and the keyframe's own
size sets the gap to the next one — which is at least as large.  Nothing in the rate breaks the
loop; only shrinking the picture does, because that divides the displacement too.

A RUN, not one frame, and that is what separates a SCROLL from a WINDOW SWITCH.  A switch is one
vector-less whole-screen frame and then a still screen — a keyframe is the right answer and there
is nothing to escape.  A scroll produces them without end.  At 320 kbps, where the rate rule alone
never drops, this is the only thing that does: measured there, eight consecutive 24 KB keyframes at
1.17 fps, every one of them because 0.85 s of scrolling is 408 px and the search cannot see that
far.")

(defparameter *video-scales* '(1 2 4))

(defparameter *first-picture-mbs* 200
  "The fewest macroblocks the FIRST picture of a session is allowed to be reduced to.  At 1280x800
a quarter is 260, which is a recognisable desktop — you can see where the windows are and which one
has focus — and 1.4 KB.  Below a couple of hundred macroblocks it stops being a picture.")

(defparameter *scale-climb-secs* 0.3d0
  "How long the screen must be still before the first picture climbs one step towards full size.

Short, and it can afford to be: the encoder does not run at all while the link is busy, so by the
time this is even consulted the previous step has been delivered in full.  It is a debounce, not a
hysteresis — the hysteresis that matters (SCALE-RESTORE-SECS) exists to keep a PAUSE IN A SCROLL
from buying a full-size keyframe, and at session start there is no scroll to pause.")

(defun scale-for-first-picture (full-mbs)
  "The resolution the FIRST picture of a session should be coded at: the smallest that is still
worth looking at.

TIME TO FIRST PIXELS IS NOT TIME TO A COMPLETE PICTURE, and the first is what a session is judged
on.  A whole screen at 1280x800 cannot be sent in under 22 KB whatever the quantizer, which is 4.4
seconds of a 40 kbps link and 8.8 of a 20 kbps one — seconds in which the viewer has NOTHING, not a
rough version of something.  The same screen at a quarter of the size is 1.4 KB: a third of a
second, blurry, and unmistakably the desktop.  Nobody would choose the blurry one to KEEP, and
nobody has to — it is replaced by a half-size picture and then a full-size one as fast as the link
will carry them (see the climb in START-VIDEO).  What they would choose is to see something."
  (or (car (last (remove-if (lambda (s) (< (/ full-mbs (* s s)) *first-picture-mbs*))
                            *video-scales*)))
      1))

(defun scale-for-motion (bytes-per-sec full-mbs)
  "The resolution divisor at which a WHOLE-SCREEN change can still be delivered as motion at this
rate.  The cheapest such frame is a keyframe, its price is *KEYFRAME-FLOOR-PER-MB* per macroblock,
and a divisor of S has 1/S^2 of the macroblocks.  FULL-MBS is the macroblock count at full size.

This is the rung's EAGERNESS, and it is a derivation rather than a table: at 320 kbps a 1280x800
keyframe is 0.55 s of link and the answer is 1 — full resolution, never drop, the detail is worth
more than the cadence.  At 160 and 80 it is 2, at 40 and below it is 4."
  (or (find-if (lambda (s)
                 (<= (/ (* *keyframe-floor-per-mb* full-mbs) (* s s) (max 1d0 bytes-per-sec))
                     *motion-frame-secs*))
               *video-scales*)
      (car (last *video-scales*))))

(defun scale-drop-fraction (bytes-per-sec full-mbs)
  "How much of the screen must be GENUINELY NEW — changed and not accounted for by the frame's
motion vector — before dropping resolution is worth its keyframe.  The same arithmetic as
SCALE-FOR-MOTION read the other way round: the fraction of a full-size screen this rate can still
recode inside *MOTION-FRAME-SECS*.  Above that fraction the frame is too big to arrive in time and
the macroblocks have to go somewhere; below it, full resolution is affordable and preferable.

At 320 kbps this comes out at 1.0 — nothing short of the entire screen, and SCALE-FOR-MOTION says 1
there anyway.  At 160 it is ~0.68, at 80 ~0.34, and at 40 and below it floors at 0.25: down there
a quarter of the screen changing at once is already more than the link can carry at full size."
  (max 0.25d0 (min 1d0 (/ (* (max 1d0 bytes-per-sec) *motion-frame-secs*)
                          (* *keyframe-floor-per-mb* (max 1 full-mbs))))))

(defun scale-restore-secs (bytes-per-sec full-mbs)
  "How long the screen must be QUIET before full resolution is restored.

THE HYSTERESIS IS DELIBERATELY ASYMMETRIC because the two switches do not cost the same.  Dropping
is cheap: its keyframe is at the smaller size, 1.4 KB at quarter resolution where the full-size one
is 22 KB.  Restoring costs the full-size keyframe, so it must not happen on a pause in a scroll —
pay 4.4 s of a 40 kbps link for a picture that is stale again before it lands and you have made
both resolutions worse than either.  So the wait is the RESTORE'S OWN PRICE IN SECONDS: about as
long as the keyframe it is about to send will take to arrive.  A scroll that has been still that
long has stopped.

THE FLOOR IS A PROPERTY OF HANDS, NOT OF THE LINK.  Above about 80 kbps the restore keyframe is
quick enough that its own price would allow a wait of a second or less — and a second is shorter
than the gap between two flicks of a scroll wheel.  Measured at 160 kbps with a 1.5 s floor, on a
scroll that moved for 1.5 s and rested for 2.0, twelve times over: thirteen resolution switches and
five full-size keyframes, 200 KB spent restoring a picture that was stale again before it landed.
At 2.5 s the same scroll switches twice."
  (max 2.5d0 (min 10d0 (/ (* *keyframe-floor-per-mb* (max 1 full-mbs))
                          (max 1d0 bytes-per-sec)))))

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

(defvar *video-input-at* 0
  "GET-INTERNAL-REAL-TIME of the last input event forwarded to the desktop, or 0 if the caller does
not report any.  Set it from wherever key and pointer events go in; the sender only reads it.

INPUT IS DAMAGE THAT HAS NOT HAPPENED YET.  The idle passes are gated on the screen having been
quiet for CLEANUP-MS, and a screen is quiet for the whole round trip between a click landing on the
desktop and the desktop repainting — which is precisely the window in which starting a refinement
puts it in front of the response.  A keystroke is the cheapest possible warning that the picture is
about to change, and it arrives on the channel we are already reading.  So it counts as activity for
the purpose of starting idle work, and for nothing else: it does not touch the resolution rules, and
it cannot starve refinement, because it only postpones it by the same CLEANUP-MS that real damage
does.")

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
                                           (motion-qi 127)
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
reported, so a rung sitting on its ceiling is visible as such.

IDLE WORK IS SIZED THE SAME WAY, times *IDLE-FRAME-X*.  A refinement or a drain runs while the
desktop is quiet, and the temptation is to let it spend freely because nothing is waiting behind it
— but the screen stops being quiet without warning, and a frame already handed to the pacer cannot
be recalled, so the size of the idle frame in flight IS the delay on the next thing the user does.
It is worst at the top of the ladder, where a fast link lets a refinement pick a fine quantizer and
build a huge frame.  Both jobs are resumable, so being cut short costs one frame's skip flags; see
*IDLE-FRAME-X* and IDLE-FRAME-BUDGET.  Input counts as activity for the purpose of starting them
(*VIDEO-INPUT-AT*), because input is damage that has not happened yet.

Quality adapts to the link: QI is the best (sharpest) quantizer, MAX-QI the worst we will fall
back to, and TARGET-KBS the rate the pacer emits at.

AND MAX-QI IS THE SETTLING CEILING, NOT THE MOTION ONE — MOTION-QI IS THAT, and separating the
two is what makes a busy screen cheap at a fast link.  They are different questions wearing one
name.  MAX-QI bounds THE QUALITY THE PICTURE SETTLES TO: it is where the refinement arc stops,
so a rung that can afford a sharp still should have a low one.  What a LIVE frame may fall to
when its budget binds is a different number entirely, and it has to reach 127 — the coarsest
quantizer the format has — AT EVERY RATE, because a frame that must cover the whole screen
inside one frame-time's worth of bytes can only do it by being coarse.  With one knob doing
both, the rungs from 160 kbps up were capped at MAX-QI 52 during motion: no quantizer in
[QI, 52] ever fit a full-screen scroll, FIT-FRAME's fallback fired on every frame, and each one
spent up to the LATENCY ceiling instead — 22 KB against a 5 KB budget at 480 kbps, four frames
of it per ten seconds.  Sharp, and several seconds stale.  The picture only went coarse-and-
fluid at 5-48 kbps, which is where MAX-QI happened to reach 100-127 on its own.

So MOTION-QI is the ceiling FIT-FRAME searches to and the clamp on the chosen quantizer, and
nothing else reads it: the refinement pass, the drain and the settled quality are all still
MAX-QI's, so what the picture converges to when the screen stops is exactly what it was.  It
defaults to 127 and there is no rate at which it should be lower — coarse frames are the
cheapest motion there is, and the refinement machinery already sharpens them back.

MOTION-QI-EASE rate-limits ONE DIRECTION of that: a live frame may coarsen as fast as the
budget demands, but may only SHARPEN by that many quantizer steps per frame while the screen
is still moving.  Unclamping the ceiling means a frame whose change happens to be small can
jump from 127 to QI and the next one back, which reads as the picture pumping.  Coarsening is
never delayed, because delaying it is the lag this whole change exists to remove.

THE QUANTIZER CONTROLLER on top of all of it is driven by the
ENCODER'S OWN OUTPUT against the frame budget, measured BEFORE pacing — the old one compared
post-pacing throughput with the target, which the pacer produces by construction, so it read
'on target' for ever and never coarsened at all.  Then, once the screen has been quiet for
CLEANUP-MS, whatever was coded coarsely is re-coded finer, a whole screen at a time.

AND RESOLUTION IS A LEVER TOO — the last one, and past a certain point the only one that moves.
A keyframe's price is a MACROBLOCK COUNT (~5.5 bytes each at the coarsest quantizer VP8 has), the
per-frame skip flags are a macroblock count, and VP8's +-255 px vector range is a distance the
picture may move BETWEEN DELIVERED FRAMES — which at one frame every few seconds a scroll exceeds,
losing the vector and turning every frame into a keyframe.  All three ease by S^2 (or S) when the
source is asked for 1/S of the picture, so the sender asks: it publishes *VIDEO-SCALE* and a source
that honours it hands back a box-filtered desktop at that size.  The frame-size mismatch then
drives the resize path that already existed here, which is a keyframe — and that is exactly why the
switch is not free and not frequent.  FULL RESOLUTION IS THE DEFAULT and local damage keeps it: the
macroblocks that did not change are already correct in the viewer's decoder and cost a bit each to
say so, and there is no version of dropping resolution for a cursor that is not simply throwing
delivered detail away.  What earns a switch is damage that is GENUINELY GLOBAL — measured as
changed macroblocks minus translating ones, never as a bounding box — because then there is no
preserved detail to protect.  See SCALE-FOR-MOTION, SCALE-DROP-FRACTION and SCALE-RESTORE-SECS for
the policy; the hysteresis is asymmetric because the two switches do not cost the same.

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
    ;; a session starts at full resolution as far as the SOURCE is concerned; the first frame's own
    ;; damage is what decides whether it stays there (see the SWITCHED binding below).  Set here
    ;; rather than left over from whatever the last session ended on.
    (setf *video-scale* 1)
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
               (fps-want 0d0) (fps-cap 0d0) (frame-budget 0) (idle-budget 0)
               (cur-qi qi) (qi-bias 0) (stale-qi qi)
               ;; the quantizer the last LIVE frame was coded at, and the only thing
               ;; *MOTION-QI-EASE* reads.  Reset to QI the moment the screen goes quiet, so a
               ;; burst of coarse motion cannot slow the refinement arc that follows it.
               (moving-qi qi)
               ;; TWO DEGREES OF NOT FITTING, and they are worth separate counts because they
               ;; have separate remedies.  OVERRUNS: no quantizer up to the MOTION ceiling could
               ;; cover the change inside the frame-rate budget, so the frame spent up to the
               ;; latency ceiling instead — whole and current, but bigger than the cadence wanted.
               ;; Before the motion ceiling was separated from MAX-QI this was every frame of
               ;; every scroll from 160 kbps up, and it is what "sharp but laggy" was made of.
               ;; DEFERRALS: even the latency ceiling was too small, so part of the change was
               ;; carried to a later frame and the viewer is briefly shown a patchwork.  A rung
               ;; that keeps doing either is short of MACROBLOCKS rather than of quantizer, and
               ;; only resolution moves it.
               (overruns 0) (deferrals 0)
               ;; THE RESOLUTION DIVISOR NOW IN FORCE, and when it last changed.  SCALE-AT starts
               ;; at zero rather than at "now" so the FIRST picture is free to be small: a session
               ;; start is global damage by definition, there is no delivered detail to protect,
               ;; and at 40 kbps it is the difference between a picture in 0.4 s and one in 5.9 s.
               (sc 1) (scale-at 0) (scale-drops 0) (scale-ups 0) (lost-run 0)
               ;; CLIMBING is on from the first picture until the stream reaches full size without
               ;; the desktop having moved.  While it is on, going back up is a fast STEP rather
               ;; than the slow jump the steady-state rule makes — see *SCALE-CLIMB-SECS*.
               (climbing nil)
               ;; the divisor of the last frame that actually WENT OUT.  A switch is a request:
               ;; SC changes, the frame is skipped, and the picture at the new size does not exist
               ;; until the source has answered and the encoder has coded it.  Climbing on SC
               ;; alone skipped straight past half size — two switches in the same millisecond,
               ;; and the viewer never saw the middle step at all.
               (sc-coded 1)
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
                       ;; the MOTION ceiling, which a rung may name and which defaults to the
                       ;; coarsest quantizer the format has.  Never finer than MAX-QI: this knob
                       ;; exists to let a LIVE frame go coarser than the settled picture, so a
                       ;; rung setting it below MAX-QI would only be re-creating the clamp.
                       motion-qi    (max (or (getf p :motion-qi) motion-qi) max-qi)
                       ;; CUR-QI is the quantizer the last frame ACTUALLY used, and during motion
                       ;; that is now allowed past MAX-QI — clamping it here would make the
                       ;; report lie about the frame that just went out.
                       cur-qi       (min (max cur-qi qi) (max max-qi motion-qi)))
                 (unless (equal p *video-profile-applied*)
                   (setf *video-profile-applied* p)
                   (when log
                     (funcall log (format nil "profile ~@[~a ~]applied live: qi ~a/~a (motion ~a) key-qi ~a target ~,2f KB/s ~,1f fps frame<=~,1f KB cleanup ~a ms resync ~,0f s"
                                          (getf p :name) qi max-qi motion-qi (or key-qi qi)
                                          target-kbs
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
                           ;; two frames, and the one we code has to cover all of them.
                           ;; UNLESS THE GRID CHANGED UNDER THEM.  A resolution switch restates
                           ;; damage on a different macroblock grid, so the accumulated set is
                           ;; not merely stale, it is a different length — BIT-IOR would error and
                           ;; the frame it described no longer exists.  Start again from the new
                           ;; one, which a switching source reports as the whole screen anyway.
                           (if dirty
                               (if (and cap-dirty (= (length cap-dirty) (length dirty)))
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
                            ;; THE FRAME IN HAND IS NOT THE SIZE THE ENCODER HOLDS.  Either the
                            ;; desktop resized, or we asked the source for a different resolution
                            ;; (see *VIDEO-SCALE*).  Everything that reaches across the two — the
                            ;; motion search, the keyframe/inter comparison, the deferred set — is
                            ;; reading one geometry against the other and must not run.
                            (resized (and enc y (or (/= w (vp8::ve-width enc))
                                                    (/= h (vp8::ve-height enc)))))
                            ;; the macroblock count OF THE FRAME BEING CODED, which on the pass
                            ;; that changes resolution is not the encoder's.  Overhead, the frame
                            ;; budget and the fps ceiling are all functions of it.
                            (mbs (cond ((and y w h) (* (ceiling w 16) (ceiling h 16)))
                                       (enc (* (vp8::ve-mb-cols enc) (vp8::ve-mb-rows enc)))
                                       (t 0)))
                            ;; the skip flag every macroblock pays whether it codes or not
                            (overhead (+ 64 (floor mbs 8)))
                            ;; the LATENCY ceiling, in seconds of link — what a frame with nothing
                            ;; waiting behind it may cost, and the backstop for one that cannot be
                            ;; covered inside the frame-rate budget below
                            (link-budget (floor (* max-frame-kb 1024)))
                            (link-ceiling (* *frame-coverage-x* link-budget))
                            ;; HOW LONG SINCE ANYTHING HAPPENED, counting input as something.  The
                            ;; idle passes below are gated on this; the resolution rules are not
                            ;; (they use QUIET, which is the desktop's own clock) — restoring full
                            ;; resolution is not made wrong by a keystroke, and gating it on one
                            ;; would let a jiggled mouse pin the picture small.
                            (idle-ms (/ (* 1000d0 (- (get-internal-real-time)
                                                     (max idle-since *video-input-at*)))
                                        internal-time-units-per-second))
                            ;; THE FRAME-RATE BUDGET: a second of the link divided by the cadence
                            ;; we are trying to hold, clamped to what the skip flags leave room
                            ;; for.  This is the number that decides whether the next rung up buys
                            ;; more pictures or just a better one.
                            (bps (* target-kbs 1024))
                            ;; TARGET-FPS ARRIVES SIZED FOR THE FULL-RESOLUTION SCREEN, because
                            ;; that is where the rung derived it (a second of link over three
                            ;; frames' worth of skip flags at 4000 macroblocks).  At a reduced
                            ;; resolution both the flags and the encode are S^2 cheaper, so asking
                            ;; for the same cadence would leave the whole point of the switch on
                            ;; the floor.  Raise the REQUEST by S^2; the ceiling that actually
                            ;; binds is still FRAME-FPS-CAP, computed from this frame's real
                            ;; overhead, and *MAX-TARGET-FPS* still caps it at what the encoder
                            ;; can produce.
                            (sfps (min 12d0 (* (float (or target-fps 10) 1d0) sc sc)))
                            (plan (multiple-value-list
                                   (motion-frame-budget bps sfps overhead)))
                            (budget (first plan)) (feff (second plan)) (fcap (third plan))
                            ;; ... and what an IDLE frame may spend, which is the same number times
                            ;; a small multiple.  See *IDLE-FRAME-X*.
                            (idle-room (idle-frame-budget budget overhead))
                            ;; what this frame is being asked to cover, and the finest quantizer
                            ;; that covers ALL of it inside the ceiling.  PENDING is dropped
                            ;; outright when the grid changed under it — it names macroblocks of a
                            ;; picture that is no longer the one being coded.
                            (want (cond ((null pending) dirty)
                                        ((null dirty) pending)
                                        ((= (length pending) (length dirty))
                                         (bit-ior pending dirty))
                                        (t dirty)))
                            (nwant (if want (count 1 want) mbs))
                            ;; HOW MUCH OF THE CHANGE IS MERELY A TRANSLATION.  Asked before the
                            ;; frame is coded, because it is what decides HOW to code it — and
                            ;; asked of the encoder's own translation test rather than guessed, so
                            ;; the two terms below are both measurements.  The vector goes back
                            ;; into the encode below, so the search happens once per frame.
                            (mvp (when (and enc y (not resized))
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
                            ;; ---- RESOLUTION FOLLOWS DAMAGE -------------------------------
                            ;; Decided HERE, before the frame is coded, from the two counts the
                            ;; motion estimate has just produced — how much changed, and how much
                            ;; of that merely moved.  Never from a bounding box: a window dragged
                            ;; across the screen has an enormous one and almost no new content.
                            ;;
                            ;; A switch means the pixels in hand are the wrong size, so the frame
                            ;; is SKIPPED rather than coded: coding it would spend the expensive
                            ;; resolution on a picture we are about to replace, which on a session
                            ;; start is the whole 22 KB we are trying not to send.  The source is
                            ;; expected to answer the next poll at the new size (a scale change
                            ;; produces a frame whether the screen changed or not), and if it does
                            ;; not honour the request at all nothing is lost but one poll — the
                            ;; decision cannot fire twice, since SC now equals what it asked for.
                            (switched
                              (let* ((tn (get-internal-real-time))
                                     (dwell (/ (float (- tn scale-at))
                                               internal-time-units-per-second))
                                     (quiet (/ (float (- tn idle-since))
                                               internal-time-units-per-second))
                                     (full-mbs (* mbs sc sc))
                                     (goal (scale-for-motion bps full-mbs)))
                                (cond
                                  ;; THE FIRST PICTURE OF THE SESSION, and it is not judged by the
                                  ;; same rule as the rest.  Everything else here trades a picture
                                  ;; the viewer already has against one it is about to get; this
                                  ;; one trades against a BLANK SCREEN, so the only number that
                                  ;; matters is how soon there are pixels.  Smallest size that is
                                  ;; still a picture, at every rung including the top one — 1.4 KB
                                  ;; and a third of a second at 40 kbps, against 22 KB and four and
                                  ;; a half seconds of nothing — then climb.
                                  ((and y (null enc) (> (scale-for-first-picture mbs) 1))
                                   (setf sc (scale-for-first-picture mbs) scale-at tn
                                         climbing t lost-run 0)
                                   (incf scale-drops) t)
                                  ;; ... and the climb.  One step at a time, as fast as each step
                                  ;; can be delivered: quarter, half, full.  Stepping costs a
                                  ;; keyframe per step and is still the cheaper trade — at 40 kbps
                                  ;; the three of them together are 29 KB against 22, and they buy
                                  ;; a picture at 0.3 s and a legible one at 1.5 instead of four
                                  ;; and a half seconds of blank.
                                  ((and climbing (> sc 1) (null y) (= sc-coded sc)
                                        (>= quiet *scale-climb-secs*))
                                   (setf sc (max 1 (floor sc 2)) scale-at tn lost-run 0)
                                   (when (= sc 1) (setf climbing nil))
                                   (incf scale-ups) t)
                                  ;; genuinely global damage, and this rate cannot carry it at
                                  ;; this size.  Drop the whole way at once — stepping down would
                                  ;; buy two keyframes to reach one resolution.
                                  ;;
                                  ;; NOT ON A RESIZED FRAME, and that is not a detail: a frame that
                                  ;; arrives at a new size reports the WHOLE SCREEN as damaged by
                                  ;; construction, because the viewer has no picture at that size.
                                  ;; Judged on its own damage, the frame that restores full
                                  ;; resolution is always "global" and always immediately undoes
                                  ;; itself — measured, before this line: up at 4.6 s, down at 5.5,
                                  ;; up at 10.6, down at 11.8, for the whole session.  Damage WE
                                  ;; caused is not evidence about the desktop.  Whatever the
                                  ;; desktop is really doing shows up on the next frame, which is
                                  ;; not resized and is judged normally.
                                  ;;
                                  ;; TWO CONDITIONS, AND THE FIRST IS WHAT PROTECTS A DRAG.  The
                                  ;; damage has to cover most of the screen AND most of what it
                                  ;; covers has to be new.  A dragged window fails the first: its
                                  ;; damage is the union of two rectangles, a bit over half the
                                  ;; screen — huge as a bounding box, and no more than that.  It
                                  ;; can still fail the second when the drag is slow enough for
                                  ;; the vector to carry it, but it must not be able to trip the
                                  ;; rule on area alone, because area is precisely the number the
                                  ;; motion vector has already made cheap.
                                  ((and y (not resized) (> goal sc)
                                        (> nwant (* 0.7 mbs))
                                        (>= nnew (* (scale-drop-fraction bps full-mbs) mbs))
                                        (>= dwell *scale-drop-dwell-secs*))
                                   (setf sc goal scale-at tn lost-run 0 climbing nil)
                                   (incf scale-drops) t)
                                  ;; the search has lost the picture and no rate will find it
                                  ;; again — halve, which halves the displacement too.  One step
                                  ;; at a time here, because unlike the rule above this one does
                                  ;; not know how far short it is; if half is still not enough the
                                  ;; run simply builds again and it halves again.
                                  ((and y (not resized) (< sc 4)
                                        (>= lost-run *scale-lost-frames*)
                                        (>= dwell *scale-drop-dwell-secs*))
                                   (setf sc (min 4 (* sc 2)) scale-at tn lost-run 0 climbing nil)
                                   (incf scale-drops) t)
                                  ;; the screen has been still long enough that the full-size
                                  ;; keyframe is worth its price.  Straight back to 1: there is no
                                  ;; reason to arrive at half resolution on the way to full.
                                  ((and (> sc 1) (null y)
                                        (>= quiet (scale-restore-secs bps full-mbs)))
                                   (setf sc 1 scale-at tn lost-run 0 climbing nil)
                                   (incf scale-ups) t)
                                  (t nil))))
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
                            ;; THE MOTION CEILING, WHICH IS NOT THE SETTLING ONE.  MAX-QI says
                            ;; how coarse the picture is allowed to STAY; this says how coarse
                            ;; one frame may be while the screen is moving, and it has to reach
                            ;; the coarsest the format has at every rate or motion cannot be
                            ;; made cheap.  See the account in START-VIDEO's docstring: with
                            ;; MAX-QI doing both jobs, every rung from 160 kbps up was capped at
                            ;; 52 during a scroll, nothing in [QI, 52] ever fit, and every frame
                            ;; took the overrun path below and spent up to LINK-CEILING.
                            (mo-qi (max max-qi (min 127 motion-qi)))
                            (fit (multiple-value-list
                                  (fit-frame nnew bpu live-overhead qi mo-qi
                                             mo-ceiling link-ceiling)))
                            (fq (first fit))
                            (ceiling-bytes (second fit))
                            (fits (third fit))
                            ;; SHARPEN SLOWLY, COARSEN AT ONCE.  FQ is whatever this frame's
                            ;; change happens to fit in, and once the ceiling is unclamped that
                            ;; can be 127 on one frame and QI on the next — the picture pumping
                            ;; between mush and detail while the user is still scrolling.  So a
                            ;; live frame may only step *finer* by *MOTION-QI-EASE* per frame
                            ;; while the screen is moving; going coarser is never delayed,
                            ;; because a delayed coarsening is exactly the lag being removed.
                            ;; MOVING-QI is reset to QI whenever the screen goes quiet, so the
                            ;; ease never stands between the refinement pass and its target.
                            (useq (let ((want (max qi (min mo-qi (+ fq qi-bias)))))
                                    (if (and (plusp *motion-qi-ease*)
                                             (< want moving-qi)
                                             (> moving-qi qi))
                                        (max want (- moving-qi *motion-qi-ease*))
                                        want)))
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
                            (key-cheaper (and enc y (not resized) (> nwant (* 0.8 mbs))
                                              (or (< nmv (floor nwant 4))
                                                  (> (+ mv-bytes (* nnew bpu (qi-cost useq)))
                                                     (* mbs kpu (qi-cost kqi))))))
                            (need-key (or (null enc) resized forced key-due key-cheaper))
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
                                ;; the resolution just changed: whatever is in hand is the wrong
                                ;; size, and so is anything the idle branch would have refined
                                (switched nil)
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
                                         ;; NOTE the budget: both jobs below are bounded by the
                                         ;; LIVE frame budget times *IDLE-FRAME-X*, not by the
                                         ;; link-time ceiling.  Nothing is waiting behind them
                                         ;; WHILE THE SCREEN IS IDLE, and the screen stops being
                                         ;; idle without warning; a frame already handed to the
                                         ;; pacer cannot be recalled, so its size is the floor on
                                         ;; how long the next real change waits.  Both jobs resume
                                         ;; exactly where they left off, so being cut short costs
                                         ;; one frame's overhead and nothing else.
                                         ((and enc pending (position 1 pending))
                                          (let* ((ns (count 1 pending)))
                                            (multiple-value-bind (dq fits)
                                                (fit-quantizer ns idle-room bpu overhead
                                                               qi max-qi)
                                              (multiple-value-bind (f nmb defer mv eqi)
                                                  (with-timing (st-enc)
                                                    (vp8::encode-inter-frame
                                                     enc (vp8::ve-prev-y enc) (vp8::ve-prev-u enc)
                                                     (vp8::ve-prev-v enc)
                                                     :qi dq :dirty pending :force t :clean-qi qi
                                                     :max-bytes idle-room
                                                     :max-mbs (unless fits
                                                                (mbs-fitting idle-room bpu
                                                                             overhead dq))))
                                                (declare (ignore mv))
                                                (setf pending defer)
                                                (when eqi
                                                  (setf cur-qi eqi
                                                        stale-qi (if (> eqi qi)
                                                                     (max stale-qi eqi) stale-qi)))
                                                (when nmb (ps-note st-mbs nmb))
                                                f))))
                                         ;; NOT WHILE THE PICTURE IS THE WRONG SIZE.  At a reduced
                                         ;; resolution a quiet screen is on its way back to full
                                         ;; (see the restore rule), and every byte spent sharpening
                                         ;; the small picture is a byte spent on pixels the
                                         ;; restoring keyframe is about to overwrite — and spent
                                         ;; ahead of it, so it delays the thing it wastes itself
                                         ;; on.  Sharpening resumes once the size is settled.
                                         ;; SC-CODED as well as SC, because between asking for a
                                         ;; size and holding a picture at it there are passes
                                         ;; where the two disagree — and in exactly those passes
                                         ;; the encoder still holds the OLD size, so this would
                                         ;; sharpen the picture that is being replaced.  Measured
                                         ;; on the session-start climb at 40 kbps: 15 KB, three
                                         ;; seconds of link, spent making a half-size picture
                                         ;; pretty immediately before a full-size keyframe
                                         ;; overwrote all of it.
                                         ((and (= sc 1) (= sc-coded 1) st (position 1 st)
                                               (> idle-ms cleanup-ms))
                                          (incf cleanups)
                                          ;; A SUBSET OF THE SCREEN, ALL THE WAY TO QI — never the
                                          ;; whole screen a quantizer step at a time.  Both spend
                                          ;; the same bytes per frame and they are not close.
                                          ;;
                                          ;; A step re-codes EVERY stale macroblock on every pass.
                                          ;; The cost model says that is free — it prices a step as
                                          ;; the difference between two quantizers, and differences
                                          ;; telescope — but the model is describing a residual, and
                                          ;; the encoder is re-sending a whole macroblock: its
                                          ;; modes, its tokens, and the quantization error the last
                                          ;; pass left for this one to undo.  Take a macroblock down
                                          ;; in thirty steps and it is coded thirty times.
                                          ;;
                                          ;; A subset codes each macroblock ONCE, at the target, and
                                          ;; pays only the frame overhead for being spread out.
                                          ;; Measured from a coarse keyframe to the rung's own
                                          ;; quantizer, same per-frame budget, subset -> step:
                                          ;; 85 KB / 2.0 s -> 615 KB / 14.5 s at 320 kbps, 200 KB /
                                          ;; 9.9 s -> 754 KB / 35.8 s at 160, 137 / 13.3 -> 550 /
                                          ;; 48.4 at 80, 58 / 10.9 -> 318 / 49.9 at 40.  And the
                                          ;; step arm ends up a WORSE picture (50.4 dB against 52.6
                                          ;; at 320), because it stops when it runs out of steps
                                          ;; rather than when it reaches QI.
                                          ;;
                                          ;; The visible difference is a band of sharpness sweeping
                                          ;; down the screen instead of the whole picture lifting at
                                          ;; once.  It sweeps several times faster.
                                          (let* ((ns (count 1 st))
                                                 ;; what the viewer already has, when it has
                                                 ;; anything: a macroblock stale at QI itself was
                                                 ;; never sent at all, and costs full price
                                                 (from (and (> stale-qi qi) stale-qi))
                                                 ;; how much of the stale set this frame can take,
                                                 ;; and NIL for "all of it" — the estimate is a mean
                                                 ;; over macroblocks that are not alike, so it is
                                                 ;; the byte bound below, not this, that keeps the
                                                 ;; promise
                                                 (fit (mbs-fitting idle-room rpu overhead qi from))
                                                 (cap (when (< fit ns) fit)))
                                            (setf cur-qi qi)
                                            ;; the set is only settled when the pass covers all of
                                            ;; it; a slice leaves the rest exactly as coarse as it
                                            ;; was, which is what FROM has to keep saying
                                            (unless cap (setf stale-qi qi))
                                            (ps-note st-mbs (if cap (min ns cap) ns))
                                            (multiple-value-bind (f nmb)
                                                (with-timing (st-clean)
                                                  (vp8::encode-inter-frame
                                                   enc (vp8::ve-prev-y enc)
                                                   (vp8::ve-prev-u enc) (vp8::ve-prev-v enc)
                                                   :qi qi :dirty (copy-seq st) :force t
                                                   :clean-qi qi :max-mbs cap
                                                   :max-bytes idle-room))
                                              ;; refinement pays for its own estimate: its residual
                                              ;; is nothing like a live frame's.  NMB is what the
                                              ;; encoder actually coded, which under the byte bound
                                              ;; is what the bound allowed rather than what CAP
                                              ;; asked for — so this learns from the frame that
                                              ;; happened and the next CAP is closer.
                                              (when (and f nmb (>= nmb 16))
                                                (let ((m (/ (max 1d0 (- (length f) overhead))
                                                            nmb
                                                            (qi-step-cost qi from))))
                                                  (setf rpu (if (> m rpu)
                                                                (+ (* 0.4d0 rpu) (* 0.6d0 m))
                                                                (+ (* 0.85d0 rpu)
                                                                   (* 0.15d0 m))))))
                                              f)))))))
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
                                              (incf deferrals)
                                              (mbs-fitting ceiling-bytes bpu live-overhead useq))))
                                   ;; THE FALLBACK, COUNTED WHERE IT ACTUALLY HAPPENS.  FIT-FRAME
                                   ;; hands back the LATENCY ceiling instead of the frame-rate
                                   ;; budget when no quantizer it was allowed could cover the
                                   ;; change — that is the "spend up to the ceiling rather than
                                   ;; defer" path, and it is the one that used to fire on every
                                   ;; frame of every scroll above 160 kbps.  It is NOT the same
                                   ;; thing as FITS, which asks the much rarer question of
                                   ;; whether even the ceiling is too small; that one defers, and
                                   ;; is counted just above.
                                   (when (> ceiling-bytes mo-ceiling) (incf overruns))
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
                                               ;; what the ease measures its next step from, and
                                               ;; it is the quantizer the ENCODER settled on
                                               ;; (EQI above), not the one we asked for
                                               moving-qi useq
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
                       ;; THE EASE IS A MOTION RULE AND THE MOTION IS OVER.  *MOTION-QI-EASE*
                       ;; slows how fast a live frame may sharpen so a scroll does not pump; the
                       ;; refinement pass that follows a scroll is not a live frame and must not
                       ;; inherit its brake, or the arc back to QI would take a quantizer step at
                       ;; a time.  Same gate the idle passes use, for the same reason.
                       (when (> idle-ms cleanup-ms) (setf moving-qi qi))
                       ;; publish the resolution the source should hand us next.  The source reads
                       ;; it when it next produces a frame, so the switch costs one poll, not a
                       ;; restart — the same shape as *VIDEO-PROFILE*.
                       (setf *video-scale* sc)
                       ;; HAS THE SEARCH LOST THE PICTURE?  Counted from KEY-CHEAPER — the rule
                       ;; that actually turns a moving screen into a keyframe — because that is
                       ;; the thing we need to escape, and it has two halves: no usable vector at
                       ;; all, and a vector that accounts for too little to be worth an inter
                       ;; frame.  A test written on the vector count alone sees only the first.
                       ;; Measured with such a test: sixteen consecutive 6 KB keyframes at half
                       ;; resolution, not one of them counted, because the vector was carrying a
                       ;; fifth of the screen — not enough to avoid the keyframe, quite enough to
                       ;; look like the search was working.
                       ;;
                       ;; Here rather than in the decision above because KEY-CHEAPER is not known
                       ;; until the frame has been priced.  The policy therefore reads a count of
                       ;; what already happened, which is the right tense for it.  Reset once the
                       ;; screen has been still a moment: that episode is over.
                       (unless switched
                         (cond ((null y)
                                (when (> (/ (float (- (get-internal-real-time) idle-since))
                                            internal-time-units-per-second)
                                         1d0)
                                  (setf lost-run 0)))
                               (resized)
                               (key-cheaper (incf lost-run))
                               (t (setf lost-run 0))))
                       (when (and switched log)
                         (funcall log (format nil "resolution 1/~a — ~a (~a of ~a mbs new, ~a moved)"
                                              sc (cond ((and climbing y) "first picture, fast")
                                                       (climbing "climbing")
                                                       (y "damage is global")
                                                       (t "screen is settled"))
                                              nnew mbs nmv)))
                       ;; carry the frame-rate plan out to the report.  FPS-CAP is the number
                       ;; worth watching: when the cadence we asked for is above it, the rung is
                       ;; not short of BITS, it is short of macroblocks to spend them on, and no
                       ;; amount of bitrate will make the picture move any faster at this size.
                       (setf fps-want feff fps-cap fcap frame-budget budget idle-budget idle-room)
                       (if frame
                           (progn
                             (setf sc-coded sc)
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
                                      :idle-budget idle-budget
                                      ;; the resolution divisor now in force, and how often it has
                                      ;; moved: a rung that thrashes here is a rung whose
                                      ;; hysteresis is wrong, and it is invisible in every other
                                      ;; number the sender reports
                                      :scale sc :scale-drops scale-drops :scale-ups scale-ups
                                      :qi cur-qi :qi-bias qi-bias
                                      :bytes-per-mb (* bpu (qi-cost cur-qi))
                                      :moved-mb (list (ps-min st-mv) (/ (ps-total st-mv) (max 1 (ps-n st-mv))) (ps-max st-mv))
                                      :bytes-per-moved-mb mpu
                                      :pending (if pending (count 1 pending) 0)
                                      ;; live frames that could not be covered inside their
                                      ;; budget even at the MOTION ceiling and took the latency
                                      ;; ceiling instead.  Supposed to be rare; when it is not,
                                      ;; the rung is short of macroblocks rather than of
                                      ;; quantizer, and only resolution moves it.
                                      :overruns overruns :deferrals deferrals
                                      :cleanups cleanups)))
                     (when (and log (> dt 5000))
                       (funcall log (format nil "~a | ~a | ~a | ~a | ~a | ~a | ~a | sent ~a idle ~a seen ~a | ~,1f KB/s | ~,1f fps (want ~,1f, flags allow ~,1f, cap ~,1f) frame<=~a B idle<=~a B | 1/~a res (~a down ~a up) | qi ~a~@[+~a~] b/mb ~,0f mv-b/mb ~,1f | pend ~a | over ~a defer ~a | cleans ~a"
                                            (ps-line st-take) (ps-line st-enc) (ps-line st-clean)
                                            (ps-line st-send) (ps-line st-kb) (ps-line st-mbs)
                                            (ps-line st-mv)
                                            sent skipped takes (/ bytes 1.024 dt)
                                            (/ (* 1000d0 sent) dt)
                                            fps-want fps-cap (/ (* 1000d0 takes) dt) frame-budget idle-budget
                                            sc scale-drops scale-ups
                                            cur-qi (and (plusp qi-bias) qi-bias)
                                            (* bpu (qi-cost cur-qi)) mpu
                                            (if pending (count 1 pending) 0) overruns deferrals cleanups))
                       (ps-reset st-take) (ps-reset st-enc) (ps-reset st-send) (ps-reset st-loop)
                       (ps-reset st-clean) (ps-reset st-kb) (ps-reset st-mbs) (ps-reset st-qi)
                       (ps-reset st-mv) (ps-reset st-lag)
                       (setf sent 0 skipped 0 bytes 0 takes 0 cleanups 0 overruns 0 deferrals 0
                             last-report (get-internal-real-time)))))
               (error (e) (when log (funcall log (format nil "video tx: ~a" e)))))
             ;; poll on a short tick: the capture clock and the link are both checked here, and
             ;; neither of them is allowed to become the other's pacing.
             (sleep poll-secs))))
       :name "wm-video-tx")
      (lambda () (setf running nil)))))
