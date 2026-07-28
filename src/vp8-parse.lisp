;;;; vp8-parse.lisp — a VP8 keyframe PARSER, built on the bit-exact bool decoder.
;;;;
;;;; This is a debugging oracle, not a decoder: it reads a keyframe's headers and token partition
;;;; and reports what it finds.  Pointed at a libvpx-produced frame it tells us whether our
;;;; understanding of the bitstream matches a real encoder's; pointed at our own frame it shows
;;;; what a reader actually sees.  Ground truth beats guessing — every VP8 fix so far came from it.
;;;;
;;;; What it has already established:
;;;;   * it parses a real libvpx keyframe cleanly (headers, quantizer, modes, coefficients), so our
;;;;     structural model of the bitstream is right
;;;;   * it reads OUR frames back with the values we intended (a gradient MB's Y2 DC of -227 is
;;;;     exactly the -68 luma residual we meant), so our writer and a spec-following reader agree
;;;;   * libvpx's own flat-200 frame carries Y2 DC = 240 at qi=4 and decodes to exactly 200, while
;;;;     the same arithmetic says 188 — an INDEPENDENT confirmation of the ~1.2x modelling error
;;;;     captured in +y2-dc-fudge+ (so that constant is compensating something real, not papering
;;;;     over a coding mistake)
;;;;
;;;; Still open: blocks with AC coefficients decode wrong even though this parser reads them back
;;;; correctly.  Since writer and reader agree but ffmpeg disagrees, the fault is a shared
;;;; misunderstanding.  The 1.2x discrepancy is the strongest lead — resolving where it comes from
;;;; (likely the same place) should fix the AC path too.

(in-package #:webrtc-media.vp8)

;;; vp8_coef_tree as (node -> (bit0-target bit1-target)), negative = -token
(defparameter +coef-tree+
  #(-11 2  -0 4  -1 6  8 12  -2 10  -3 -4  14 16  -5 -6  18 20  -7 -8  -9 -10))

(defun read-token (br probs type band ctx)
  "Walk vp8_coef_tree with the [type][band][ctx] probabilities; returns the token id."
  (let ((i 0))
    (loop
      (let* ((node (ash i -1))
             (bit (bread-bit br (coef-prob probs type band ctx node)))
             (next (aref +coef-tree+ (+ i bit))))
        (if (<= next 0)
            (return (- next))
            (setf i next))))))

(defun read-coef-value (br token)
  "Given TOKEN, read any category extra bits + the sign; returns the signed coefficient."
  (let ((mag (case token
               (0 0) (1 1) (2 2) (3 3) (4 4)
               (t (let* ((c (- token +tok-cat1+))
                         (ps (aref +cat-probs+ c))
                         (extra 0))
                    (loop for i from 0 below (length ps)
                          do (setf extra (logior (ash extra 1) (bread-bit br (aref ps i)))))
                    (+ (aref +cat-base+ c) extra))))))
    (if (zerop mag) 0 (if (= 1 (bread-bit br 128)) (- mag) mag))))

(defun read-block (br probs type first-coef ctx)
  "Read one block's coefficients; returns (values coefficient-vector any-nonzero)."
  (let ((out (make-array 16 :initial-element 0)) (c ctx) (nz nil))
    (loop for i from first-coef below 16 do
      (let ((tok (read-token br probs type (aref +coef-bands+ i) c)))
        (when (= tok +tok-eob+) (return))
        (let ((v (read-coef-value br tok)))
          (setf (aref out i) v)
          (unless (zerop v) (setf nz t))
          (setf c (cond ((zerop v) 0) ((= 1 (abs v)) 1) (t 2))))))
    (values out nz)))

(defun parse-keyframe (bytes &key (max-blocks 8) (verbose t))
  "Parse a VP8 keyframe: uncompressed header, frame header, per-MB modes, and the first few
coefficient blocks.  Returns a plist of what was found."
  (let* ((tag (logior (aref bytes 0) (ash (aref bytes 1) 8) (ash (aref bytes 2) 16)))
         (keyframe (zerop (logand tag 1)))
         (version (logand (ash tag -1) 7))
         (show (logbitp 4 tag))
         (part1-size (ash tag -5))
         (width (logior (aref bytes 6) (ash (logand (aref bytes 7) #x3f) 8)))
         (height (logior (aref bytes 8) (ash (logand (aref bytes 9) #x3f) 8)))
         (p1 (subseq bytes 10 (+ 10 part1-size)))
         (br (make-breader* p1)))
    (when verbose
      (format t "~&== uncompressed: keyframe=~a version=~a show=~a part1=~a ~ax~a~%"
              keyframe version show part1-size width height))
    (let* ((color (bread-literal br 1)) (clamp (bread-literal br 1))
           (seg (bread-literal br 1)))
      (when (plusp seg) (format t "!! segmentation enabled — parser stops (not handled)~%")
        (return-from parse-keyframe nil))
      (let* ((ftype (bread-literal br 1)) (flevel (bread-literal br 6)) (sharp (bread-literal br 3))
             (lfadj (bread-literal br 1)))
        (when (plusp lfadj)
          ;; mode_ref_lf_delta_update
          (when (plusp (bread-literal br 1))
            (dotimes (i 8) (when (plusp (bread-literal br 1))
                             (bread-literal br 6) (bread-literal br 1)))))
        (let* ((nparts (ash 1 (bread-literal br 2)))
               (qi (bread-literal br 7))
               (deltas (loop repeat 5 collect
                             (if (plusp (bread-literal br 1))
                                 (let ((m (bread-literal br 4)) (s (bread-literal br 1)))
                                   (if (plusp s) (- m) m))
                                 0)))
               (refresh (bread-literal br 1))
               (updates 0))
          (when verbose
            (format t "== header: color=~a clamp=~a filter(type=~a level=~a sharp=~a) parts=~a~%"
                    color clamp ftype flevel sharp nparts)
            (format t "== quant: y_ac_qi=~a deltas=~a refresh_entropy=~a~%" qi deltas refresh))
          ;; token probability updates
          (let ((probs (copy-seq +default-coef-probs+)))
            (dotimes (i 1056)
              (when (= 1 (bread-bit br (aref +coeff-update-probs+ i)))
                (incf updates)
                (setf (aref probs i) (bread-literal br 8))))
            (when verbose (format t "== token prob updates: ~a of 1056~%" updates))
            (let* ((no-skip (bread-literal br 1))
                   (prob-skip (if (plusp no-skip) (bread-literal br 8) 0))
                   (mb-cols (ceiling width 16)) (mb-rows (ceiling height 16))
                   (modes '()) (skips '()))
              (when verbose (format t "== mb_no_skip_coeff=~a prob_skip=~a  MBs=~ax~a~%"
                                    no-skip prob-skip mb-cols mb-rows))
              ;; per-MB records
              (dotimes (i (* mb-cols mb-rows))
                (let ((skip (when (plusp no-skip) (bread-bit br prob-skip))))
                  (push skip skips)
                  ;; kf ymode tree
                  (let ((ymode
                          (if (= 0 (bread-bit br (aref +kf-ymode-prob+ 0))) :b-pred
                              (if (= 0 (bread-bit br (aref +kf-ymode-prob+ 1)))
                                  (if (= 0 (bread-bit br (aref +kf-ymode-prob+ 2))) :dc :v)
                                  (if (= 0 (bread-bit br (aref +kf-ymode-prob+ 3))) :h :tm)))))
                    (push ymode modes)
                    (when (eq ymode :b-pred)
                      (when verbose (format t "!! B_PRED macroblock — parser stops~%"))
                      (return-from parse-keyframe nil))
                    ;; uv mode
                    (if (= 0 (bread-bit br (aref +kf-uv-mode-prob+ 0))) :dc
                        (if (= 0 (bread-bit br (aref +kf-uv-mode-prob+ 1))) :v
                            (if (= 0 (bread-bit br (aref +kf-uv-mode-prob+ 2))) :h :tm))))))
              (setf modes (nreverse modes) skips (nreverse skips))
              (when verbose
                (format t "== modes: first 8 = ~a  skips = ~a~%"
                        (subseq modes 0 (min 8 (length modes)))
                        (subseq skips 0 (min 8 (length skips)))))
              ;; ---- token partition ----
              (let* ((rest (subseq bytes (+ 10 part1-size)))
                     (tbr (make-breader* rest)))
                (when verbose (format t "== token partition: ~a bytes; first blocks:~%" (length rest)))
                (let ((shown 0))
                  (multiple-value-bind (y2 nz) (read-block tbr probs +blk-y2+ 0 0)
                    (declare (ignore nz))
                    (when verbose (format t "   Y2 : ~a~%" (subseq y2 0 8))))
                  (dotimes (b 16)
                    (multiple-value-bind (blk nz) (read-block tbr probs +blk-y-after-y2+ 1 0)
                      (declare (ignore nz))
                      (when (and verbose (< shown max-blocks))
                        (format t "   Y~2,'0d: ~a~%" b (subseq blk 0 8)) (incf shown))))))
              (list :qi qi :deltas deltas :updates updates :nparts nparts
                    :filter-level flevel :modes modes))))))))
