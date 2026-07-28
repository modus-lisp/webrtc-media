;;;; vp8-tokens.lisp — VP8 coefficient token coding (RFC 6386 §13).
;;;;
;;;; Coefficients are coded by walking vp8_coef_tree with probabilities selected by
;;;; [block-type][coefficient-band][context], where context is 0/1/2 from how many of the
;;;; neighbouring (left/above) blocks had non-zero coefficients.  Tokens above FOUR carry
;;;; "extra bits" (a category) plus a sign.  We implement the tokens needed for small
;;;; coefficients (through category 6).
;;;;
;;;; STATUS: WIP.  Positive luma DC round-trips through ffmpeg correctly; negative DC and chroma
;;;; do not.  Cause: VP8 selects probabilities by an entropy CONTEXT derived from whether the
;;;; left/above neighbouring blocks had non-zero coefficients, and we currently pass ctx=0
;;;; unconditionally — so the decoder reads with different probabilities than we wrote.  Needs
;;;; the per-plane left/above context arrays maintained across blocks and macroblocks.

(in-package #:webrtc-media.vp8)

;;; token ids (libvpx entropy.h)
(defconstant +tok-zero+ 0) (defconstant +tok-one+ 1) (defconstant +tok-two+ 2)
(defconstant +tok-three+ 3) (defconstant +tok-four+ 4)
(defconstant +tok-cat1+ 5) (defconstant +tok-cat2+ 6) (defconstant +tok-cat3+ 7)
(defconstant +tok-cat4+ 8) (defconstant +tok-cat5+ 9) (defconstant +tok-cat6+ 10)
(defconstant +tok-eob+ 11)

;;; Path through vp8_coef_tree for each token: list of (node-index . bit).
;;; Tree nodes (from libvpx): 0:EOB/2  1:ZERO/4  2:ONE/6  3:{8,12}  4:TWO/10  5:{THREE,FOUR}
;;;                           6:{14,16} 7:{CAT1,CAT2} 8:{18,20} 9:{CAT3,CAT4} 10:{CAT5,CAT6}
(defparameter +token-paths+
  (vector '((0 . 1) (1 . 0))                                   ; ZERO
          '((0 . 1) (1 . 1) (2 . 0))                           ; ONE
          '((0 . 1) (1 . 1) (2 . 1) (3 . 0) (4 . 0))           ; TWO
          '((0 . 1) (1 . 1) (2 . 1) (3 . 0) (4 . 1) (5 . 0))   ; THREE
          '((0 . 1) (1 . 1) (2 . 1) (3 . 0) (4 . 1) (5 . 1))   ; FOUR
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 0) (7 . 0))   ; CAT1
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 0) (7 . 1))   ; CAT2
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 1) (8 . 0) (9 . 0))   ; CAT3
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 1) (8 . 0) (9 . 1))   ; CAT4
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 1) (8 . 1) (10 . 0))  ; CAT5
          '((0 . 1) (1 . 1) (2 . 1) (3 . 1) (6 . 1) (8 . 1) (10 . 1))  ; CAT6
          '((0 . 0))))                                         ; EOB

;;; category extra-bit probabilities + base values (RFC 6386 §13.2)
(defparameter +cat-probs+
  (vector #(159) #(165 145) #(173 148 140) #(176 155 140 135)
          #(180 157 141 134 130) #(254 254 243 230 196 177 153 140 133 130 129)))
(defparameter +cat-base+ (vector 5 7 11 19 35 67))

(defun coef-prob (probs block-type band ctx node)
  "Index the [4][8][3][11] coefficient probability table."
  (aref probs (+ (* block-type 8 3 11) (* band 3 11) (* ctx 11) node)))

(defun write-token (bw probs block-type band ctx token)
  "Code TOKEN by walking vp8_coef_tree with the [type][band][ctx] probabilities."
  (dolist (step (aref +token-paths+ token))
    (bwrite-bit bw (coef-prob probs block-type band ctx (car step)) (cdr step))))

(defun write-coef (bw probs block-type band ctx value)
  "Code one coefficient VALUE (already quantized, may be negative): its token, any category
extra bits, then the sign.  Returns the token used."
  (let* ((mag (abs value))
         (token (cond ((= mag 0) +tok-zero+) ((= mag 1) +tok-one+) ((= mag 2) +tok-two+)
                      ((= mag 3) +tok-three+) ((= mag 4) +tok-four+)
                      ((<= mag 6) +tok-cat1+) ((<= mag 10) +tok-cat2+) ((<= mag 18) +tok-cat3+)
                      ((<= mag 34) +tok-cat4+) ((<= mag 66) +tok-cat5+) (t +tok-cat6+))))
    (write-token bw probs block-type band ctx token)
    (when (>= token +tok-cat1+)                       ; extra bits, MSB first
      (let* ((c (- token +tok-cat1+))
             (ps (aref +cat-probs+ c))
             (extra (- mag (aref +cat-base+ c))))
        ;; extra bits: MSB first, probability ps[0] first (RFC 6386 §13.2 / libvpx)
        (loop for i from 0 below (length ps)
              do (bwrite-bit bw (aref ps i) (logand (ash extra (- (- (length ps) 1 i))) 1)))))
    ;; WIP: sign handling is not yet right — positive DC decodes correctly with this
    ;; polarity but negative does not, which points at the entropy-context modelling below
    ;; rather than the sign bit itself.
    (unless (zerop mag) (bwrite-flag bw (minusp value)))   ; RFC 6386: 1 = negative
    token))

(defun write-dc-only-block (bw probs block-type dc ctx)
  "Code a block whose only non-zero coefficient is DC: the DC token (band 0), then EOB.
Returns the context for the next block (1 if this block had any non-zero coefficient)."
  (write-coef bw probs block-type 0 ctx dc)
  (unless (zerop dc)                                  ; EOB after DC (band 1, ctx 0 after a 1-token)
    (write-token bw probs block-type 1 (if (= (abs dc) 1) 1 2) +tok-eob+))
  (if (zerop dc) 0 1))
