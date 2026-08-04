;;;; vp8-tokens.lisp — VP8 coefficient token coding (RFC 6386 §13).
;;;;
;;;; Coefficients are coded by walking vp8_coef_tree with probabilities selected by
;;;; [block-type][coefficient-band][context], where context is 0/1/2 from how many of the
;;;; neighbouring (left/above) blocks had non-zero coefficients.  Tokens above FOUR carry
;;;; "extra bits" (a category) plus a sign.  We implement the tokens needed for small
;;;; coefficients (through category 6).
;;;;
;;;; One non-obvious rule (libvpx encoder/tokenize.c `skip_eob_node', decoder/detokenize.c
;;;; `GetCoeffs'): the EOB branch — tree node 0 — is only coded for the block's first coefficient
;;;; and after a NON-ZERO one.  After a ZERO coefficient the decoder does not read it, so neither
;;;; may we.  Hence the SKIP-EOB argument threaded through WRITE-TOKEN / WRITE-COEF.

(in-package #:webrtc-media.vp8)

;;; token ids (libvpx entropy.h)
(defconstant +tok-zero+ 0) (defconstant +tok-one+ 1) (defconstant +tok-two+ 2)
(defconstant +tok-three+ 3) (defconstant +tok-four+ 4)
(defconstant +tok-cat1+ 5) (defconstant +tok-cat2+ 6) (defconstant +tok-cat3+ 7)
(defconstant +tok-cat4+ 8) (defconstant +tok-cat5+ 9) (defconstant +tok-cat6+ 10)
(defconstant +tok-eob+ 11)

;;; Path through vp8_coef_tree for each token.  Each step is one byte, (node << 1) | bit — a flat
;;; specialized array rather than a list of conses, because WRITE-TOKEN is the single hottest
;;; caller in the codec (a keyframe walks it ~200k times) and a cons list costs a pointer chase and
;;; two boxed reads per step where a byte array costs an indexed load.
;;; Tree nodes (from libvpx): 0:EOB/2  1:ZERO/4  2:ONE/6  3:{8,12}  4:TWO/10  5:{THREE,FOUR}
;;;                           6:{14,16} 7:{CAT1,CAT2} 8:{18,20} 9:{CAT3,CAT4} 10:{CAT5,CAT6}
(defparameter +token-paths+
  (map 'simple-vector
       (lambda (steps)
         (map '(simple-array (unsigned-byte 8) (*))
              (lambda (s) (logior (ash (car s) 1) (cdr s))) steps))
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
               '((0 . 0)))))                                        ; EOB
(declaim (type simple-vector +token-paths+))

;;; category extra-bit probabilities + base values (RFC 6386 §13.2)
(defparameter +cat-probs+
  (map 'simple-vector (lambda (v) (coerce v '(simple-array (unsigned-byte 8) (*))))
       (vector #(159) #(165 145) #(173 148 140) #(176 155 140 135)
               #(180 157 141 134 130) #(254 254 243 230 196 177 153 140 133 130 129))))
(declaim (type simple-vector +cat-probs+))
(defparameter +cat-base+
  (make-array 6 :element-type '(unsigned-byte 8) :initial-contents '(5 7 11 19 35 67)))
(declaim (type (simple-array (unsigned-byte 8) (6)) +cat-base+))

(declaim (inline coef-base))
(defun coef-base (block-type band ctx)
  "Where this [type][band][ctx] triple's eleven node probabilities start in the [4][8][3][11]
table.  Hoisted out of the tree walk: the triple is fixed for a whole token, only the node moves."
  (declare (type (integer 0 3) block-type) (type (integer 0 7) band) (type (integer 0 2) ctx)
           (optimize (speed 3) (safety 0)))
  (+ (* block-type 264) (* band 33) (* ctx 11)))

(declaim (inline coef-prob))
(defun coef-prob (probs block-type band ctx node)
  "Index the [4][8][3][11] coefficient probability table."
  (declare (type (simple-array (unsigned-byte 8) (1056)) probs)
           (type (integer 0 10) node) (optimize (speed 3) (safety 0)))
  (aref probs (+ (coef-base block-type band ctx) node)))

(defun write-token (bw probs block-type band ctx token &optional skip-eob)
  "Code TOKEN by walking vp8_coef_tree with the [type][band][ctx] probabilities.

SKIP-EOB is libvpx's `skip_eob_node`: after a ZERO token the decoder does NOT test the
EOB branch at all (detokenize.c GetCoeffs jumps straight back to the ZERO test), so the
encoder must not emit tree node 0's bit either.  See tokenize.c: t->skip_eob_node = (pt == 0)."
  (declare (type (simple-array (unsigned-byte 8) (1056)) probs)
           (type (integer 0 3) block-type) (type (integer 0 7) band) (type (integer 0 2) ctx)
           (type (integer 0 11) token) (optimize (speed 3) (safety 0)))
  (let ((path (the (simple-array (unsigned-byte 8) (*)) (svref +token-paths+ token)))
        (base (coef-base block-type band ctx)))
    (declare (type fixnum base))
    (dotimes (i (length path))
      (let* ((s (aref path i)) (node (ash s -1)))
        (unless (and skip-eob (= 0 node))
          (bwrite-bit bw (aref probs (+ base node)) (logand s 1)))))))

(defun write-coef (bw probs block-type band ctx value &optional skip-eob)
  "Code one coefficient VALUE (already quantized, may be negative): its token, any category
extra bits, then the sign.  Returns the token used."
  (declare (type (simple-array (unsigned-byte 8) (1056)) probs)
           (type (integer 0 3) block-type) (type (integer 0 7) band) (type (integer 0 2) ctx)
           (type (signed-byte 32) value) (optimize (speed 3) (safety 0)))
  (let* ((mag (abs value))
         (token (cond ((= mag 0) +tok-zero+) ((= mag 1) +tok-one+) ((= mag 2) +tok-two+)
                      ((= mag 3) +tok-three+) ((= mag 4) +tok-four+)
                      ((<= mag 6) +tok-cat1+) ((<= mag 10) +tok-cat2+) ((<= mag 18) +tok-cat3+)
                      ((<= mag 34) +tok-cat4+) ((<= mag 66) +tok-cat5+) (t +tok-cat6+))))
    (declare (type (integer 0 2147483647) mag))
    (write-token bw probs block-type band ctx token skip-eob)
    (when (>= token +tok-cat1+)                       ; extra bits, MSB first
      (let* ((c (- token +tok-cat1+))
             (ps (the (simple-array (unsigned-byte 8) (*)) (svref +cat-probs+ c)))
             (extra (- mag (aref +cat-base+ c)))
             (n (length ps)))
        (declare (type fixnum extra n))
        ;; extra bits: MSB first, probability ps[0] first (RFC 6386 §13.2 / libvpx)
        (dotimes (i n)
          (bwrite-bit bw (aref ps i) (logand (ash extra (- (- n 1 i))) 1)))))
    (unless (zerop mag) (bwrite-flag bw (minusp value)))   ; RFC 6386: 1 = negative
    token))

(defun write-dc-only-block (bw probs block-type dc ctx)
  "Code a block whose only non-zero coefficient is DC: the DC token (band 0), then EOB.
Returns the context for the next block (1 if this block had any non-zero coefficient)."
  (cond ((zerop dc)
         ;; an all-zero block is a bare EOB at band 0 — NOT a ZERO token, which would leave the
         ;; decoder still reading coefficients and desynchronise the whole token partition
         (write-token bw probs block-type 0 ctx +tok-eob+)
         0)
        (t
         (write-coef bw probs block-type 0 ctx dc)
         (write-token bw probs block-type 1 (if (= (abs dc) 1) 1 2) +tok-eob+)
         1)))
