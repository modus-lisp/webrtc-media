;;;; vp8-bool.lisp — VP8's boolean entropy coder (RFC 6386 §7 decoder, §13.2 encoder).
;;;;
;;;; A binary range coder: every syntax element in a VP8 frame is coded as a series of bool
;;;; decisions, each with an 8-bit probability (of the bit being 0).  This is THE primitive the
;;;; whole codec is built on, so it has to be bit-exact — hence a round-trip test against our own
;;;; decoder.  Ported from the libvpx reference (boolhuff.c / dboolhuff), with the low/value
;;;; registers kept explicitly masked to 32/16 bits since Lisp integers don't wrap.

(defpackage #:webrtc-media.vp8
  (:use #:cl)
  (:nicknames #:vp8)
  (:export #:make-bwriter #:bwrite-bit #:bwrite-literal #:bwrite-flag #:bwrite-finish
           #:make-breader* #:bread-bit #:bread-literal #:bread-flag
           ;; encoder entry points
           #:encode-gray-frame #:encode-keyframe-gray #:encode-keyframe-checker #:parse-keyframe))

(in-package #:webrtc-media.vp8)

;;; ---- encoder ---------------------------------------------------------------
;;;
;;; EVERY SLOT IS TYPED, and the buffer is a SIMPLE array with an explicit fill index rather than
;;; an adjustable vector with a fill pointer.  Both are about the same thing: this is the codec's
;;; innermost loop — a 1280x800 keyframe runs it 687k times — and untyped slots made every one of
;;; those iterations do generic arithmetic on boxed values (the profile showed GENERIC-+, GENERIC-*
;;; and a full call to ASH inside it), while the adjustable buffer made the byte store a full call
;;; to VECTOR-PUSH-EXTEND and the carry loop's AREF a HAIRY-DATA-VECTOR-REF.  The types below are
;;; the ones the algorithm actually guarantees:
;;;
;;;   RANGE  is 255 to start and thereafter (ASH range (- 8 (INTEGER-LENGTH range))), which for any
;;;          range in 1..255 lands in 128..255 — so (UNSIGNED-BYTE 8) is exact, not a guess.
;;;   LOW    is masked to 32 bits on every path that writes it.
;;;   OFFSET is -COUNT-before-the-shift, and the branch is only reached when COUNT+SHIFT >= 0 with
;;;          SHIFT <= 8, so COUNT-before is at least -8.  Saying so is what turns (ASH low ...) from
;;;          a full call into a shift instruction: without a sign for the shift count the compiler
;;;          cannot pick a direction.
(defstruct (bwriter (:conc-name bw-))
  (low 0 :type (unsigned-byte 32))
  (range 255 :type (unsigned-byte 8))
  (count -24 :type fixnum)
  (fill 0 :type fixnum)
  (buf (make-array 8192 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (*))))

(declaim (inline bw-push))
(defun bw-push (bw byte)
  "Append one coded byte, doubling the buffer when it is full."
  (declare (type (unsigned-byte 8) byte) (optimize (speed 3) (safety 0)))
  (let ((n (bw-fill bw)) (b (bw-buf bw)))
    (declare (type fixnum n) (type (simple-array (unsigned-byte 8) (*)) b))
    (when (>= n (length b))
      (let ((new (make-array (* 2 (length b)) :element-type '(unsigned-byte 8))))
        (replace new b)
        (setf b new (bw-buf bw) new)))
    (setf (aref b n) byte
          (bw-fill bw) (the fixnum (1+ n)))))

(declaim (inline bwrite-bit))
(defun bwrite-bit (bw prob bit)
  "Encode BIT (0/1) with PROB = P(bit=0), an 8-bit probability (RFC 6386 §13.2)."
  (declare (type (unsigned-byte 8) prob) (type bit bit)
           (optimize (speed 3) (safety 0)))
  (let* ((split (+ 1 (ash (* (- (bw-range bw) 1) prob) -8)))
         (range split)
         (low (bw-low bw)))
    (declare (type (unsigned-byte 8) range) (type (unsigned-byte 32) low))
    (when (plusp bit)
      (setf low (logand (+ low split) #xffffffff)
            range (- (bw-range bw) split)))
    (let ((shift (- 8 (integer-length range)))       ; vp8_norm[range]
          (fshift 0))
      (declare (type (integer 0 8) shift fshift))
      (setf fshift shift
            range (ash range shift))
      (incf (bw-count bw) shift)
      (when (>= (bw-count bw) 0)
        (let ((offset (the (integer 1 8) (- shift (bw-count bw)))))
          (when (logtest (logand (ash low (1- offset)) #xffffffff) #x80000000)   ; carry
            (let ((x (1- (bw-fill bw))) (b (bw-buf bw)))
              (declare (type fixnum x) (type (simple-array (unsigned-byte 8) (*)) b))
              (loop while (and (>= x 0) (= (aref b x) #xff))
                    do (setf (aref b x) 0) (decf x))
              (when (>= x 0) (incf (aref b x)))))
          (bw-push bw (logand (ash low (- offset 24)) #xff))
          (setf low (logand (ash low offset) #xffffff)
                fshift (bw-count bw))
          (decf (bw-count bw) 8)))
      (setf (bw-low bw) (logand (ash low fshift) #xffffffff)
            (bw-range bw) range))))

(defun bwrite-literal (bw value nbits)
  "Encode NBITS of VALUE MSB-first, each at probability 128 (a uniform literal)."
  (declare (type fixnum value nbits) (optimize (speed 3) (safety 0)))
  (loop for i of-type fixnum from (1- nbits) downto 0
        do (bwrite-bit bw 128 (logand (ash value (- i)) 1))))

(declaim (inline bwrite-flag))
(defun bwrite-flag (bw bit) (bwrite-bit bw 128 (if bit 1 0)))

(defun bwrite-finish (bw)
  "Flush: 32 zero-bits at prob 128 push the last partial byte out.  Returns the coded bytes."
  (dotimes (i 32) (bwrite-bit bw 128 0))
  (subseq (bw-buf bw) 0 (bw-fill bw)))

;;; ---- decoder (RFC 6386 §7.3) -----------------------------------------------
(defstruct (breader (:conc-name br-))
  buf (pos 2) (value 0) (range 255) (bitcount 0))

(defun make-breader* (buf)
  (make-breader :buf buf :value (logior (ash (aref buf 0) 8) (aref buf 1))))

(defun bread-bit (br prob)
  (let* ((split (+ 1 (ash (* (- (br-range br) 1) prob) -8)))
         (bigsplit (ash split 8))
         (bit 0))
    (cond ((>= (br-value br) bigsplit)
           (setf bit 1 (br-range br) (- (br-range br) split)
                 (br-value br) (- (br-value br) bigsplit)))
          (t (setf (br-range br) split)))
    (loop while (< (br-range br) 128) do
      (setf (br-value br) (ash (br-value br) 1)
            (br-range br) (ash (br-range br) 1))
      (when (= (incf (br-bitcount br)) 8)
        (setf (br-bitcount br) 0)
        (when (< (br-pos br) (length (br-buf br)))
          (setf (br-value br) (logior (br-value br) (aref (br-buf br) (br-pos br))))
          (incf (br-pos br)))))
    bit))

(defun bread-literal (br nbits)
  (let ((v 0)) (dotimes (i nbits v) (setf v (logior (ash v 1) (bread-bit br 128))))))
(defun bread-flag (br) (= 1 (bread-bit br 128)))
