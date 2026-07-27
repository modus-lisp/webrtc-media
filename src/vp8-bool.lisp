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
           #:make-breader* #:bread-bit #:bread-literal #:bread-flag))

(in-package #:webrtc-media.vp8)

;;; ---- encoder ---------------------------------------------------------------
(defstruct (bwriter (:conc-name bw-))
  (low 0) (range 255) (count -24)
  (buf (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))

(defun bwrite-bit (bw prob bit)
  "Encode BIT (0/1) with PROB = P(bit=0), an 8-bit probability (RFC 6386 §13.2)."
  (let* ((split (+ 1 (ash (* (- (bw-range bw) 1) prob) -8)))
         (range split)
         (low (bw-low bw)))
    (when (plusp bit)
      (setf low (logand (+ low split) #xffffffff)
            range (- (bw-range bw) split)))
    (let ((shift (- 8 (integer-length range))))       ; vp8_norm[range]
      (setf range (ash range shift))
      (incf (bw-count bw) shift)
      (when (>= (bw-count bw) 0)
        (let ((offset (- shift (bw-count bw))))
          (when (logtest (logand (ash low (1- offset)) #xffffffff) #x80000000)   ; carry
            (let ((x (1- (fill-pointer (bw-buf bw)))))
              (loop while (and (>= x 0) (= (aref (bw-buf bw) x) #xff))
                    do (setf (aref (bw-buf bw) x) 0) (decf x))
              (when (>= x 0) (incf (aref (bw-buf bw) x)))))
          (vector-push-extend (logand (ash low (- offset 24)) #xff) (bw-buf bw))
          (setf low (logand (ash low offset) #xffffff)
                shift (bw-count bw))
          (decf (bw-count bw) 8)))
      (setf (bw-low bw) (logand (ash low shift) #xffffffff)
            (bw-range bw) range))))

(defun bwrite-literal (bw value nbits)
  "Encode NBITS of VALUE MSB-first, each at probability 128 (a uniform literal)."
  (loop for i from (1- nbits) downto 0
        do (bwrite-bit bw 128 (logand (ash value (- i)) 1))))

(defun bwrite-flag (bw bit) (bwrite-bit bw 128 (if bit 1 0)))

(defun bwrite-finish (bw)
  "Flush: 32 zero-bits at prob 128 push the last partial byte out.  Returns the coded bytes."
  (dotimes (i 32) (bwrite-bit bw 128 0))
  (subseq (bw-buf bw) 0))

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
