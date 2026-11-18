;;;; rtcp-gate.lisp — the browser's feedback on our video: SRTCP decryption (RFC 7714 §9) and the
;;;; NACK / PLI it carries (RFC 4585), against packets built here byte by byte.
;;;;
;;;;     sbcl --load inspect/rtcp-gate.lisp
;;;;
;;;; The SRTCP packet below is assembled from the RFC's layout directly -- clear header, GCM over
;;;; the rest with AAD = header ‖ E|index, tag, then the E|index word -- not by any function under
;;;; test, so a decryptor that agreed only with itself would fail it.  The keys are the one thing
;;;; only a browser can confirm; glass-webrtc's linkcond does (its card log says "SRTCP decrypted").

(require :asdf)
(let ((here (make-pathname :name nil :type nil
                           :defaults (or *load-truename* *compile-file-truename*))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :webrtc-media)))

(in-package #:webrtc-media)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail))))

(defun bytes (&rest xs) (coerce (alexandria-free-flatten xs) '(vector (unsigned-byte 8))))
(defun alexandria-free-flatten (xs)
  (loop for x in xs append (if (listp x) (alexandria-free-flatten x) (list x))))
(defun be16 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun be32 (n) (list (ldb (byte 8 24) n) (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun rtcp-pkt (fmt pt words-after-header &rest body)
  "One RTCP packet: V=2, FMT/count, PT, length in words-1, then BODY (already 4-byte aligned)."
  (declare (ignore words-after-header))
  (let ((b (alexandria-free-flatten body)))
    (append (list (logior #x80 fmt) pt) (be16 (1- (/ (+ 4 (length b)) 4))) b)))

(defparameter *sender* #x11223344)
(defparameter *media* #x5e5e0001)
(defparameter *rr* (rtcp-pkt 0 201 nil (be32 *sender*)))                      ; empty receiver report
(defparameter *nack* (rtcp-pkt 1 205 nil (be32 *sender*) (be32 *media*)
                               (be16 100) (be16 #b101)                       ; 100, 101, 103
                               (be16 65535) (be16 1)))                       ; 65535, 0 (wraps)
(defparameter *pli* (rtcp-pkt 1 206 nil (be32 *sender*) (be32 *media*)))

(format t "~&[rtcp feedback]~%")
(multiple-value-bind (seqs pli) (rtcp-feedback (bytes *rr* *nack*))
  (ok "a NACK's PID and bitmask name the lost sequence numbers, wrapping at 2^16"
      (equal seqs '(100 101 103 65535 0)) (format nil "~a" seqs))
  (ok "no PLI in that compound packet" (not pli)))
(multiple-value-bind (seqs pli) (rtcp-feedback (bytes *rr* *pli*))
  (ok "a PLI after a receiver report is found" (and pli (null seqs))))
(ok "a packet that is not RTCP version 2 is not read"
    (null (rtcp-feedback (bytes (list 0 205 0 3 0 0 0 0 0 0 0 0 0 100 0 0)))))

(let ((got-nack nil) (got-pli 0))
  (let ((*rtcp-on-nack* (lambda (s) (setf got-nack s)))
        (*rtcp-on-pli* (lambda () (incf got-pli))))
    (handle-rtcp (bytes *rr* *nack* *pli*))
    (ok "HANDLE-RTCP hands the lost packets to the NACK handler" (equal got-nack '(100 101 103 65535 0)))
    (ok "...and the picture loss to the PLI handler, once" (= got-pli 1))))

(format t "~&[SRTCP, RFC 7714 §9]~%")
(let* ((key (bytes (loop for i below 16 collect (* 7 i))))
       (salt (bytes (loop for i below 12 collect (+ 100 i))))
       (ctx (make-srtp-context :recv-rtcp-key key :recv-rtcp-salt salt))
       (plain (bytes *rr* *nack*))
       (index 4242)
       (eidx (be32 (logior #x80000000 index)))
       (clear (subseq plain 0 8))
       ;; IV = (00 00 ‖ SSRC ‖ 00 00 ‖ index) XOR salt, built here independently
       (iv (let ((v (bytes 0 0 (be32 *sender*) 0 0 (be32 index))))
             (dotimes (i 12 v) (setf (aref v i) (logxor (aref v i) (aref salt i))))))
       (ct.tag (seal:aes-gcm-encrypt key iv (subseq plain 8) (bytes (coerce clear 'list) eidx)))
       (wire (bytes (coerce clear 'list) (coerce (car ct.tag) 'list) (coerce (cdr ct.tag) 'list) eidx)))
  (ok "an SRTCP packet built from the RFC's layout decrypts to the compound RTCP packet"
      (equalp (srtcp-unprotect ctx wire) plain))
  (let ((bad (copy-seq wire))) (setf (aref bad 12) (logxor 1 (aref bad 12)))
    (ok "a flipped ciphertext bit fails the tag (NIL)" (null (srtcp-unprotect ctx bad))))
  (let ((bad (copy-seq wire))) (setf (aref bad (- (length bad) 1)) (logxor 1 (aref bad (- (length bad) 1))))
    (ok "a different index fails too: it is authenticated" (null (srtcp-unprotect ctx bad))))
  (let ((unenc (copy-seq wire))) (setf (aref unenc (- (length unenc) 4)) (logand #x7f (aref unenc (- (length unenc) 4))))
    (ok "E=0 (unencrypted SRTCP) is refused, not misread" (null (srtcp-unprotect ctx unenc))))
  (ok "a context without SRTCP keys decrypts nothing"
      (null (srtcp-unprotect (make-srtp-context) wire))))

(format t "~&~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
