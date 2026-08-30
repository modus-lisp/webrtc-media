;;;; rtp-gate.lisp — RTP framing against the bytes RFC 3550 actually specifies.
;;;;
;;;;     sbcl --load inspect/rtp-gate.lisp
;;;;
;;;; The header is twelve bytes with a fixed layout, and every field in it is
;;;; big-endian.  That makes it exactly the kind of code that works on the machine
;;;; that wrote it and fails against anybody else's stack: a sequence number packed
;;;; little-endian still round-trips through our own parser perfectly, and is wrong
;;;; on the wire.  So the checks below assert the BYTES, not just the round trip —
;;;; a round-trip test would pass with both bytes swapped.
;;;;
;;;; Nothing here touches the network or a codec: RTP-PACK and RTP-PARSE are pure
;;;; functions over a byte vector, which is why this is the part worth gating.

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
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(format t "~&== the twelve bytes ==~%")
(let* ((payload (make-array 4 :element-type '(unsigned-byte 8)
                              :initial-contents '(#xDE #xAD #xBE #xEF)))
       (p (make-rtp :pt 96 :marker t :seq #x1234 :timestamp #x89ABCDEF
                    :ssrc #x01020304 :payload payload))
       (b (rtp-pack p)))
  (ok "length is 12 + payload" (= (length b) 16) (format nil "~d" (length b)))
  ;; V=2 in the top two bits, and nothing else set: no padding, no extension, no CSRC.
  (ok "byte 0 is #x80 — version 2, and no padding/extension/CSRC" (= (aref b 0) #x80)
      (format nil "#x~2,'0X" (aref b 0)))
  ;; The marker is the top bit of byte 1; the payload type is the low seven.
  (ok "the marker rides the top bit of byte 1 with PT beneath it"
      (= (aref b 1) (logior #x80 96)) (format nil "#x~2,'0X" (aref b 1)))
  ;; BIG-ENDIAN, which is the whole point.  #x1234 must be #x12 then #x34.
  (ok "the sequence number is big-endian"
      (and (= (aref b 2) #x12) (= (aref b 3) #x34))
      (format nil "~2,'0X ~2,'0X" (aref b 2) (aref b 3)))
  (ok "…and so is the timestamp"
      (equal (list (aref b 4) (aref b 5) (aref b 6) (aref b 7))
             '(#x89 #xAB #xCD #xEF)))
  (ok "…and the SSRC"
      (equal (list (aref b 8) (aref b 9) (aref b 10) (aref b 11))
             '(#x01 #x02 #x03 #x04)))
  (ok "the payload follows the header untouched"
      (equalp (subseq b 12) payload))

  (format t "~&== and back again ==~%")
  (let ((q (rtp-parse b)))
    (ok "payload type survives" (= (rtp-pt q) 96))
    (ok "marker survives" (eq (and (rtp-marker q) t) t))
    (ok "sequence survives" (= (rtp-seq q) #x1234))
    (ok "timestamp survives" (= (rtp-timestamp q) #x89ABCDEF))
    (ok "ssrc survives" (= (rtp-ssrc q) #x01020304))
    (ok "payload survives" (equalp (rtp-payload q) payload))))

(format t "~&== headers this code did not write ==~%")
;; A parser is only interesting on input it did not produce.  Both of these are legal
;; RTP that RTP-PACK never emits, and both used to be the way a real peer breaks you.
(let* ((payload (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(1 2)))
       ;; CC=2: two CSRC identifiers sit between the header and the payload.
       (with-csrc (concatenate '(vector (unsigned-byte 8))
                               (list #x82 96 #x00 #x01 0 0 0 0 0 0 0 0
                                     9 9 9 9  8 8 8 8)
                               payload))
       ;; X=1: a four-byte extension header plus one 32-bit word of extension.
       (with-ext (concatenate '(vector (unsigned-byte 8))
                              (list #x90 96 #x00 #x02 0 0 0 0 0 0 0 0
                                    #xBE #xDE 0 1  7 7 7 7)
                              payload)))
  (let ((q (rtp-parse with-csrc)))
    (ok "a CSRC list is skipped, not read as payload" (equalp (rtp-payload q) payload)
        (format nil "~a" (coerce (rtp-payload q) 'list)))
    (ok "…and the fields before it still parse" (= (rtp-seq q) 1)))
  (let ((q (rtp-parse with-ext)))
    (ok "a header extension is skipped by its declared length"
        (equalp (rtp-payload q) payload)
        (format nil "~a" (coerce (rtp-payload q) 'list)))
    (ok "…and the fields before it still parse" (= (rtp-seq q) 2))))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
