;;;; srtp.lisp — SRTP with AES-GCM (RFC 7714), keyed by DTLS-SRTP (RFC 5764).
;;;;
;;;; Profile SRTP_AEAD_AES_128_GCM (0x0007): 16-byte key, 12-byte salt, 16-byte tag, no separate
;;;; auth key.  Keys are exported from the finished DTLS handshake:
;;;;
;;;;   keying = PRF(master_secret, "EXTRACTOR-dtls_srtp", client_random‖server_random, 2*(16+12))
;;;;   split  = client_key(16) ‖ server_key(16) ‖ client_salt(12) ‖ server_salt(12)   [RFC 5764 §4.2]
;;;;
;;;; The DTLS client (our box, a=setup:active) sends with the client_* keys and receives with the
;;;; server_* keys.  Per-packet 12-byte IV (RFC 7714 §8.1):
;;;;
;;;;   IV = ( 0x0000 ‖ SSRC(4) ‖ ROC(4) ‖ SEQ(2) )  XOR  salt
;;;;
;;;; The RTP header is the GCM AAD (sent in the clear); the payload is encrypted; the 16-byte tag
;;;; is appended.  So an SRTP packet is:  [RTP header] [encrypted payload] [tag].

(in-package #:webrtc-media)

(defconstant +srtp-aead-aes-128-gcm+ 7)          ; DTLS-SRTP profile id (RFC 7714)
(defconstant +gcm-key-len+ 16)
(defconstant +gcm-salt-len+ 12)
(defconstant +gcm-tag-len+ 16)

(defstruct srtp-context
  send-key send-salt recv-key recv-salt
  (send-roc 0) (send-prev-seq nil)               ; rollover counters for the 48-bit index
  (recv-roc 0) (recv-prev-seq nil))

(defun make-srtp-context-keys (send-key send-salt recv-key recv-salt)
  "Build a context from explicit keys/salts (used by tests and by SRTP-CONTEXT-FROM-DTLS)."
  (make-srtp-context :send-key send-key :send-salt send-salt
                     :recv-key recv-key :recv-salt recv-salt))

(defun srtp-kdf (master-key master-salt label n)
  "RFC 3711 §4.3 / RFC 7714 §11 session-key derivation: N key bytes for LABEL, via AES-128 in
counter mode with key_derivation_rate = 0.  The counter block is MASTER-SALT left-aligned
(rest zero), with LABEL XORed into byte 7 and a 16-bit block index in the last two bytes."
  (let ((slen (length master-salt))
        (out (make-array (* 16 (ceiling n 16)) :element-type '(unsigned-byte 8))))
    (dotimes (j (ceiling n 16))
      (let ((iv (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace iv master-salt :end2 slen)
        (setf (aref iv 7) (logand (logxor (aref iv 7) label) #xff))
        (setf (aref iv 14) (ldb (byte 8 8) j) (aref iv 15) (ldb (byte 8 0) j))
        (replace out (seal:aes-128-encrypt-block master-key iv) :start1 (* j 16))))
    (subseq out 0 n)))

(defconstant +srtp-label-rtp-key+  #x00)
(defconstant +srtp-label-rtp-salt+ #x02)

(defun srtp-context-from-dtls (session &key (role :client))
  "Derive an SRTP context from a finished seal DTLS SESSION.  The DTLS-SRTP exporter yields the
MASTER key+salt per direction; SRTP then derives the SESSION key (16B) + salt (12B) from them via
the KDF.  ROLE :client (a=setup:active, our default) sends with the client keys, receives with the
server keys."
  (let* ((km (seal:dtls-export-keying-material
              session "EXTRACTOR-dtls_srtp" (* 2 (+ +gcm-key-len+ +gcm-salt-len+))))
         (cmk (subseq km 0 16)) (smk (subseq km 16 32))          ; client/server MASTER keys
         (cms (subseq km 32 44)) (sms (subseq km 44 56)))        ; client/server MASTER salts
    (flet ((skey (mk ms) (srtp-kdf mk ms +srtp-label-rtp-key+ +gcm-key-len+))
           (ssalt (mk ms) (srtp-kdf mk ms +srtp-label-rtp-salt+ +gcm-salt-len+)))
      (let ((ck (skey cmk cms)) (cs (ssalt cmk cms))
            (sk (skey smk sms)) (ss (ssalt smk sms)))
        (if (eq role :client)
            (make-srtp-context-keys ck cs sk ss)
            (make-srtp-context-keys sk ss ck cs))))))

(declaim (inline %u16 %u32))
(defun %u16 (buf off) (logior (ash (aref buf off) 8) (aref buf (1+ off))))
(defun %u32 (buf off) (let ((v 0)) (dotimes (i 4 v) (setf v (logior (ash v 8) (aref buf (+ off i)))))))

(defun %srtp-iv (salt ssrc roc seq)
  "12-byte AES-GCM IV = (00 00 ‖ SSRC ‖ ROC ‖ SEQ) XOR salt."
  (let ((iv (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref iv 2) (ldb (byte 8 24) ssrc) (aref iv 3) (ldb (byte 8 16) ssrc)
          (aref iv 4) (ldb (byte 8 8) ssrc)  (aref iv 5) (ldb (byte 8 0) ssrc))
    (setf (aref iv 6) (ldb (byte 8 24) roc)  (aref iv 7) (ldb (byte 8 16) roc)
          (aref iv 8) (ldb (byte 8 8) roc)   (aref iv 9) (ldb (byte 8 0) roc))
    (setf (aref iv 10) (ldb (byte 8 8) seq)  (aref iv 11) (ldb (byte 8 0) seq))
    (dotimes (i 12 iv) (setf (aref iv i) (logxor (aref iv i) (aref salt i))))))

(defun %roc-for (seq prev-seq roc)
  "Guess the rollover counter for SEQ given the previous seq.  Returns (values roc new-prev)."
  (cond ((null prev-seq) (values roc seq))
        ((and (< seq #x4000) (> prev-seq #xc000)) (values (logand (1+ roc) #xffffffff) seq)) ; wrapped up
        (t (values roc seq))))

(defun %hdr-len (buf)
  "Length of the RTP header in BUF (12 + CSRCs + one extension), so we know where payload starts."
  (let* ((cc (logand (aref buf 0) #x0f)) (ext (logbitp 4 (aref buf 0))) (off (+ 12 (* 4 cc))))
    (when ext (incf off (+ 4 (* 4 (%u16 buf (+ off 2)))))) off))

(defun srtp-protect (ctx rtp-bytes)
  "Encrypt a packed RTP packet -> SRTP bytes (header ‖ ciphertext ‖ 16-byte tag).  Uses the send keys."
  (let* ((hlen (%hdr-len rtp-bytes))
         (ssrc (%u32 rtp-bytes 8)) (seq (%u16 rtp-bytes 2)))
    (multiple-value-bind (roc newprev) (%roc-for seq (srtp-context-send-prev-seq ctx) (srtp-context-send-roc ctx))
      (setf (srtp-context-send-roc ctx) roc (srtp-context-send-prev-seq ctx) newprev)
      (let* ((hdr (subseq rtp-bytes 0 hlen))
             (payload (subseq rtp-bytes hlen))
             (iv (%srtp-iv (srtp-context-send-salt ctx) ssrc roc seq))
             (ct.tag (seal:aes-gcm-encrypt (srtp-context-send-key ctx) iv payload hdr))
             (ct (car ct.tag)) (tag (cdr ct.tag))
             (out (make-array (+ hlen (length ct) +gcm-tag-len+) :element-type '(unsigned-byte 8))))
        (replace out hdr)
        (replace out ct :start1 hlen)
        (replace out tag :start1 (+ hlen (length ct)))
        out))))

(defun srtp-unprotect (ctx srtp-bytes)
  "Decrypt+verify SRTP bytes -> packed RTP packet, or NIL if the tag fails.  Uses the recv keys."
  (let* ((hlen (%hdr-len srtp-bytes))
         (ssrc (%u32 srtp-bytes 8)) (seq (%u16 srtp-bytes 2))
         (n (length srtp-bytes)))
    (when (< n (+ hlen +gcm-tag-len+)) (return-from srtp-unprotect nil))
    (multiple-value-bind (roc newprev) (%roc-for seq (srtp-context-recv-prev-seq ctx) (srtp-context-recv-roc ctx))
      (let* ((hdr (subseq srtp-bytes 0 hlen))
             (ct  (subseq srtp-bytes hlen (- n +gcm-tag-len+)))
             (tag (subseq srtp-bytes (- n +gcm-tag-len+)))
             (iv (%srtp-iv (srtp-context-recv-salt ctx) ssrc roc seq))
             (pt (seal:aes-gcm-decrypt (srtp-context-recv-key ctx) iv ct hdr tag)))
        (when pt
          (setf (srtp-context-recv-roc ctx) roc (srtp-context-recv-prev-seq ctx) newprev)
          (let ((out (make-array (+ hlen (length pt)) :element-type '(unsigned-byte 8))))
            (replace out hdr) (replace out pt :start1 hlen) out))))))
