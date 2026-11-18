;;;; rtcp.lisp — the RTCP feedback a browser sends about our video: picture loss and lost packets.
;;;;
;;;; WITHOUT IT ONE LOST PACKET FREEZES THE PICTURE.  An inter frame is a delta on the frame before,
;;;; so a frame missing one fragment cannot be decoded and neither can anything after it, until a
;;;; keyframe -- which the sender used to send only every KEY-SECS (30 s).  On a link losing 3% of
;;;; packets nearly every frame of more than a few packets loses one: the desktop stood still.
;;;;
;;;; A browser says what it is missing, once the answer offers it (a=rtcp-fb:PT nack / nack pli,
;;;; webrtc-data's make-answer-sdp):
;;;;   NACK  RTPFB (PT 205) FMT 1, RFC 4585 §6.2.1: these sequence numbers never arrived -- each
;;;;         FCI entry a PID and a bitmask of the 16 after it.  The sender resends them from a short
;;;;         history (start-video), so a loss costs one round trip instead of a frozen picture.
;;;;   PLI   PSFB (PT 206) FMT 1, RFC 4585 §6.3.1: I cannot decode, start me again.  The sender
;;;;         makes its next frame a keyframe (*FORCE-KEYFRAME*), rate-limited -- a keyframe is the
;;;;         expensive thing (see START-VIDEO), and a browser repeats a PLI until one arrives.
;;;;
;;;; Everything else in a compound packet (receiver reports, SDES, REMB, transport-cc) is skipped.

(in-package #:webrtc-media)

(defvar *rtcp-on-nack* nil
  "Called with a list of RTP sequence numbers the peer reports lost, or NIL for nobody listening.
Set by START-VIDEO for the life of its stream.")

(defvar *rtcp-on-pli* nil
  "Called with no arguments when the peer reports picture loss, or NIL.  Set by START-VIDEO.")

(defvar *rtcp-stats* (list :packets 0 :undecrypted 0 :nack 0 :nack-seqs 0 :pli 0)
  "Counts of what arrived, for a log line and a test.")

(defun %rtcp-count (key &optional (n 1))
  (setf (getf *rtcp-stats* key) (+ n (getf *rtcp-stats* key 0))))

(defun rtcp-nack-seqs (fci-bytes start end)
  "The sequence numbers named by the NACK FCI entries in FCI-BYTES[START, END): per 4-octet entry
a PID and a BLP whose bit i means PID+1+i was lost too."
  (loop for off from start below (- end 3) by 4
        for pid = (%u16 fci-bytes off) for blp = (%u16 fci-bytes (+ off 2))
        append (cons pid (loop for i below 16 when (logbitp i blp)
                               collect (logand (+ pid 1 i) #xffff)))))

(defun rtcp-feedback (rtcp)
  "Walk the compound RTCP packet RTCP: (values NACKED-SEQS PLI-P) -- every sequence number a
generic NACK named, and whether any PLI was present."
  (let ((off 0) (n (length rtcp)) (seqs '()) (pli nil))
    (loop while (<= (+ off 4) n) do
      (let* ((b0 (aref rtcp off)) (fmt (logand b0 31)) (pt (aref rtcp (1+ off)))
             (len (* 4 (1+ (%u16 rtcp (+ off 2)))))
             (end (min n (+ off len))))
        (unless (= (ldb (byte 2 6) b0) 2) (return))          ; not RTCP version 2: stop
        (cond ((and (= pt 205) (= fmt 1) (>= (- end off) 12)) ; generic NACK
               (setf seqs (append seqs (rtcp-nack-seqs rtcp (+ off 12) end))))
              ((and (= pt 206) (= fmt 1))                     ; PLI
               (setf pli t)))
        (setf off (+ off len))))
    (values seqs pli)))

(defun handle-rtcp (rtcp)
  "Act on the compound RTCP packet RTCP (decrypted): lost packets to *RTCP-ON-NACK*, picture loss
to *RTCP-ON-PLI*."
  (%rtcp-count :packets)
  (multiple-value-bind (seqs pli) (rtcp-feedback rtcp)
    (when seqs
      (%rtcp-count :nack) (%rtcp-count :nack-seqs (length seqs))
      (let ((f *rtcp-on-nack*)) (when f (funcall f seqs))))
    (when pli
      (%rtcp-count :pli)
      (let ((f *rtcp-on-pli*)) (when f (funcall f))))))
