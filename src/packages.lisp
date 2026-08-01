;;;; webrtc-media — the media plane for a pure-CL WebRTC peer.
;;;;
;;;; webrtc-data carries the SCTP data channel over DTLS.  webrtc-media carries audio (and,
;;;; later, video) as SRTP alongside it on the SAME bundled ICE/DTLS transport: packets are
;;;; demuxed by their first byte — DTLS records (20-63) stay with webrtc-data's SCTP, SRTP
;;;; (128-191) routes here.  SRTP keys come from the DTLS handshake (RFC 5764 exporter), which
;;;; seal already sets up (it offers the use_srtp extension).
;;;;
;;;; Codec is G.711 (PCMU, payload type 0): browser-native, trivial, and it keeps the whole
;;;; stack pure-CL (no Opus/CFFI).  64 kbit/s is nothing over the channel.

(defpackage #:webrtc-media
  (:use #:cl)
  (:nicknames #:wm)
  ;; G.711 (PCMU/PCMA companding) now lives in reed, the pure-CL audio-codec
  ;; library.  Import its entry points and re-export them so wm's public API is
  ;; unchanged (wm:pcmu-encode etc. are reed's symbols).
  (:import-from #:reed #:pcmu-encode #:pcmu-decode #:pcma-encode #:pcma-decode)
  (:export
   ;; G.711 codec (re-exported from reed; (signed-byte 16) PCM <-> (unsigned-byte 8) codewords)
   #:pcmu-encode #:pcmu-decode #:pcma-encode #:pcma-decode
   ;; RTP
   #:rtp #:make-rtp #:rtp-pack #:rtp-parse
   #:rtp-pt #:rtp-marker #:rtp-seq #:rtp-timestamp #:rtp-ssrc #:rtp-payload
   ;; SRTP
   #:srtp-context #:make-srtp-context #:srtp-context-from-dtls
   #:srtp-protect #:srtp-unprotect
   #:+srtp-aead-aes-128-gcm+
   ;; session glue (webrtc-media/rtc)
   #:start-audio #:start-video #:vp8-packetize
   ;; live quality switching: set this and a RUNNING sender adopts it (see vp8-rtp.lisp)
   #:*video-profile*))
