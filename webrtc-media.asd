(defsystem "webrtc-media"
  :description "A from-scratch WebRTC media plane in pure Common Lisp: SRTP/SRTCP (AES-GCM),
RTP/RTCP framing, and the G.711 codec, atop seal's DTLS + crypto.  Pairs with webrtc-data
(the SCTP data-channel plane) on the same bundled ICE/DTLS transport."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("seal")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "g711")        ; G.711 PCMU (mu-law) + PCMA (A-law) companding
     (:file "rtp")         ; RTP + RTCP header pack/parse (RFC 3550)
     (:file "srtp")        ; DTLS-SRTP keying (RFC 5764) + SRTP/SRTCP AES-GCM (RFC 7714)
     (:file "vp8-bool")))))  ; VP8 boolean entropy coder (RFC 6386 §7/§13) — first piece of the VP8 codec

(defsystem "webrtc-media/rtc"
  :description "Session glue: full-duplex G.711/SRTP audio over a webrtc-data ICE/DTLS transport."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("webrtc-media" "webrtc-data" "bordeaux-threads")
  :serial t
  :components
  ((:module "src"
    :components ((:file "session")))))
