(defsystem "webrtc-media"
  :description "A from-scratch WebRTC media plane in pure Common Lisp: SRTP/SRTCP (AES-GCM)
and RTP/RTCP framing atop seal's DTLS + crypto, with the G.711 audio codec provided by reed
and the VP8 video codec by reel (the pure-CL audio- and video-codec libraries).  Pairs with
webrtc-data (the SCTP data-channel plane) on the same bundled ICE/DTLS transport.

The codec used to live in here, in eight src/vp8-*.lisp files.  It is reel's now: an encoder
is not a property of the transport that happens to carry it, and a caller who wants to encode
a frame to a file should not be loading SRTP to do it.  vp8-rtp.lisp — the RFC 7741 payload
format and the capture-driven sender — stays, because that IS transport."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("seal" "reed" "reel")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "audio")       ; the 20 ms / 8 kHz audio frame + the SOURCE thunk that produces one
     (:file "rtp")         ; RTP + RTCP header pack/parse (RFC 3550)
     (:file "srtp")))))    ; DTLS-SRTP keying (RFC 5764) + SRTP/SRTCP AES-GCM (RFC 7714)

(defsystem "webrtc-media/rtc"
  :description "Session glue: full-duplex G.711/SRTP audio over a webrtc-data ICE/DTLS transport."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("webrtc-media" "webrtc-data" "bordeaux-threads")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components ((:file "session")
                 (:file "vp8-rtp")))))   ; VP8 RTP payload format (RFC 7741) + video sender
