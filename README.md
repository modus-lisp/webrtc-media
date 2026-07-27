# webrtc-media

A from-scratch WebRTC **media plane** in pure Common Lisp — SRTP/SRTCP (AES-GCM,
RFC 7714), RTP/RTCP framing (RFC 3550), and the G.711 codec. Pairs with
[webrtc-data](../webrtc-data) (the SCTP data-channel plane) on the same bundled
ICE/DTLS transport; SRTP keys come from the DTLS handshake (RFC 5764) via `seal`.

G.711 (PCMU) keeps the whole stack pure-CL — no Opus, no CFFI.

## Status
- [x] G.711 PCMU/PCMA codec
- [x] RTP pack/parse
- [x] SRTP-GCM protect/unprotect + DTLS-SRTP keying
- [ ] SDP m=audio negotiation
- [ ] ICE-transport demux (SRTP alongside SCTP) + full-duplex audio
