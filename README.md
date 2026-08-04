# webrtc-media

A from-scratch WebRTC **media plane** in pure Common Lisp — SRTP/SRTCP (AES-GCM,
RFC 7714), RTP/RTCP framing (RFC 3550), the G.711 codec, and a VP8 encoder.
Pairs with [webrtc-data](../webrtc-data) (the SCTP data-channel plane) on the
same bundled ICE/DTLS transport; SRTP keys come from the DTLS handshake
(RFC 5764) via `seal`.

G.711 (PCMU) keeps the whole stack pure-CL — no Opus, no CFFI.

## Status
- [x] G.711 PCMU/PCMA codec (from `reed`, the pure-CL audio-codec library)
- [x] RTP pack/parse
- [x] SRTP-GCM protect/unprotect + DTLS-SRTP keying
- [x] SDP m=audio negotiation (in webrtc-data's `sdp.lisp` — PT 0, `a=rtcp-mux`)
- [x] ICE-transport demux (SRTP alongside SCTP) + full-duplex audio
- [x] a real audio **source** and **sink** — see below
- [ ] jitter buffer + packet-loss concealment
- [ ] RTCP sender/receiver reports
- [ ] A/V clock sync (audio and video each pace themselves)

## Audio: where the samples come from and go

`start-audio` is open at both ends. Outbound, `:source` is a thunk asked for one
frame every 20 ms — up to 160 `(signed-byte 16)` samples at 8 kHz, or `NIL` when
it has nothing to say. Inbound, `:on-rx-pcm` is handed `(PCM RTP)`: the decoded
frame, plus the parsed header a jitter buffer would need.

```lisp
(wm:start-audio agent session
                :source     (wm:make-tone-source :hz 440d0 :on 1 :period 1)
                :on-rx-pcm  (lambda (pcm rtp) (declare (ignore rtp)) (enqueue pcm)))
```

Three things about the source are load-bearing:

- **It runs on the sender's thread**, which is the stream's clock. It must not
  block — read from a queue some capture thread fills; do not wait on a device.
- **`NIL` is not a gap.** The sender fills the frame with silence and sends it
  anyway, because the timing the far side reads is carried by the packets
  themselves; a sender that goes quiet stops telling the peer what time it is.
- **8 kHz mono, because PCMU is.** There is no resampler in this stack. A source
  at 44.1 or 48 kHz has to rate-convert before it gets here, and there is
  nothing here to do that with yet.

`reed` decodes MP3, AAC-LC and Opus but **encodes only G.711**, so outbound is
64 kbit/s µ-law until someone writes an encoder. There is no mixer: one source,
one stream.

## Tests

```sh
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/audio-seam.lisp
```

`inspect/audio-seam.lisp` puts a tone of a known frequency in at `:source` and
runs it through everything except ICE — G.711 encode, RTP pack, SRTP protect,
then the inbound half against a separately-keyed context, as the two ends of a
call are — and measures the result with a Goertzel filter. Insisting the energy
is *at 440 Hz* rather than merely present is the point: a path that dropped,
doubled or half-rate-ed the samples still delivers plausible bytes, identical
packet counts, identical sequence numbers and an identical RMS level. The gate
carries its own negative control proving exactly that.
