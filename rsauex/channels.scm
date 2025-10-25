(define-module (rsauex channels)
  #:use-module ((guix channels) #:prefix channels:)
  #:export (nonguix-channel
            nonguix-substitute-url
            nonguix-substitute-primary-key))

(define nonguix-channel
  (channels:channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (channels:make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (channels:openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define nonguix-substitute-url
  "https://substitutes.nonguix.org")

(define nonguix-substitute-primary-key
  '(public-key
    (ecc
     (curve Ed25519)
     (q #vu8(193 253 83 229 212 206 151 25
                 51 236 80 201 243 7 174 33
                 113 162 211 181 44 128 70 66
                 167 163 95 132 243 164 234 152)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %channels
  (list
   (channels:channel
    (inherit channels:%default-guix-channel)
    (commit "20c3da4c64b83108116fd0e57e361a7fb20a30d7"))
   (channels:channel
    (inherit nonguix-channel)
    (commit "a345ef84fbdf3b2491acb2c2b6665a4eb97bd4aa"))))

%channels
