(define-module (rsauex channels)
  #:use-module ((guix channels) #:prefix channels:)
  #:export (nonguix-channel))

(define nonguix-channel
  (channels:channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (channels:make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (channels:openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
