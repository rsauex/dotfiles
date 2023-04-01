(define-module (rsauex packages openxcom)
  #:use-module ((gnu packages boost)         #:prefix boost:)
  #:use-module ((gnu packages compression)   #:prefix compression:)
  #:use-module ((gnu packages gl)            #:prefix gl:)
  #:use-module ((gnu packages sdl)           #:prefix sdl:)
  #:use-module ((gnu packages serialization) #:prefix serialization:)
  #:use-module ((gnu packages tls)           #:prefix tls:)
  #:use-module ((guix build-system cmake)    #:prefix cmake-build-system:)
  #:use-module ((guix git-download)          #:prefix git-download:)
  #:use-module ((guix licenses)              #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public openxcom
  (package
    (name "openxcom")
    (version "1.0.0.2019.10.18")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/OpenXcom/OpenXcom")
             (commit "f9853b2cb8c8f741ac58707487ef493416d890a3")))
       (file-name (git-download:git-file-name name version))
       (sha256
        (base32
         "0kbfawj5wsp1mwfcm5mwpkq6s3d13pailjm5w268gqpxjksziyq0"))))
    (build-system cmake-build-system:cmake-build-system)
    (arguments
     '(#:tests? #f
       ;; #:configure-flags
       ;; (list (string-append "-DTTF_FONT_DIR=\"" (assoc-ref %outputs "out") "/share/fonts/truetype/\""))
       ))
    (inputs
     `(,sdl:sdl
       ,sdl:sdl-gfx
       ,sdl:sdl-image
       ,sdl:sdl-mixer
       ,boost:boost
       ,serialization:yaml-cpp
       ,gl:glu
       ,gl:mesa
       ,tls:openssl-1.1
       ,compression:zlib))
    (home-page "https://openxcom.org/")
    (synopsis "...")
    (description "...")
    (license licenses:gpl3)))
