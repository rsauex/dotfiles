(define-module (rsauex packages devilutionx)
  #:use-module ((gnu packages crypto)     #:prefix crypto:)
  #:use-module ((gnu packages sdl)        #:prefix sdl:)
  #:use-module ((guix build-system cmake) #:prefix cmake-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)       #:prefix git-download:)
  #:use-module ((guix licenses)           #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public devilutionx
  (package
    (name "devilution")
    (version "1.1.0")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/diasurgical/devilutionX")
             (commit version)))
       (file-name (git-download:git-file-name name version))
       (sha256
        (base32
         "18kidqcdl7ny51hx7kpcw0nm5s77ns02xnspm80dqd12simgzbfx"))))
    (build-system cmake-build-system:cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "-DTTF_FONT_DIR=\"" (assoc-ref %outputs "out") "/share/fonts/truetype/\""))))
    (inputs
     `(("libsodium" ,crypto:libsodium)
       ("sdl2-mixer" ,sdl:sdl2-mixer)
       ("sdl2-ttf" ,sdl:sdl2-ttf)))
    (home-page "https://github.com/diasurgical/devilutionX")
    (synopsis "Diablo build for modern operating systems.")
    (description "Diablo build for modern operating systems.")
    (license licenses:unlicense)))
