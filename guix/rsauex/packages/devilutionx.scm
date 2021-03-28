(define-module (rsauex packages devilutionx)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages sdl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils))

(define-public devilutionx
  (package
    (name "devilution")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diasurgical/devilutionX")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18kidqcdl7ny51hx7kpcw0nm5s77ns02xnspm80dqd12simgzbfx"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "-DTTF_FONT_DIR=\"" (assoc-ref %outputs "out") "/share/fonts/truetype/\""))))
    (inputs
     `(("libsodium" ,libsodium)
       ("sdl2-mixer" ,sdl2-mixer)
       ("sdl2-ttf" ,sdl2-ttf)))
    (home-page "https://github.com/diasurgical/devilutionX")
    (synopsis "Diablo build for modern operating systems.")
    (description "Diablo build for modern operating systems.")
    (license unlicense)))
