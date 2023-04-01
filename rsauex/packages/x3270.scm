(define-module (rsauex packages x3270)
  #:use-module ((gnu packages m4)       #:prefix m4:)
  #:use-module ((gnu packages ncurses)  #:prefix ncurses:)
  #:use-module ((gnu packages tcl)      #:prefix tcl:)
  #:use-module ((gnu packages xml)      #:prefix xml:)
  #:use-module ((gnu packages xorg)     #:prefix xorg:)
  #:use-module ((guix build-system gnu) #:prefix gnu-build-system:)
  #:use-module ((guix download)         #:prefix download:)
  #:use-module ((guix licenses)         #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public x3270
  (define v-major "4")
  (define v-minor "2")
  (define v-suffix "ga9")
  (package
    (name "x3270")
    (version (string-append v-major "." v-minor v-suffix))
    (source (origin
              (method download:url-fetch)
              (uri (string-append
                    "http://x3270.bgp.nu/download/0" v-major ".0" v-minor
                    "/suite3270-" version "-src.tgz"))
              (sha256
               (base32
                "01i81lh8xbpz2nbj4hv2yfdh6mpv46jq6nb7b2c3w2pmhcvhq32z"))))
    (build-system gnu-build-system:gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-fontdir=" %output "/share/fonts/X11"))
       #:tests? #f))
    (native-inputs
     (list m4:m4))
    (inputs
     (list xml:expat
           xorg:libx11
           xorg:libxt
           xorg:libxaw
           xorg:libxmu
           xorg:bdftopcf
           xorg:mkfontdir
           tcl:tcl
           ncurses:ncurses))
    (home-page "http://x3270.bgp.nu/index.html")
    (synopsis "3270 Terminal Emulator")
    (description
     "IBM 3270 terminal emulator for the X Window System")
    (license licenses:bsd-3)))
