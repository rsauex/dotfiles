(define-module (rsauex packages plata)
  #:use-module ((gnu packages autotools)  #:prefix autotools:)
  #:use-module ((gnu packages base)       #:prefix base:)
  #:use-module ((gnu packages bash)       #:prefix bash:)
  #:use-module ((gnu packages glib)       #:prefix glib:)
  #:use-module ((gnu packages gtk)        #:prefix gtk:)
  #:use-module ((gnu packages inkscape)   #:prefix inkscape:)
  #:use-module ((gnu packages pkg-config) #:prefix pkg-config:)
  #:use-module ((gnu packages web)        #:prefix web:)
  #:use-module ((gnu packages xml)        #:prefix xml:)
  #:use-module ((guix build-system gnu)   #:prefix gnu-build-system:)
  #:use-module ((guix git-download)       #:prefix git-download:)
  #:use-module ((guix licenses)           #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public plata-theme
  (package
    (name "plata-theme")
    (version "0.9.9")
    (source
     (origin
       (method git-download:git-fetch)
       (uri
        (git-download:git-reference
         (url "https://gitlab.com/tista500/plata-theme")
         (commit version)))
       (file-name (git-download:git-file-name name version))
       (sha256 (base32 "1iwvlv9qcrjyfbzab00vjqafmp3vdybz1hi02r6lwbgvwyfyrifk"))))
    (build-system gnu-build-system:gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-mate")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; let's call configure from configure phase and not now
             (setenv "NOCONFIGURE" "true")
             (invoke "sh" "autogen.sh"))))))
    ;; (propagated-inputs
    ;;  `(("murrine" ,murrine)))
    (inputs
     `(("gdk-pixbuf" ,gtk:gdk-pixbuf)
       ("glib" ,glib:glib)))
    (native-inputs
     `(("bash" ,bash:bash)
       ("coreutils" ,base:coreutils)
       ("glib:bin" ,glib:glib "bin") ; for glib-compile-resources
       ("inkscape" ,inkscape:inkscape)
       ("libxml2" ,xml:libxml2)
       ("pkg-config" ,pkg-config:pkg-config)
       ("autoconf" ,autotools:autoconf)
       ("automake" ,autotools:automake)
       ("sassc" ,web:sassc)))
    (synopsis "A Gtk theme based on Material Design Refresh.")
    (description "A Gtk theme based on Material Design Refresh.")
    (home-page "https://gitlab.com/tista500/plata-theme")
    (license licenses:gpl2)))
