(define-module (rsauex packages plata)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu))

(define-public plata-theme
  (package
    (name "plata-theme")
    (version "0.9.9")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://gitlab.com/tista500/plata-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256 (base32 "1iwvlv9qcrjyfbzab00vjqafmp3vdybz1hi02r6lwbgvwyfyrifk"))))
    (build-system gnu-build-system)
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
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)))
    (native-inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("inkscape" ,inkscape)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("sassc" ,sassc)))
    (synopsis "A Gtk theme based on Material Design Refresh.")
    (description "A Gtk theme based on Material Design Refresh.")
    (home-page "https://gitlab.com/tista500/plata-theme")
    (license gpl2)))
