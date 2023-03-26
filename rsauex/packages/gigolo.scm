(define-module (rsauex packages gigolo)
  #:use-module ((gnu packages glib)       #:prefix glib:)
  #:use-module ((gnu packages gnome)      #:prefix gnome:)
  #:use-module ((gnu packages pkg-config) #:prefix pkg-config:)
  #:use-module ((gnu packages xfce)       #:prefix xfce:)
  #:use-module ((guix build-system gnu)   #:prefix gnu-build-system:)
  #:use-module ((guix download)           #:prefix download:)
  #:use-module ((guix gexp))
  #:use-module ((guix licenses)           #:prefix licenses:)
  #:use-module ((guix packages))
  #:use-module ((guix utils)))

(define-public gigolo
  (package
    (name "gigolo")
    (version "0.5.1")
    (source
     (origin
       (method download:url-fetch)
       (uri (string-append "https://archive.xfce.org/src/apps/"
                           "gigolo/" (version-major+minor version) "/"
                           "gigolo-" version ".tar.bz2"))
       (sha256
        (base32
         "0kp19072l2xn2962cfqaf57l9pjykvm08ivhg52q9r3ib3dvm1ya"))))
    (build-system gnu-build-system:gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config:pkg-config)
       ("intltool" ,glib:intltool)))
    (inputs
     `(("exo" ,xfce:exo)
       ("gvfs" ,gnome:gvfs)))
    (synopsis "Xfce fs manager.")
    (description "A frontend to easily manage connections to remote filesystems.")
    (home-page "https://www.xfce.org/")
    (license licenses:gpl2+)))
