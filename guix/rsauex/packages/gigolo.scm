(define-module (rsauex packages gigolo)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xfce)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public gigolo
  (package
    (name "gigolo")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/apps/"
                           "gigolo/" (version-major+minor version) "/"
                           "gigolo-" version ".tar.bz2"))
       (sha256
        (base32
         "0kp19072l2xn2962cfqaf57l9pjykvm08ivhg52q9r3ib3dvm1ya"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("exo" ,exo)
       ("gvfs" ,gvfs)))
    (synopsis "Xfce fs manager.")
    (description "A frontend to easily manage connections to remote filesystems.")
    (home-page "https://www.xfce.org/")
    (license gpl2+)))
