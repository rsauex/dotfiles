(define-module (rsauex packages fcitx5)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils))

(define-public fcitx5-m17n
  (package
    (name "fcitx5-m17n")
    (version "5.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fcitx/fcitx5-m17n")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fwipb11018b96lcp7xx5msqn8b56ivy0h04prmsarlgdk6q8917"))))
    (build-system cmake-build-system)
    (native-inputs
     `(,gettext-minimal
       ,pkg-config
       ,extra-cmake-modules))
    (inputs
     `(,fcitx5
       ,m17n-lib
       ,m17n-db
       ,fmt))
    (synopsis "m17n engine for fcitx5")
    (description "m17n engine for fcitx5")
    (home-page "https://github.com/fcitx/fcitx5-m17n")
    (license lgpl2.1+)))
