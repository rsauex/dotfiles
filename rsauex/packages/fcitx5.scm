(define-module (rsauex packages fcitx5)
  #:use-module ((gnu packages emacs)          #:prefix emacs:)
  #:use-module ((gnu packages fcitx5)         #:prefix fcitx5:)
  #:use-module ((gnu packages gettext)        #:prefix gettext:)
  #:use-module ((gnu packages kde-frameworks) #:prefix kde-frameworks:)
  #:use-module ((gnu packages pkg-config)     #:prefix pkg-config:)
  #:use-module ((gnu packages pretty-print)   #:prefix pretty-print:)
  #:use-module ((guix build-system cmake)     #:prefix cmake-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)           #:prefix git-download:)
  #:use-module ((guix licenses)               #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public fcitx5-m17n
  (package
    (name "fcitx5-m17n")
    (version "5.0.9")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/fcitx/fcitx5-m17n")
             (commit version)))
       (file-name (git-download:git-file-name name version))
       (sha256
        (base32
         "1fwipb11018b96lcp7xx5msqn8b56ivy0h04prmsarlgdk6q8917"))))
    (build-system cmake-build-system:cmake-build-system)
    (native-inputs
     `(,gettext:gettext-minimal
       ,pkg-config:pkg-config
       ,kde-frameworks:extra-cmake-modules))
    (inputs
     `(,fcitx5:fcitx5
       ,emacs:m17n-lib
       ,emacs:m17n-db
       ,pretty-print:fmt))
    (synopsis "m17n engine for fcitx5")
    (description "m17n engine for fcitx5")
    (home-page "https://github.com/fcitx/fcitx5-m17n")
    (license licenses:lgpl2.1+)))
