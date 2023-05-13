(define-module (rsauex packages minipro)
  #:use-module ((gnu packages libusb)     #:prefix libusb:)
  #:use-module ((gnu packages pkg-config) #:prefix pkg-config:)
  #:use-module ((guix build-system gnu))
  #:use-module ((guix gexp))
  #:use-module ((guix git-download))
  #:use-module ((guix licenses)           #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public minipro
  (let ((version "0.6")
        (commit "9db72e929c9ba98ea5ca9ffb72a684b534b770ec")
        (commit-short "9db72e92")
        (commit-date "2023-04-03 00:00:00 +000"))
    (package
      (name "minipro")
      (version (string-append version "-" commit-short))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/DavidGriffith/minipro")
               (commit commit)))
         (sha256
          (base32
           "192j0sl771ifrjzd6whp6ysnhivss7b7y01z068zckxy8imslx50"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:make-flags #~(list (string-append "VERSION=" #$version)
                                  (string-append "GIT_BRANCH=" #$version)
                                  (string-append "GIT_HASH=" #$commit)
                                  (string-append "GIT_HASH_SHORT=" #$commit-short)
                                  (string-append "GIT_DATE=" #$commit-date)
                                  (string-append "PREFIX=" #$output)
                                  (string-append "UDEV_DIR=" #$output "/lib/udev")
                                  (string-append "COMPLETIONS_DIR=" #$output "/share/bash-completion/completions")
                                  (string-append "PKG_CONFIG=" #$(this-package-native-input "pkg-config") "/bin/pkg-config"))
             #:phases #~(modify-phases %standard-phases
                          (delete 'configure))))
      (native-inputs
       `(("pkg-config" ,pkg-config:pkg-config)))
      (inputs
       `(("libusb" ,libusb:libusb)))
      (synopsis "An open source program for controlling the MiniPRO TL866xx series of chip programmers.")
      (description "This program exists because the manufacturer of the MiniPRO TL866xx series of
chip programmers does not provide a program for use on Linux or other flavors
of Unix. We who keep this project going prefer a simple, free, and open-source
program that presents a command-line interface that allows for a GUI front-end
if desired.")
      (home-page "https://gitlab.com/DavidGriffith/minipro")
      (license licenses:gpl3))))
