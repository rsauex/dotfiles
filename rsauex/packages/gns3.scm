(define-module (rsauex packages gns3)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix build-system python)
  #:use-module (guix utils))

(define-public gns3-gui
  (package
    (name "gns3-gui")
    (version "2.2.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GNS3/gns3-gui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04yqh0kq5pkmadcxf090ilh9sqqxajcg65nf7ai1iikxi3x7z3r8"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fixup-requirements
           (lambda _
             (substitute* `("requirements.txt")
               (("psutil==")
                "psutil>=")
               (("sentry-sdk==")
                "sentry-sdk>="))
             #t)))))
    (inputs
     `(,python-distro
       ,python-psutil
       ,python-sentry-sdk
       ,python-jsonschema
       ,python-pyqt))
    (synopsis "GNS3 GUI")
    (description "GNS3 GUI")
    (home-page "https://github.com/GNS3/gns3-gui")
    (license licenses:gpl3+)))
