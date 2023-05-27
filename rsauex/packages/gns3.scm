(define-module (rsauex packages gns3)
  #:use-module ((gnu packages python-xyz)  #:prefix python-xyz:)
  #:use-module ((gnu packages qt)          #:prefix qt:)
  #:use-module ((guix build-system python) #:prefix python-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)        #:prefix git-download:)
  #:use-module ((guix licenses)            #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public gns3-gui
  (package
    (name "gns3-gui")
    (version "2.2.29")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/GNS3/gns3-gui")
             (commit (string-append "v" version))))
       (file-name (git-download:git-file-name name version))
       (sha256
        (base32
         "04yqh0kq5pkmadcxf090ilh9sqqxajcg65nf7ai1iikxi3x7z3r8"))))
    (build-system python-build-system:python-build-system)
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
                "sentry-sdk>=")
               (("jsonschema==")
                "jsonschema>="))
             #t)))))
    (inputs
     `(,python-xyz:python-distro
       ,python-xyz:python-psutil
       ,python-xyz:python-sentry-sdk
       ,python-xyz:python-jsonschema
       ,qt:python-pyqt))
    (synopsis "GNS3 GUI")
    (description "GNS3 GUI")
    (home-page "https://github.com/GNS3/gns3-gui")
    (license licenses:gpl3+)))
