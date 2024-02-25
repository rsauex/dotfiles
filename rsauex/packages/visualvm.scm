(define-module (rsauex packages visualvm)
  #:use-module ((guix build-system copy)  #:prefix copy-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix download)           #:prefix download:)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)))

(define-public visualvm
  (package
    (name "visualvm")
    (version "2.1.7")
    (source
     (origin
       (method download:url-fetch/zipbomb)
       (uri (string-append "https://github.com/oracle/visualvm/releases/download/"
                           version "/visualvm_" (string-delete #\. version) ".zip"))
       (sha256
        (base32
         "0cvzhs89gazjiq351p253alwavgjk9zgi8l9kgk5mcrzygiy1wcs"))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     (list #:install-plan
           #~'((#$(string-append "visualvm_" (string-delete #\. version)) "share/visualvm"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-bin
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (mkdir (string-append out "/bin"))
                     (symlink (string-append out "/share/visualvm/bin/visualvm")
                              (string-append out "/bin/visualvm"))))))))
    (home-page "https://visualvm.github.io/")
    (synopsis "All-in-One Java troubleshooting tool.")
    (description "VisualVM is a visual tool integrating commandline JDK tools and lightweight
profiling capabilities. Designed for both development and production time use.")
    (license license:gpl2+)))
