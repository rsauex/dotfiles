(define-module (rsauex packages docker)
  #:use-module ((guix build-system copy)  #:prefix copy-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix download)           #:prefix download:)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)))

(define-public docker-compose-2
  (package
    (name "docker-compose-2")
    (version "2.35.1")
    (source
     (origin
       (method download:url-fetch)
       (uri (string-append "https://github.com/docker/compose/releases/download/v"
                           version "/docker-compose-linux-x86_64"))
       (sha256
        (base32
         "0c62nhfi6ghpp5lyv3mbm6qwvzcnpy99h4jx9qsx0pbfj7i2rnvv"))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("docker-compose-linux-x86_64" "/bin/docker-compose"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'make-executable
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (chmod (string-append out "/bin/docker-compose") #o755)))))))
    (home-page "https://github.com/docker/compose")
    (synopsis "Define and run multi-container applications with Docker.")
    (description "Docker Compose is a tool for running multi-container applications on Docker
defined using the Compose file format. A Compose file is used to define how
one or more containers that make up your application are configured. Once you
have a Compose file, you can create and start your application with a single
command: docker compose up.")
    (license license:asl2.0)))
