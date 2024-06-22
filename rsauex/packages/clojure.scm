(define-module (rsauex packages clojure)
  #:use-module ((guix build-system copy)  #:prefix copy-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix download)           #:prefix download:)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)))

(define-public clojure-lsp
  (package
    (name "clojure-lsp")
    (version "2024.04.22-11.50.26")
    (source
     (origin
       (method download:url-fetch/zipbomb)
       (uri (string-append "https://github.com/clojure-lsp/clojure-lsp/releases/download/"
                           version "/clojure-lsp-native-static-linux-amd64.zip"))
       (sha256
        (base32
         "0zrcvi9c5ir2552rxyhgn5vfcj5f9rxbl5qrmc7f3srkdnqnhcx9"))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("clojure-lsp" "bin/clojure-lsp"))))
    (home-page "https://github.com/clojure-lsp/clojure-lsp")
    (synopsis "Clojure & ClojureScript Language Server (LSP) implementation.")
    (description "The goal of this project is to bring great editing tools for
Clojure/Clojurescript to all editors and programatically via its CLI and
API. It aims to work alongside you to help you navigate, identify and fix
errors, perform refactors and much more!")
    (license license:expat)))
