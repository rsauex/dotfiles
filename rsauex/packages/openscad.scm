(define-module (rsauex packages openscad)
  #:use-module ((gnu packages gcc)            #:prefix gcc:)
  #:use-module ((guix build-system copy)      #:prefix copy-build-system:)
  #:use-module ((guix download)               #:prefix download:)
  #:use-module ((guix git-download)           #:prefix git-download:)
  #:use-module ((guix gexp))
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages))
  #:use-module ((nonguix build-system binary) #:prefix non-binary-build-system:))

(define-public openscad-lsp
  (package
    (name "openscad-lsp")
    (version "2.0.1")
    (source
     (origin
       (method download:url-fetch)
       (uri (string-append "https://github.com/Leathong/openscad-LSP/releases/download/v"
                           version "/openscad-lsp-x86_64-unknown-linux-gnu.tar.xz"))
       (sha256
        (base32
         "1sh521kin3w0k0v7qqvxy8x9b8bynh5x1fykhx9sd4qd3227y6z5"))))
    (build-system non-binary-build-system:binary-build-system)
    (arguments
     (list #:install-plan
           #~'(("openscad-lsp" "bin/openscad-lsp"))
           #:patchelf-plan
           #~`(("openscad-lsp" ("libc" "gcc")))))
    (inputs
     (list `(,gcc:gcc "lib")))
    (home-page "https://github.com/openscad-lsp/openscad-lsp")
    (synopsis "A LSP (Language Server Protocol) server for OpenSCAD.")
    (description "A LSP (Language Server Protocol) server for OpenSCAD.")
    (license license:asl2.0)))

(define-public bosl2
  (let ((commit "7d02591c01d2c8fa1c2d665a5214a4f53ac82490"))
    (package
      (name "bosl2")
      (version (string-append "2.0.0-" (substring commit 0 7)))
      (source (origin
                (method git-download:git-fetch)
                (uri (git-download:git-reference
                      (url "https://github.com/BelfrySCAD/BOSL2.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1dqrl1mxvmncwif77ghz3gms49i15sa9lr708nbb46jiljyffww2"))))
      (build-system copy-build-system:copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("." "share/openscad/lib/BOSL2"))))
      (native-search-paths
       (list (search-path-specification
              (variable "OPENSCADPATH")
              (files '("share/openscad/lib")))))
      (home-page "https://github.com/BelfrySCAD/BOSL2")
      (synopsis "The Belfry OpenScad Library, v2.0.")
      (description "An OpenSCAD library of shapes, masks, and manipulators to make working with OpenSCAD easier.")
      (license license:bsd-2))))
