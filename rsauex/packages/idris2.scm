(define-module (rsauex packages idris2)
  #:use-module ((gnu packages chez)           #:prefix chez:)
  #:use-module ((gnu packages llvm)           #:prefix llvm:)
  #:use-module ((gnu packages multiprecision) #:prefix multiprecision:)
  #:use-module ((guix build-system gnu)       #:prefix gnu-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)           #:prefix git-download:)
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)))

(define-public idris2
  (let ((commit "bf87b623ef64244451d10c4b5460e8fc2f88c99a"))
    (package
      (name "idris2")
      (version "0.5.1")
      (source (origin
                (method git-download:git-fetch)
                (uri (git-download:git-reference
                      (url "https://github.com/idris-lang/Idris2.git")
                      (commit (if (string=? commit version)
                                  (string-append "v" version)
                                  commit))))
                (file-name (git-download:git-file-name name version))
                (sha256
                 (base32 "1q6yqd6adfvxwh8yq7dx00907yd9zmallwq2rlr27bxhs6shccgx"))))
      (build-system gnu-build-system:gnu-build-system)
      (native-inputs
       (list llvm:clang
             chez:chez-scheme))
      (inputs
       (list multiprecision:gmp
             chez:chez-scheme))
      (arguments
       `(#:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               "CC=clang")
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key (make-flags '()) (parallel-build? #t) #:allow-other-keys)
               (apply invoke "make"
                      "bootstrap"
                      "SCHEME=scheme"
                      `(,@(if parallel-build?
                              `("-j" ,(number->string (parallel-job-count)))
                              '())
                        ,@make-flags))))
           (add-before 'build 'fix-files
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((bash (assoc-ref inputs "bash")))
                 ,@(unless (string=? commit version)
                     `((substitute* "Makefile"
                         (("\\$\\(GIT_SHA1\\)") ,commit))))
                 (for-each
                  (lambda (file)
                    (substitute* file
                      (("/bin/sh") (string-append bash "/bin/sh"))))
                  (list "src/Compiler/Scheme/Chez.idr"
                        "src/Compiler/Scheme/ChezSep.idr"
                        "src/Compiler/Scheme/Racket.idr"
                        "bootstrap/idris2_app/idris2.rkt"
                        "bootstrap/idris2_app/idris2.ss")))))
           (replace 'install
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" "install" "install-with-src-libs" make-flags)))
           (add-after 'install 'replace-wrapper-script
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Remove existing idris2 wrapper that sets incorrect LD_LIBRARY_PATH
                 (delete-file (string-append out "/bin/idris2"))
                 ;; The only thing we need from idris2_app is the actual binary
                 (copy-file (string-append out "/bin/idris2_app/idris2.so")
                            (string-append out "/bin/idris2"))
                 (delete-file-recursively (string-append out "/bin/idris2_app/"))
                 ;; idris2 needs to find scheme at runtime to compile
                 ;; idris2 installs packages with --install into the path given by
                 ;;   IDRIS2_PREFIX. We set that to a default of ~/.idris2, to mirror the
                 ;;   behaviour of the standard Makefile install.
                 (let* ((chez (assoc-ref inputs "chez-scheme"))
                        (name (string-append ,name "-" ,version))
                        (global-libraries (list (string-append out "/" name))))
                   (wrap-program
                       (string-append out "/bin/idris2")
                     `("CHEZ" = (,(string-append "${CHEZ:-" chez "/bin/scheme}")))
                     `("IDRIS2_PREFIX" = ("${IDRIS2_PREFIX:-\"$HOME/.idris2\"}"))
                     `("IDRIS2_LIBS" ":" suffix (,(string-append out "/" name "/lib")))
                     `("IDRIS2_DATA" ":" suffix (,(string-append out "/" name "/support")))
                     `("IDRIS2_PACKAGE_PATH" ":" suffix ,global-libraries)
                     `("DYLD_LIBRARY_PATH" ":" suffix (,(string-append out "/" name "/lib")))
                     `("LD_LIBRARY_PATH" ":" suffix (,(string-append out "/" name "/lib"))))))))
           (delete 'configure))
         #:tests? #f))
      (home-page "https://www.idris-lang.org")
      (synopsis "A purely functional programming language with first class types")
      (description "Idris is a general purpose language with full dependent
types.  It is compiled, with eager evaluation.  Dependent types allow types to
be predicated on values, meaning that some aspects of a program's behaviour
can be specified precisely in the type.  The language is closely related to
Epigram and Agda.")
      (license license:bsd-3))))
