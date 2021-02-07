(define-module (rsauex packages)
  #:use-module (guix discovery)
  #:use-module (guix describe)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (search-rsauex-aux-file))

(define %rsauex-aux-files-path
  (make-parameter
   (map (cut string-append <> "/rsauex/packages/aux-files")
        %load-path)))

(define (search-rsauex-aux-file file-name)
  (or (search-path (%rsauex-aux-files-path) file-name)
      (raise (formatted-message (G_ "~a: rsauex aux file not found")
                                file-name))))
