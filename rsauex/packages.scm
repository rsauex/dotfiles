(define-module (rsauex packages)
  #:use-module ((guix diagnostics))
  #:use-module ((guix gexp))
  #:use-module ((guix ui))
  #:use-module ((srfi srfi-26))
  #:export (search-rsauex-aux-file
            search-rsauex-private-file
            search-rsauex-patch
            search-rsauex-home-file
            rsauex-home-file))

(define %rsauex-aux-files-path
  (make-parameter
   (map (cut string-append <> "/rsauex/packages/aux-files")
        %load-path)))

(define (search-rsauex-aux-file file-name)
  (or (search-path (%rsauex-aux-files-path) file-name)
      (raise (formatted-message (G_ "~a: rsauex aux file not found")
                                file-name))))

(define %rsauex-private-files-path
  (make-parameter
   (map (cut string-append <> "/private-files")
        %load-path)))

(define* (search-rsauex-private-file file-name #:key (no-error? #f))
  (or (search-path (%rsauex-private-files-path) file-name)
      (if no-error?
          #f
          (raise (formatted-message (G_ "~a: rsauex private file not found")
                                    file-name)))))

(define %rsauex-patch-path
  (make-parameter
   (map (cut string-append <> "/rsauex/packages/patches")
        %load-path)))

(define* (search-rsauex-patch file-name #:key (no-error? #f))
  (or (search-path (%rsauex-patch-path) file-name)
      (if no-error?
          #f
          (raise (formatted-message (G_ "~a: rsauex patch not found")
                                    file-name)))))

(define* (search-rsauex-home-file file-name #:key (no-error? #f))
  (let ((file-name (if (absolute-file-name? file-name)
                       file-name
                       (string-append (getenv "HOME") "/dotfiles/home-files/" file-name))))
    (cond
     ((file-exists? file-name)
      file-name)
     (no-error?
      #f)
     (#t
      (raise (formatted-message (G_ "~a: rsauex home file not found")
                                file-name))))))

(define* (rsauex-home-file file
                           #:optional
                           (name (basename file))
                           #:key
                           (recursive? #f)
                           (select? (const #t)))
  (local-file (assume-valid-file-name (search-rsauex-home-file file))
              name #:recursive? recursive? #:select? select?))
