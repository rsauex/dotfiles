(define-module (rsauex home services git)
  #:use-module ((gnu home services))
  #:use-module ((gnu services configuration))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((srfi srfi-1))

  #:export (git-configuration

            git-service-type))

(define git-hooks
  (list #:applypatch-msg
        #:pre-applypatch
        #:post-applypatch
        #:pre-commit
        #:pre-merge-commit
        #:prepare-commit-msg
        #:commit-msg
        #:post-commit
        #:pre-rebase
        #:post-checkout
        #:post-merge
        #:pre-push
        #:pre-receive
        #:update
        #:proc-receive
        #:post-receive
        #:post-update
        #:reference-transaction
        #:push-to-checkout
        #:pre-auto-gc
        #:post-rewrite
        #:sendemail-validate
        #:fsmonitor-watchman
        #:p4-changelist
        #:p4-prepare-changelist
        #:p4-post-changelist
        #:p4-pre-submit
        #:post-index-change))

(define (git-hook? thing)
  (and (pair? thing)
       (member (car thing) git-hooks)
       (file-like? (cdr thing))))

(define (list-of-git-hooks? thing)
  (and (list? thing)
       (every git-hook? thing)))

(define-configuration/no-serialization git-configuration
  (hooks
   (list-of-git-hooks '())
   "The cursor theme package."))

(define (git-configuration-hooks-of-type config type)
  (let ((all-hooks (git-configuration-hooks config)))
    (map cdr (filter (lambda (val) (eq? type (car val))) all-hooks))))

(define (make-git-hook hook-name hooks)
  (program-file
   hook-name
   #~(begin
       (use-modules
        ((ice-9 popen))
        ((ice-9 textual-ports)))

       (define args
         (cdr (program-arguments)))

       (define input
         (get-string-all (current-input-port)))

       (define (run-hook hook)
         (let ((pipe (apply open-pipe* OPEN_WRITE hook args)))
           (put-string pipe input)
           (let ((exit-val (status:exit-val (close-pipe pipe))))
             (unless (zero? exit-val)
               (exit exit-val)))))

       (for-each run-hook (list #$@hooks))

       (let ((local-hook #$(string-append "./.git/hooks/" hook-name)))
         (when (file-exists? local-hook)
           (run-hook local-hook)))

       (exit 0))))

(define (add-git-xdg-configuration-files config)
  (map (lambda (hook)
         (let ((hook-name (symbol->string (keyword->symbol hook)))
               (hooks (git-configuration-hooks-of-type config hook)))
           `(,(string-append "git/hooks/" hook-name)
             ,(make-git-hook hook-name hooks))))
       git-hooks))

(define git-service-type
  (service-type (name 'git)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-git-xdg-configuration-files)))
                (default-value (git-configuration))
                (description "Git configuration.")))
