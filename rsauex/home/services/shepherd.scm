(define-module (rsauex home services shepherd)
  #:use-module ((gnu home services shepherd)
                #:select (home-shepherd-configuration
                           home-shepherd-configuration?
                           home-shepherd-configuration-shepherd
                           home-shepherd-configuration-auto-start?
                           home-shepherd-configuration-services))
  #:use-module ((gnu home services))
  #:use-module ((gnu packages admin))
  #:use-module ((gnu services shepherd))
  #:use-module ((guix gexp))
  #:use-module ((guix records))
  #:use-module ((guix sets))
  #:use-module ((srfi srfi-1))

  #:export (home-shepherd-service-type

            simple-forkexec-shepherd-service
            simple-one-shot-shepherd-service

            home-shepherd-configuration-file
            home-shepherd-session-launch-file)
  #:re-export (shepherd-service
                shepherd-service?
                shepherd-service-documentation
                shepherd-service-provision
                shepherd-service-canonical-name
                shepherd-service-requirement
                shepherd-service-one-shot?
                shepherd-service-respawn?
                shepherd-service-start
                shepherd-service-stop
                shepherd-service-auto-start?
                shepherd-service-modules

                shepherd-action

                home-shepherd-configuration
                home-shepherd-configuration?
                home-shepherd-configuration-shepherd
                home-shepherd-configuration-auto-start?
                home-shepherd-configuration-services))

(define (home-shepherd-configuration-file config)
  "Return the shepherd configuration file for SERVICES.  SHEPHERD is used
as shepherd package."
  (let* ((services (home-shepherd-configuration-services config))
         (autostart-services (filter shepherd-service-auto-start? services)))
    (assert-valid-graph services)
    (let ((files (map shepherd-service-file services)))
      (define config
        #~(begin
            (use-modules (srfi srfi-34)
                         (system repl error-handling))
            (apply register-services
                   (map (lambda (file) (load file))
                        '#$files))
            (format #t "Starting services...~%")
            (let ((services-to-start '#$(append-map shepherd-service-provision autostart-services)))
              (if (defined? 'start-in-the-background)
                  (start-in-the-background services-to-start)
                  (for-each start services-to-start)))))

      (scheme-file "shepherd.conf" config))))

(define (home-shepherd-session-launch-file config)
  (let ((shepherd (file-append (home-shepherd-configuration-shepherd config) "/bin/shepherd")))
    (program-file
     "schepherd-launch"
     #~(begin
         (use-modules (shepherd support))

         (define command
           (cdr (command-line)))

         (define session-id
           (or (getenv "XDG_SESSION_ID")
               (error "Environment variable 'XDG_SESSION_ID' is not defined!")))

         (define socket
           (string-append default-socket-dir "/socket-" session-id))

         (define shepherd-pid
           #f)

         (define (terminate)
           (when shepherd-pid
             (kill shepherd-pid SIGTERM)
             (waitpid shepherd-pid))
           (delete-file socket)
           (primitive-exit 0))

         (for-each (lambda (signal)
                     (sigaction signal (lambda (_) (terminate))))
                   (list SIGINT SIGTERM SIGHUP))

         (set! shepherd-pid (primitive-fork))

         (if (= shepherd-pid 0)
             (execlp #$shepherd
                     #$shepherd
                     "--socket"
                     socket
                     "--config"
                     #$(home-shepherd-configuration-file config))
             (begin
               (setenv "SHEPHERD_SESSION_SOCKET" socket)
               (apply system* command)
               (terminate)))))))

(define-public home-shepherd-service-type
  (service-type (name 'home-shepherd)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        (lambda (config)
                          `(,(home-shepherd-configuration-shepherd config))))
                       (service-extension
                        home-xdg-configuration-files-service-type
                        (lambda (config)
                          `(("shepherd/shepherd.conf"
                             ,(home-shepherd-configuration-file config)))))))
                (compose concatenate)
                (extend
                 (lambda (config extra-services)
                   (home-shepherd-configuration
                     (inherit config)
                     (services
                      (append (home-shepherd-configuration-services config)
                              extra-services)))))
                (default-value (home-shepherd-configuration))
                (description "Configure and install userland Shepherd.")))

(define (add-packages-to-xdg-data-dirs-gexp packages env-gexp)
  #~(let* ((vars #$env-gexp)
           (xdg-data-dirs? (cut string-prefix? "XDG_DATA_DIRS=" <>))
           (data (find xdg-data-dirs? vars))
           (vars (remove xdg-data-dirs? vars))
           (data (cons* #$@(map (lambda (package)
                                  (file-append package "/share"))
                                packages)
                        (if data (list (substring data 14)) (list)))))
      (if (null? data)
          vars
          (cons (string-append "XDG_DATA_DIRS=" (string-join data ":")) vars))))

(define* (simple-forkexec-shepherd-service name documentation command-gexp
                                           #:key
                                           (respawn? #t)
                                           (requirement '())
                                           (data-packages '()))
  (shepherd-service
    (documentation documentation)
    (provision (list name))
    (start #~(make-forkexec-constructor
              #$command-gexp
              #:environment-variables #$(add-packages-to-xdg-data-dirs-gexp
                                         data-packages
                                         #~(default-environment-variables))))
    (stop #~(make-kill-destructor))
    (respawn? respawn?)
    (requirement requirement)
    (modules (cons* `(srfi srfi-1)
                    `(srfi srfi-26)
                    %default-modules))))

(define* (simple-one-shot-shepherd-service name documentation command-gexp
                                           #:key
                                           (auto-start? #t)
                                           (requirement '())
                                           (extra-modules '()))
  (shepherd-service
    (documentation documentation)
    (provision (list name))
    (start command-gexp)
    (one-shot? #t)
    (auto-start? auto-start?)
    (requirement requirement)
    (modules (append %default-modules extra-modules))))
