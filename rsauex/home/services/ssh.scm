(define-module (rsauex home services ssh)
  #:use-module ((gnu home))
  #:use-module ((gnu packages ssh) #:prefix ssh:)
  #:use-module ((gnu packages))
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd) #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))

  #:export (ssh-agent-configuration

            ssh-agent-configuration-expiration
            ssh-agent-configuration-socket

            ssh-agent-service-type))

(define-configuration/no-serialization ssh-agent-configuration
  (expiration
   (string "0")
   "Default value for the maximum lifetime of identities added to the agent.")
  (socket
   (gexp #~(string-append (getenv "XDG_RUNTIME_DIR") "/ssh-agent-" (getenv "XDG_SESSION_ID") "-socket"))
   "Socket to bind the ssh agent to."))

(define (add-ssh-agent-shepherd-service config)
  (let ((expiration (ssh-agent-configuration-expiration config))
        (socket (ssh-agent-configuration-socket config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'ssh-agent
             "Run `ssh-agent'"
             #~`(#$(file-append ssh:openssh "/bin/ssh-agent") "-D" "-t" ,#$expiration "-a" ,#$socket))))
     (environment
      (list (cons "SSH_AUTH_SOCK" socket))))))

(define ssh-agent-service-type
  (service-type (name 'ssh-agent)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        add-ssh-agent-shepherd-service)))
                (default-value (ssh-agent-configuration))
                (description "Run ssh-agent.")))
