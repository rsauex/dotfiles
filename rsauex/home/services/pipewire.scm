(define-module (rsauex home services pipewire)
  #:use-module ((gnu home services))
  #:use-module ((gnu packages linux)        #:prefix linux:)
  #:use-module ((gnu packages pulseaudio)   #:prefix pulseaudio:)
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))
  #:export (pipewire-configuration

            pipewire-extension

            pipewire-service-type))

(define-configuration/no-serialization pipewire-configuration
  (pipewire
   (package linux:pipewire)
   "The pipewire package to use")
  (wireplumber
   (package linux:wireplumber)
   "The wireplumber package to use")
  (pulseaudio
   (package pulseaudio:pulseaudio)
   "The pulseaudio package to use")
  (extra-configs
   (alist '())
   "Alist of extra config files"))

(define-configuration/no-serialization pipewire-extension
  (extra-configs
   (alist '())
   "Alist of extra config files"))

(define (pipewire-extensions original-config extension-configs)
  (pipewire-configuration
   (inherit original-config)
   (extra-configs
    (concatenate (cons (pipewire-configuration-extra-configs original-config)
                       (map pipewire-extension-extra-configs extension-configs))))))

(define (pipewire-profile-service config)
  (list (pipewire-configuration-pipewire config)
        (pipewire-configuration-wireplumber config)))

(define (pipewire-pulse-wrapper config)
  (let ((pipewire (pipewire-configuration-pipewire config))
        (pulseaudio (pipewire-configuration-pulseaudio config)))
    (program-file
     "pipewire-pulse-wrapper"
     #~(let ((pulseaudio-bin #$(file-append pulseaudio "/bin"))
             (pipewire-pulse #$(file-append pipewire "/bin/pipewire-pulse")))
         (setenv "PATH" (string-join (list (getenv "PATH") pulseaudio-bin) ":"))
         (execl pipewire-pulse pipewire-pulse)))))

(define (pipewire-gui-startup-service config)
  (let ((pipewire (pipewire-configuration-pipewire config))
        (wireplumber (pipewire-configuration-wireplumber config))
        (pipewire-pulse (pipewire-pulse-wrapper config)))
    (my-gui-startup:gui-startup-extension
     (services
      (list (my-shepherd:simple-forkexec-shepherd-service
             'pipewire
             "Run `pipewire'"
             #~`(#$(file-append pipewire "/bin/pipewire")))
            (my-shepherd:simple-forkexec-shepherd-service
             'pipewire-pulse
             "Run `pipewire-pulse'"
             #~`(#$pipewire-pulse)
             #:requirement '(pipewire))
            (my-shepherd:simple-forkexec-shepherd-service
             'wireplumber
             "Run `wireplumber'"
             #~`(#$(file-append wireplumber "/bin/wireplumber"))
             #:requirement '(pipewire)))))))

(define (pipewire-home-xdg-configuration-files-service config)
  (map (lambda (extra-config)
         (cons (string-append "pipewire/pipewire.conf.d/" (car extra-config))
               (cdr extra-config)))
       (pipewire-configuration-extra-configs config)))

(define pipewire-service-type
  (service-type (name 'pipewire)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        pipewire-profile-service)
                       (service-extension
                        my-gui-startup:gui-startup-service-type
                        pipewire-gui-startup-service)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        pipewire-home-xdg-configuration-files-service)))
                (compose identity)
                (extend pipewire-extensions)
                (default-value (pipewire-configuration))
                (description "Pipewire startup")))
