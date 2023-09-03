(define-module (rsauex home services screensaver)
  #:use-module ((gnu home))
  #:use-module ((gnu packages xdisorg)             #:prefix xdisorg:)
  #:use-module ((gnu packages xorg)                #:prefix xorg:)
  #:use-module ((gnu packages))
  #:use-module ((gnu services configuration))
  #:use-module ((gnu services))
  #:use-module ((guix gexp))
  #:use-module ((guix packages))
  #:use-module ((rsauex home services gui-startup) #:prefix my-gui-startup:)
  #:use-module ((rsauex home services shepherd)    #:prefix my-shepherd:)
  #:use-module ((srfi srfi-1))

  #:export (xss-lock-configuration
            xss-lock-configuration?
            xss-lock-configuration-xss-lock
            xss-lock-configuration-xssproxy
            xss-lock-configuration-screen-blanking-timeout
            xss-lock-configuration-screen-standby-timeout
            xss-lock-configuration-screen-suspend-timeout
            xss-lock-configuration-screen-off-timeout
            xss-lock-configuration-transfer-sleep-lock
            xss-lock-configuration-locker-expr

            xss-lock-service-type))

;; TODO: notify in xss-lock

(define (positive-int? thing)
  (and (exact-integer? thing)
       (> thing 0)))

(define-maybe/no-serialization positive-int)

(define-configuration/no-serialization xss-lock-configuration
  (xss-lock
   (package xdisorg:xss-lock)
   "Xss-lock package to use.")
  (xssproxy
   (package xdisorg:xssproxy)
   "Xssproxy package to use.")
  (screen-blanking-timeout
   (maybe-positive-int)
   "After how many seconds should the screen be blanked. (Disable when not
specified)")
  (screen-standby-timeout
   (maybe-positive-int)
   "After how many seconds should the screen be put into standby mode.  (Disable
when not specified)")
  (screen-suspend-timeout
   (maybe-positive-int)
   "How many seconds  should the screen be put into suspend mode. (Disable
when not specified)")
  (screen-off-timeout
   (maybe-positive-int)
   "After how many seconds should the screen be turned off. (Disable when not
specified)")
  (transfer-sleep-lock
   (boolean #t)
   "Allow the locker process to inherit the file descriptor that represents the
delay lock obtained from the login manager.")
  (locker-expr
   (gexp (error "`locker-expr` must be provided!"))
   "Gexpr than specifies the screen locker program along with the arguments."))

(define (xssproxy-shepherd-service config)
  (let ((xssproxy-package (xss-lock-configuration-xssproxy config)))
    (my-shepherd:simple-forkexec-shepherd-service
     'screensaver-xssproxy
     "Run `xssproxy'"
     #~(let ((xssproxy #$(file-append xssproxy-package "/bin/xssproxy")))
         `(xssproxy)))))

(define (xss-lock-shepherd-service config)
  (let* ((xss-lock-package (xss-lock-configuration-xss-lock config))
         (transfer-sleep-lock (xss-lock-configuration-transfer-sleep-lock config))
         (locker-expr (xss-lock-configuration-locker-expr config)))
    (my-shepherd:simple-forkexec-shepherd-service
     'screensaver-xss-lock
     "Run `xss-lock'"
     #~`(#$(file-append xss-lock-package "/bin/xss-lock")
         ,@#$(if transfer-sleep-lock #~`("-l") #~`())
         "--"
         ,@#$locker-expr))))

(define (xset-screensaver-shepherd-service config)
  (let ((blanking-timeout (maybe-value (xss-lock-configuration-screen-blanking-timeout config) 0))
        (standby-timeout (maybe-value (xss-lock-configuration-screen-standby-timeout config) 0))
        (suspend-timeout (maybe-value (xss-lock-configuration-screen-suspend-timeout config) 0))
        (off-timeout (maybe-value (xss-lock-configuration-screen-off-timeout config) 0)))
    (my-shepherd:simple-one-shot-shepherd-service
     'screensaver-xset
     "Set `xset' settings for screensaver"
     #~(lambda ()
         (let ((xset #$(file-append xorg:xset "/bin/xset")))
           (invoke xset "s" (number->string blanking-timeout) "0")
           (invoke xset "+dpms")
           (invoke xset "dpms"
                   (number->string standby-timeout)
                   (number->string suspend-timeout)
                   (number->string off-timeout)))
         #t))))

(define (add-xss-lock-shepherd-services config)
  (my-gui-startup:gui-startup-extension
   (services
    (list (xssproxy-shepherd-service config)
          (xss-lock-shepherd-service config)
          (xset-screensaver-shepherd-service config)))))

(define xss-lock-service-type
  (service-type (name 'screensaver)
                (extensions
                 (list (service-extension
                        my-gui-startup:gui-startup-service-type
                        add-xss-lock-shepherd-services)))
                (description "Setup screen saver/screen locker using xss-lock")))
