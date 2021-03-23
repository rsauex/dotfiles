(define-module (rsauex systems base)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system pam)
  #:use-module (rsauex services login)
  #:use-module (rsauex services pam-u2f)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (my-user-auth-pam-service
            my-console-font-service-type
            %my-base-services
            %my-base-pam-services))

(define (my-user-auth-pam-service name)
  (let ((base (unix-pam-service
               name
               #:login-uid? #f
               #:allow-empty-passwords? #f)))
    (pam-service
     (inherit base)
     (auth (cons* (pam-u2f-entry "sufficient")
                  (pam-service-auth base))))))

(define hidraw-udev-rule
  (udev-rule
   "10-hidrow.rules"
   (string-append "KERNEL==\"hidraw*\","
                  "SUBSYSTEM==\"hidraw\","
                  "MODE=\"0664\","
                  "GROUP=\"users\"")))

(define pam-loginuid-entry
  (pam-entry
   (control "required")
   (module "pam_loginuid.so")))

(define %my-base-services
  (list
   ;; Login
   (service my-login-service-type
            (my-login-configuration
             (pam-service
              (let ((base (my-user-auth-pam-service "login")))
                (pam-service
                 (inherit base)
                 (session (cons* pam-loginuid-entry
                                 (pam-service-session base))))))))

   ;; YubiKey
   (service pcscd-service-type)
   (udev-rules-service 'hidraw hidraw-udev-rule)

   ;; NTP
   (service ntp-service-type
            (ntp-configuration
             (allow-large-adjustment? #t)
             (servers (list (ntp-server (type 'pool)
                                        (address "0.europe.pool.ntp.org")
                                        (options '("iburst")))
                            (ntp-server (type 'pool)
                                        (address "1.europe.pool.ntp.org")
                                        (options '("iburst")))
                            (ntp-server (type 'pool)
                                        (address "2.europe.pool.ntp.org")
                                        (options '("iburst")))
                            (ntp-server (type 'pool)
                                        (address "3.europe.pool.ntp.org")
                                        (options '("iburst")))))))))

(define (my-console-font-service-type services)
  (modify-services services
    (console-font-service-type
     config =>
     (map (match-lambda
           ((tty . font)
            `(,tty . ,(file-append (@ (gnu packages fonts) font-terminus)
                                   "/share/consolefonts/ter-v16n.psf.gz"))))
          config))))

(define %my-base-pam-services
  (map (lambda (service)
         (if (string=? (pam-service-name service) "sudo")
             (pam-service
              (inherit service)
              (auth (cons* (pam-u2f-entry "sufficient")
                           (pam-service-auth service))))
             service))
       (base-pam-services #:allow-empty-passwords? #f)))
