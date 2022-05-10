(define-module (rsauex systems base)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module ((gnu packages fonts) #:prefix fonts:)
  #:use-module (gnu services security-token)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu system pam)
  #:use-module (rsauex services pam-u2f)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (my-console-font-service-type
            %my-base-services))

(define hidraw-udev-rule
  (udev-rule
   "10-hidrow.rules"
   (string-append "KERNEL==\"hidraw*\","
                  "SUBSYSTEM==\"hidraw\","
                  "MODE=\"0664\","
                  "GROUP=\"users\","
                  "ATTRS{idVendor}==\"1050\"")))

(define %my-base-services
  (list
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
             `(,tty . ,(file-append fonts:font-terminus
                                    "/share/consolefonts/ter-v16n.psf.gz"))))
          config))))
