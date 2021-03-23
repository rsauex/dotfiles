(define-module (rsauex packages xorg)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services dbus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial)
  #:export (intel-backlight-service))

(define intel-backlight-service
  ;; Solution to make intel backlight work for non-root users...
  (simple-service 'intel-backlight
                  polkit-service-type
                  (list xf86-video-intel)))

(define-public (my-xorg config)
  (let* ((start-command (xorg-start-command config))
         (startx (program-file
                  "startx"
                  #~(let ((xinit (string-append #$xinit "/bin/xinit")))
                      (execl xinit xinit
                             "--" #$start-command ":0" "vt1" "-keeptty")))))
    (package
      (name "my-xorg")
      (version "0.0.1")
      (source #f)
      (build-system trivial-build-system)
      (arguments (list #:modules '((guix build utils))
                       #:builder `(begin
                                    (use-modules (guix build utils))
                                    (let* ((output (string-append (assoc-ref %outputs "out") "/bin"))
                                           (startx (string-append output "/startx")))
                                      (mkdir-p output)
                                      (copy-file (assoc-ref %build-inputs "startx") startx)
                                      (chmod startx #o755)))))
      (inputs `(("startx" ,startx)))
      (synopsis "My xorg config")
      (description "")
      (home-page "")
      (supported-systems (list "x86_64-linux"))
      (license expat))))
