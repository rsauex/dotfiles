(define-module (rsauex packages xorg)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial))

;; (define xorg-intel-screen-tearing-fix
;;   "Section \"Device\"
;;        Identifier  \"Intel Graphics\"
;;        Driver      \"intel\"
;;        Option      \"AccelMethod\"  \"sna\"
;;        Option      \"TearFree\" \"true\"
;;     EndSection")

(define xorg-enable-dri3
  "Section \"Device\"
       Identifier  \"Intel Graphics\"
       Driver      \"intel\"
       Option      \"DRI\"  \"3\"
   EndSection")

(define-public my-xorg
  (let* ((start-command (xorg-start-command
                         (xorg-configuration
                          (modules
                           (list xf86-input-libinput
                                 xf86-video-intel))
                          (extra-config
                           (list xorg-enable-dri3)))))
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
