(define-module (rsauex services intel-backlight)
  #:use-module ((gnu packages xorg)          #:prefix xorg:)
  #:use-module ((gnu services dbus)          #:prefix dbus-services:)
  #:use-module ((gnu))
  #:use-module ((guix build-system trivial))
  #:use-module ((guix))
  #:use-module ((srfi srfi-1))
  #:use-module ((srfi srfi-26))
  #:export (intel-backlight-service))

(define (xxx)
  (define xx
    (let ((package xorg:xf86-video-intel))
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (mkdir-p (string-append #$output "/share/polkit-1"))
            (copy-recursively (string-append #$package "/share/polkit-1")
                              (string-append #$output "/share/polkit-1"))))))
  (computed-file "xf86-video-intel-polkit" xx))

(define xf86-video-intel-polkit
  (package
    (name "xf86-video-intel-polkit")
    (version (package-version xorg:xf86-video-intel))
    (source (xxx))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out")))
           (mkdir-p (string-append out "/share/polkit-1"))
           (copy-recursively (string-append source "/share/polkit-1")
                             (string-append out "/share/polkit-1"))
           #t))))
    (home-page (package-home-page xorg:xf86-video-intel))
    (synopsis (package-synopsis xorg:xf86-video-intel))
    (description (package-description xorg:xf86-video-intel))
    (license (package-license xorg:xf86-video-intel))))

(define (intel-backlight-service)
  ;; Solution to make intel backlight work for non-root users...
  (simple-service 'intel-backlight
                  dbus-services:polkit-service-type
                  (list xf86-video-intel-polkit)))
