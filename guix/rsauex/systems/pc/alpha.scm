(define-module (rsauex systems pc alpha)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (rsauex systems base)
  #:use-module (rsauex systems desktop)
  #:export (%os))

(define %os
  (operating-system
    (inherit %my-base-desktop-system)

    (host-name "alpha")

    (mapped-devices (list (mapped-device
                           (source (uuid ""))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "/dev/mapper/cryptroot")
                           (mount-point "/")
                           (type "ext4")
                           (flags '(no-atime))
                           (dependencies mapped-devices))
                         %base-file-systems))

    (packages (cons*
               (operating-system-packages %my-base-desktop-system)))))
