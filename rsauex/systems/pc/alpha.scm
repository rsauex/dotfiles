(define-module (rsauex systems pc alpha)
  #:use-module ((gnu))
  #:use-module ((guix))
  #:use-module ((nongnu packages linux)      #:prefix non-linux:)
  #:use-module ((nongnu system linux-initrd) #:prefix non-linux-initrd:)
  #:use-module ((rsauex systems desktop)     #:prefix my-desktop-systems:)
  #:export (%os))

(define %os
  (operating-system
    (inherit my-desktop-systems:%my-base-desktop-system)

    (host-name "alpha")

    (kernel non-linux:linux)
    (initrd non-linux-initrd:microcode-initrd)
    (firmware (list non-linux:linux-firmware))

    (mapped-devices (list (mapped-device
                            (source (uuid "eb58fa70-7419-4e29-8d9f-30c53f67cc26"))
                            (target "cryptroot")
                            (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "1776-7414" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "/dev/mapper/cryptroot")
                           (mount-point "/")
                           (type "ext4")
                           (flags '(no-atime))
                           (dependencies mapped-devices))
                         (operating-system-file-systems my-desktop-systems:%my-base-desktop-system)))

    (packages (cons*
               (@ (nongnu packages game-client) steam)

               (operating-system-packages my-desktop-systems:%my-base-desktop-system)))))
