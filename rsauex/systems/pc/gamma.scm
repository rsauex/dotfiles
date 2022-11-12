(define-module (rsauex systems pc gamma)
  #:use-module ((gnu packages xorg)          #:prefix xorg:)
  #:use-module ((gnu services dbus)          #:prefix dbus-services:)
  #:use-module ((gnu services xorg)          #:prefix xorg-services:)
  #:use-module ((gnu))
  #:use-module ((guix build-system trivial))
  #:use-module ((guix))
  ;; #:use-module ((nongnu packages linux)      #:prefix non-linux:)
  ;; #:use-module ((nongnu system linux-initrd) #:prefix non-linux-initrd:)
  #:use-module ((rsauex systems desktop)     #:prefix my-desktop-systems:)
  #:export (%os))

(define %os
  (operating-system
    (inherit my-desktop-systems:%my-base-desktop-system)

    (host-name "gamma")

    ;; (kernel non-linux:linux)
    ;; (initrd non-linux-initrd:microcode-initrd)
    ;; (firmware (list non-linux:linux-firmware))

    (mapped-devices (list (mapped-device
                           (source (uuid "52edef29-e6a6-457b-9853-350b8c2d2933"))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "4807-194D" 'fat))
                           (mount-point "/boot/efi")
                           (type "vfat"))
                         (file-system
                           (device "/dev/mapper/cryptroot")
                           (mount-point "/")
                           (type "ext4")
                           (flags '(no-atime))
                           (dependencies mapped-devices))
                         (operating-system-file-systems my-desktop-systems:%my-base-desktop-system)))

    (swap-devices (list (swap-space
                         (target "/swapfile"))))

    (packages (cons* (operating-system-packages my-desktop-systems:%my-base-desktop-system)))

    (services (cons* (operating-system-user-services my-desktop-systems:%my-base-desktop-system)))))
