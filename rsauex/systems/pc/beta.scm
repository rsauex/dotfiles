(define-module (rsauex systems pc beta)
  #:use-module ((gnu packages xorg)          #:prefix xorg:)
  #:use-module ((gnu services dbus)          #:prefix dbus-services:)
  #:use-module ((gnu services xorg)          #:prefix xorg-services:)
  #:use-module ((gnu))
  #:use-module ((guix build-system trivial))
  #:use-module ((guix))
  #:use-module ((nongnu packages linux)           #:prefix non-linux:)
  #:use-module ((nongnu system linux-initrd)      #:prefix non-linux-initrd:)
  #:use-module ((rsauex services intel-backlight) #:prefix my-intel-backlight:)
  #:use-module ((rsauex systems desktop)          #:prefix my-desktop-systems:)
  #:export (%os))

(define xorg-enable-dri3
  "Section \"Device\"
       Identifier  \"Intel Graphics\"
       Driver      \"intel\"
       Option      \"DRI\"  \"3\"
   EndSection")

(define %os
  (operating-system
    (inherit my-desktop-systems:%my-base-desktop-system)

    (host-name "beta")

    (kernel non-linux:linux)
    (initrd non-linux-initrd:microcode-initrd)
    (firmware (list non-linux:linux-firmware))

    (mapped-devices (list (mapped-device
                           (source (uuid "1caaeb4e-d8a9-45e0-a2ea-6b84ec9478e8"))
                           (target "cryptroot")
                           (type luks-device-mapping))))

    (file-systems (cons* (file-system
                           (device (uuid "3A7B-4B26" 'fat))
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

    (packages (cons* (@ (gnu packages video) intel-vaapi-driver)
                     (operating-system-packages my-desktop-systems:%my-base-desktop-system)))

    (services (cons* (my-intel-backlight:intel-backlight-service)
                     (modify-services
                         (operating-system-user-services my-desktop-systems:%my-base-desktop-system)
                       (xorg-services:xorg-server-service-type
                        config => (xorg-services:xorg-configuration
                                   (extra-config
                                    (cons* xorg-enable-dri3
                                           (xorg-services:xorg-configuration-extra-config config))))))))))
