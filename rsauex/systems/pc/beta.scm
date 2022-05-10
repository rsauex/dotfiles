(define-module (rsauex systems pc beta)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module ((gnu packages xorg) #:prefix xorg:)
  #:use-module (gnu services dbus)
  #:use-module (gnu services xorg)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages linux)
  #:use-module (rsauex systems base)
  #:use-module (rsauex systems desktop)
  #:export (%os))

(define intel-backlight-service
  ;; Solution to make intel backlight work for non-root users...
  (simple-service 'intel-backlight
                  polkit-service-type
                  (list xorg:xf86-video-intel)))

(define xorg-enable-dri3
  "Section \"Device\"
       Identifier  \"Intel Graphics\"
       Driver      \"intel\"
       Option      \"DRI\"  \"3\"
   EndSection")

(define %os
  (operating-system
    (inherit %my-base-desktop-system)

    (host-name "beta")

    (kernel linux)
    (initrd microcode-initrd)
    (firmware (list linux-firmware))

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
                         %base-file-systems))

    (swap-devices (list (swap-space
                         (target "/swapfile"))))

    (packages (cons* (@ (gnu packages xorg) xbacklight)

                     (@ (gnu packages clojure) clojure)
                     (@ (nongnu packages clojure) leiningen)
                     (list (@ (gnu packages java) openjdk11) "jdk")

                     (@ (gnu packages video) vlc)
                     (@ (gnu packages video) intel-vaapi-driver)

                     (operating-system-packages %my-base-desktop-system)))

    (services (cons* intel-backlight-service
                     (modify-services
                         (operating-system-user-services %my-base-desktop-system)
                       (xorg-server-service-type
                        config => (xorg-configuration
                                   (extra-config
                                    (cons* xorg-enable-dri3
                                           (xorg-configuration-extra-config config))))))))))
