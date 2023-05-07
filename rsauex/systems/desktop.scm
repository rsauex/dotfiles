(define-module (rsauex systems desktop)
  #:use-module ((gnu packages base)               #:prefix base:)
  #:use-module ((gnu packages cups)               #:prefix cups:)
  #:use-module ((gnu packages gnome)              #:prefix gnome:)
  #:use-module ((gnu packages libusb)             #:prefix libusb:)
  #:use-module ((gnu packages linux)              #:prefix linux:)
  #:use-module ((gnu packages networking)         #:prefix networking:)
  #:use-module ((gnu packages package-management) #:prefix package-management:)
  #:use-module ((gnu packages wm)                 #:prefix wm:)
  #:use-module ((gnu services avahi)              #:prefix avahi-services:)
  #:use-module ((gnu services base)               #:prefix base-services:)
  #:use-module ((gnu services cups)               #:prefix cups-services:)
  #:use-module ((gnu services dbus)               #:prefix dbus-services:)
  #:use-module ((gnu services desktop)            #:prefix desktop-services:)
  #:use-module ((gnu services docker)             #:prefix docker-services:)
  #:use-module ((gnu services networking)         #:prefix networking-services:)
  #:use-module ((gnu services xorg)               #:prefix xorg-services:)
  #:use-module ((gnu system pam)                  #:prefix pam:)
  #:use-module ((gnu))
  #:use-module ((rsauex packages nm-forti)        #:prefix my-nm-forti:)
  #:use-module ((rsauex services pam-u2f)         #:prefix my-pam-u2f-services:)
  #:use-module ((rsauex systems base)             #:prefix my-base-systems:)
  #:use-module ((srfi srfi-1))
  #:export (%my-desktop-packages
            %my-desktop-services
            %my-base-desktop-system))

(define %my-desktop-packages
  (list package-management:flatpak
        gnome:gvfs))

;; TODO: Better name!
(define (my-pam-u2f-auth-service)
  (define (my-pam-u2f-auth-extension pam)
    (if (member (pam:pam-service-name pam) '("login" "su" "sudo" "i3lock"))
        (pam:pam-service
         (inherit pam)
         (auth (cons* (my-pam-u2f-services:pam-u2f-entry "sufficient")
                      (pam:pam-service-auth pam))))
        pam))

  (simple-service 'pam-u2f pam:pam-root-service-type (list my-pam-u2f-auth-extension)))

(define brightness-access-for-video-group
  (file->udev-rule
   "90-backlight.rules"
   (mixed-text-file
    "backlight.rules"
    "SUBSYSTEM==\"backlight\","
    "ACTION==\"add\","
    "RUN+=\"" base:coreutils "/bin/chgrp video /sys/class/backlight/%k/brightness\","
    "RUN+=\"" base:coreutils "/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define (add-nonguix-substitute services)
  (modify-services services
    (guix-service-type
     config =>
     (guix-configuration
      (inherit config)
      (substitute-urls
       (append (list "https://substitutes.nonguix.org")
               (guix-configuration-substitute-urls config)))
      (authorized-keys
       (append (list (plain-file "non-guix.pub"
                                 "(public-key
                                   (ecc
                                    (curve Ed25519)
                                    (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
               (guix-configuration-authorized-keys config)))))))

(define %my-desktop-services
  (list
   ;; Gvfs
   (simple-service 'gvfs-polkit
                   dbus-services:polkit-service-type
                   (list gnome:gvfs))

   ;; Allow u2f for auth
   (my-pam-u2f-auth-service)

   ;; Xorg
   (service desktop-services:x11-socket-directory-service-type)
   (service xorg-services:xorg-server-service-type)
   (service xorg-services:screen-locker-service-type
            (xorg-services:screen-locker-configuration
             "i3lock" (file-append wm:i3lock "/bin/i3lock") #f))

   ;; Bluetooth
   (service desktop-services:bluetooth-service-type
            (desktop-services:bluetooth-configuration
             (auto-enable? #f)))
   (simple-service 'blueman-dbus
                   dbus-services:dbus-root-service-type
                   (list networking:blueman))
   (simple-service 'blueman-polkit
                   dbus-services:polkit-service-type
                   (list networking:blueman))

   ;; Docker
   (service docker-services:docker-service-type)

   ;; Add udev rules for MTP devices so that non-root users can access
   ;; them.
   (simple-service 'mtp base-services:udev-service-type (list libusb:libmtp))

   ;; CUPS
   (service cups-services:cups-service-type
            (cups-services:cups-configuration
             (extensions
              (list cups:cups-filters))
             (default-paper-size "A4")))

   ;; Add udev rules for scanners.
   (service desktop-services:sane-service-type)

   ;; Add polkit rules, so that non-root users in the wheel group can
   ;; perform administrative tasks (similar to "sudo").
   desktop-services:polkit-wheel-service

   ;; The global fontconfig cache directory can sometimes contain
   ;; stale entries, possibly referencing fonts that have been GC'd,
   ;; so mount it read-only.
   desktop-services:fontconfig-file-system-service

   ;; Network manager
   (service networking-services:network-manager-service-type
            (networking-services:network-manager-configuration
             (dns "dnsmasq")
             (vpn-plugins
              (list my-nm-forti:network-manager-openfortivpn))))
   (service networking-services:wpa-supplicant-service-type)
   (service networking-services:modem-manager-service-type)
   (service networking-services:usb-modeswitch-service-type)
   (simple-service 'network-manager-applet
                   profile-service-type
                   (list gnome:network-manager-applet))

   ;; D-Bus services
   (service dbus-services:dbus-root-service-type)
   (service dbus-services:polkit-service-type)
   (service desktop-services:elogind-service-type)
   (service desktop-services:accountsservice-service-type)
   (service desktop-services:colord-service-type)
   (service desktop-services:cups-pk-helper-service-type)
   (service avahi-services:avahi-service-type)
   (service desktop-services:udisks-service-type)
   (service desktop-services:upower-service-type
            (desktop-services:upower-configuration
             (percentage-critical 10)
             (percentage-action 5)))
   (service desktop-services:geoclue-service-type)

   ;; Brightenss access for ordinary users
   (simple-service 'brightness-udev base-services:udev-service-type (list brightness-access-for-video-group))

   ;; Pipewire
   (simple-service 'pipewire-udev base-services:udev-service-type (list linux:pipewire))

   ;; PAM
   (base-services:pam-limits-service (list
                                      ;; Increase max open files
                                      (pam:pam-limits-entry "*" 'both 'nofile 65536)))))

(define %my-base-desktop-system
  (operating-system
    (inherit my-base-systems:%my-base-system)

    (packages (append %my-desktop-packages
                      (operating-system-packages my-base-systems:%my-base-system)))

    (services (append %my-desktop-services
                      ((compose add-nonguix-substitute)
                       (operating-system-user-services my-base-systems:%my-base-system))))))
