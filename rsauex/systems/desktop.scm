(define-module (rsauex systems desktop)
  #:use-module ((gcrypt pk-crypto)                #:prefix pk-crypto:)
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
  #:use-module ((gnu services sysctl)             #:prefix sysctl-services:)
  #:use-module ((gnu services xorg)               #:prefix xorg-services:)
  #:use-module ((gnu system pam)                  #:prefix pam:)
  #:use-module ((gnu))
  #:use-module ((rsauex channels)                 #:prefix my-channels:)
  #:use-module ((rsauex packages docker)          #:prefix my-docker:)
  #:use-module ((rsauex packages xorg)            #:prefix my-xorg:)
  #:use-module ((rsauex services pam-u2f)         #:prefix my-pam-u2f-services:)
  #:use-module ((rsauex systems base)             #:prefix my-base-systems:)
  #:use-module ((rsauex packages firewalld)       #:prefix my-firewalld:)
  #:use-module ((srfi srfi-1))
  #:export (%my-desktop-packages
            %my-desktop-services
            %my-base-desktop-system))

(define %my-desktop-packages
  (list package-management:flatpak
        gnome:gvfs
        my-docker:docker-compose-2))

;; TODO: Better name!
(define (my-pam-u2f-auth-service)
  (define (my-pam-u2f-auth-extension pam)
    (if (member (pam:pam-service-name pam) '("login" "su" "sudo" "screen-locker"))
        (pam:pam-service
         (inherit pam)
         (auth (cons* (my-pam-u2f-services:pam-u2f-entry "sufficient")
                      (pam:pam-service-auth pam))))
        pam))

  (simple-service 'pam-u2f pam:pam-root-service-type
                  (list (pam:pam-extension
                         (transformer my-pam-u2f-auth-extension)))))

(define brightness-access-for-video-group
  (file->udev-rule
   "90-backlight.rules"
   (mixed-text-file
    "backlight.rules"
    "SUBSYSTEM==\"backlight\","
    "ACTION==\"add\","
    "RUN+=\"" base:coreutils "/bin/chgrp video /sys/class/backlight/%k/brightness\","
    "RUN+=\"" base:coreutils "/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define tobii-access-udev
  (file->udev-rule
   "90-talon.rules"
   (mixed-text-file
    "talon.rules"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0127\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0118\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0106\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0128\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"010a\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0102\", GROUP=\"users\", MODE=\"0666\"\n"
    "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"2104\", ATTRS{idProduct}==\"0313\", GROUP=\"users\", MODE=\"0666\"\n")))

(define card-reader-udev
  (file->udev-rule
   "90-card-reader-enable-polling.rules"
   (mixed-text-file
    "card-reader-enable-polling.rules"
    "SUBSYSTEM==\"block\", SUBSYSTEMS==\"usb\", ATTRS{idVendor}==\"05e3\", ATTRS{idProduct}==\"0748\", ATTR{events_poll_msecs}=\"1000\"\n")))

(define (add-nonguix-substitute services)
  (modify-services services
    (guix-service-type
     config =>
     (guix-configuration
      (inherit config)
      (substitute-urls
       (append (list my-channels:nonguix-substitute-url)
               (guix-configuration-substitute-urls config)))
      (authorized-keys
       (append (list (plain-file "non-guix.pub"
                                 (pk-crypto:canonical-sexp->string
                                  (pk-crypto:sexp->canonical-sexp
                                   my-channels:nonguix-substitute-primary-key))))
               (guix-configuration-authorized-keys config)))))))

(define default-xorg-config
  "
Section \"InputClass\"
     Identifier      \"Default Keyboard\"
     MatchIsKeyboard \"on\"
     Option          \"XkbLayout\"  \"us\"
     Option          \"XkbVariant\" \"dvp\"
     Option          \"XkbOptions\" \"ctrl:nocaps\"
EndSection

Section \"InputClass\"
     Identifier      \"RMC RMC 16k Keyboard\"
     MatchIsKeyboard \"on\"
     MatchProduct    \"RMC RMC 16k Keyboard\"
     Option          \"XkbLayout\"  \"us\"
     Option          \"XkbVariant\" \",\"
     Option          \"XkbOptions\" \",\"
EndSection

Section \"InputClass\"
     Identifier      \"ZSA ErgoDox EZ Keyboard\"
     MatchIsKeyboard \"on\"
     MatchProduct    \"ZSA Technology Labs ErgoDox EZ\"
     Option          \"XkbLayout\"  \"us\"
     Option          \"XkbVariant\" \",\"
     Option          \"XkbOptions\" \",\"
EndSection
")

(define (firewalld-configuration-package _config)
  my-firewalld:firewalld)

(define firewalld-service-type
  (service-type
   (name 'firewalld)
   (description
    "Run @command{firewalld}, setting up the specified ruleset.")
   (extensions
    (list (service-extension dbus-services:dbus-root-service-type
                             (compose list firewalld-configuration-package))
          (service-extension dbus-services:polkit-service-type
                             (compose list firewalld-configuration-package))
          (service-extension profile-service-type
                             (compose list firewalld-configuration-package))))
   (default-value #nil)))

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
   (service xorg-services:xorg-server-service-type
            (xorg-services:xorg-configuration
             (server my-xorg:xorg-server)
             (extra-config (list default-xorg-config))))
   (simple-service 'screen-locker-pam
                   pam:pam-root-service-type
                   (list (unix-pam-service "screen-locker"
                                           #:allow-empty-passwords? #f)))

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

   ;; Containerd
   (service docker-services:containerd-service-type)

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

   ;; Network
   (service networking-services:network-manager-service-type
            (networking-services:network-manager-configuration
             (dns "dnsmasq")))
   (service networking-services:wpa-supplicant-service-type)
   (service networking-services:modem-manager-service-type)
   (service networking-services:usb-modeswitch-service-type)
   (simple-service 'network-manager-applet
                   profile-service-type
                   (list gnome:network-manager-applet))
   (simple-service 'sysctl-ipv6
                   sysctl-services:sysctl-service-type
                   `(;; Enable Privacy Extensions and prefer temporary
                     ;; addresses over public addresses
                     ("net.ipv6.conf.all.use_tempaddr"     . "2")
                     ("net.ipv6.conf.default.use_tempaddr" . "2")))

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

   ;; Talon Tobii
   (simple-service 'talon-tobii-udev base-services:udev-service-type (list tobii-access-udev))

   (simple-service 'card-reader-udev base-services:udev-service-type (list card-reader-udev))

   ;; Pipewire
   (simple-service 'pipewire-udev base-services:udev-service-type (list linux:pipewire))

   ;; PAM
   (service base-services:pam-limits-service-type
            (list
             ;; Increase max open files
             (pam:pam-limits-entry "*" 'both 'nofile 65536)))

   ;; Firewalld
   (service firewalld-service-type)))

(define %my-base-desktop-system
  (operating-system
    (inherit my-base-systems:%my-base-system)

    (packages (append %my-desktop-packages
                      (operating-system-packages my-base-systems:%my-base-system)))

    (services (append %my-desktop-services
                      ((compose add-nonguix-substitute)
                       (operating-system-user-services my-base-systems:%my-base-system))))))
