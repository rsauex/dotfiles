(define-module (rsauex systems desktop)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module ((gnu packages cups)               #:prefix cups:)
  #:use-module ((gnu packages docker)             #:prefix docker:)
  #:use-module ((gnu packages dunst)              #:prefix dunst:)
  #:use-module ((gnu packages fonts)              #:prefix fonts:)
  #:use-module ((gnu packages glib)               #:prefix glib:)
  #:use-module ((gnu packages gnome)              #:prefix gnome:)
  #:use-module ((gnu packages libreoffice)        #:prefix libreoffice:)
  #:use-module ((gnu packages linux)              #:prefix linux:)
  #:use-module ((gnu packages m4)                 #:prefix m4:)
  #:use-module ((gnu packages music)              #:prefix music:)
  #:use-module ((gnu packages networking)         #:prefix networking:)
  #:use-module ((gnu packages package-management) #:prefix package-management:)
  #:use-module ((gnu packages password-utils)     #:prefix passwd-utils:)
  #:use-module ((gnu packages perl)               #:prefix perl:)
  #:use-module ((gnu packages polkit)             #:prefix polkit:)
  #:use-module ((gnu packages pulseaudio)         #:prefix pulseaudio:)
  #:use-module ((gnu packages security-token)     #:prefix security-token:)
  #:use-module ((gnu packages syncthing)          #:prefix syncthing:)
  #:use-module ((gnu packages telegram)           #:prefix telegram:)
  #:use-module ((gnu packages terminals)          #:prefix terms:)
  #:use-module ((gnu packages version-control)    #:prefix vc:)
  #:use-module ((gnu packages wine)               #:prefix wine:)
  #:use-module ((gnu packages wm)                 #:prefix wm:)
  #:use-module ((gnu packages xdisorg)            #:prefix xdisorg:)
  #:use-module ((gnu packages xorg)               #:prefix xorg:)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services security-token)
  #:use-module (gnu services xorg)
  #:use-module (gnu system pam)
  #:use-module ((nongnu packages mozilla)      #:prefix mozilla:)
  #:use-module ((rsauex packages gigolo)       #:prefix gigolo:)
  #:use-module ((rsauex packages the-dot)      #:prefix the-dot:)
  #:use-module ((rsauex packages nm-forti)     #:prefix nm-forti:)
  #:use-module ((rsauex packages nordic-theme) #:prefix nordic:)
  #:use-module (rsauex services pam-u2f)
  #:use-module (rsauex services yubikey-session)
  #:use-module (rsauex systems base)
  #:use-module (rsauex systems minimal)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (%my-base-desktop-system))

(define (my-network-manager-service-type services)
  (modify-services services
    (network-manager-service-type
     config =>
     (network-manager-configuration
      (inherit config)
      (dns "dnsmasq")
      (vpn-plugins
       (cons* nm-forti:network-manager-openfortivpn
              (network-manager-configuration-vpn-plugins config)))))))

(define (my-pam-u2f-auth-service)
  (define (my-pam-u2f-auth-extension pam)
    (if (member (pam-service-name pam) '("login" "su" "sudo" "i3lock"))
        (pam-service
         (inherit pam)
         (auth (cons* (pam-u2f-entry "sufficient")
                      (pam-service-auth pam))))
        pam))

  (simple-service 'pam-u2f pam-root-service-type (list my-pam-u2f-auth-extension)))

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

(define %my-base-desktop-system
  (operating-system
    (inherit %my-base-minimal-system)

    (packages (cons* fonts:font-iosevka
                     fonts:font-google-roboto
                     fonts:font-google-noto
                     fonts:font-adobe-source-sans-pro
                     fonts:font-adobe-source-serif-pro
                     fonts:font-adobe-source-han-sans

                     gnome:gnome-themes-standard
                     gnome:gnome-themes-extra
                     gnome:hicolor-icon-theme
                     gnome:adwaita-icon-theme
                     the-dot:the-dot-cursor-theme
                     nordic:nordic-darker-theme

                     xorg:xrdb
                     xorg:xset
                     xorg:xinput
                     xorg:setxkbmap

                     m4:m4
                     wm:i3-wm
                     wm:i3status
                     wm:i3blocks

                     ;; For battery indicator in i3blocks
                     linux:acpi
                     perl:perl

                     gnome:dconf
                     gnome:dconf-editor
                     gnome:gsettings-desktop-schemas
                     gnome:gvfs
                     gnome:system-config-printer
                     polkit:polkit-gnome
                     networking:blueman

                     (list glib:glib "bin")

                     dunst:dunst
                     xdisorg:sx
                     xdisorg:rofi
                     xdisorg:xss-lock
                     xdisorg:maim
                     xdisorg:xclip
                     xdisorg:xdotool
                     xdisorg:arandr
                     music:playerctl

                     terms:alacritty
                     mozilla:firefox
                     gnome:evince
                     gnome:evolution
                     gnome:evolution-data-server
                     passwd-utils:keepassxc
                     syncthing:syncthing-gtk
                     pulseaudio:pavucontrol
                     wine:wine
                     gigolo:gigolo
                     libreoffice:libreoffice
                     (list vc:git "gui")
                     package-management:flatpak
                     telegram:telegram-desktop

                     security-token:yubikey-personalization
                     security-token:python-yubikey-manager

                     docker:docker-compose

                     (operating-system-packages %my-base-minimal-system)))

    (services (append
               (list (service cups-service-type
                              (cups-configuration
                               (extensions
                                (list cups:cups-filters))
                               (default-paper-size "A4")))
                     (simple-service 'gvfs-polkit
                                     polkit-service-type
                                     (list gnome:gvfs))
                     (udev-rules-service 'yubikey security-token:yubikey-personalization)
                     (my-pam-u2f-auth-service)
                     (screen-locker-service wm:i3lock "i3lock")
                     (service xorg-server-service-type)
                     (bluetooth-service #:auto-enable? #t)
                     (simple-service 'blueman
                                     dbus-root-service-type
                                     (list networking:blueman))
                     (simple-service 'evolution
                                     dbus-root-service-type
                                     (list gnome:evolution-data-server))
                     (service docker-service-type))

               %my-base-services

               ((compose
                 my-console-font-service-type
                 my-network-manager-service-type

                 add-nonguix-substitute

                 (cut remove (compose (cut eq? gdm-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? screen-locker-service-type <>) service-kind) <>)
                 (cut remove (compose (cut eq? ntp-service-type <>) service-kind) <>))

                %desktop-services)))))
