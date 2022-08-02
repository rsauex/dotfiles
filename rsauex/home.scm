(use-modules
 (gnu home services fontutils)
 (gnu home services shells)
 ((gnu home services xdg) #:prefix xdg:)
 (gnu home services)
 (gnu home)
 (gnu packages xdisorg)
 ((gnu packages glib)               #:prefix glib:)
 ((gnu packages gnome)              #:prefix gnome:)
 ((gnu packages networking)         #:prefix networking:)
 ((gnu packages polkit)             #:prefix polkit:)
 ((gnu packages wm)                 #:prefix wm:)
 ((gnu packages xdisorg)            #:prefix xdisorg:)
 ((gnu packages xorg)               #:prefix xorg:)
 ((gnu packages m4)                 #:prefix m4:)
 (gnu packages)
 (gnu services configuration)
 (gnu services)
 (guix gexp)
 (guix git)
 (guix packages)
 (ice-9 match)
 (ice-9 textual-ports)
 (rsauex packages)
 ((rsauex packages the-dot)             #:prefix the-dot:)
 ((rsauex home services rofi)           #:prefix rofi:)
 ((rsauex home services sx)             #:prefix sx:)
 ((rsauex home services cursor-theme)   #:prefix cursor-theme:)
 (srfi srfi-1)
 (srfi srfi-26))

(define (my-essential-services he)
  ((compose
    (cut remove (compose (cut eq? home-fontconfig-service-type <>) service-kind) <>))
   ((@@ (gnu home) home-environment-default-essential-services) he)))

(define rofi-nord-theme
  (file-append (git-checkout
                (url "https://github.com/amayer5125/nord-rofi/")
                (commit "71c76dcf31e38b426519198276cf6b5ba626e61c"))
               "/nord.rasi"))

(home-environment
 (packages (list
            ;; the-dot:the-dot-cursor-theme
            ))
 (essential-services
  (my-essential-services this-home-environment))
 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (bash-profile
           (list (rsauex-home-file ".bash_profile" "bash_profile")))
          (bashrc
           (list (rsauex-home-file ".bashrc" "bashrc")))
          (bash-logout
           (list (rsauex-home-file ".bash_logout" "bash_logout")))))
        (service rofi:rofi-service-type
                 (rofi:rofi-configuration
                  (config
                   '(("font" . "Monospace 12")))
                  (theme rofi-nord-theme)))
        (service xdg:home-xdg-user-directories-service-type)
        (simple-service 'my-environment
                        home-environment-variables-service-type
                        (list
                         ;; Better QT
                         (cons "QT_QPA_PLATFORMTHEME" "kvantum")
                         (cons "QT_STYLE_OVERRIDE" "kvantum")
                         ;; Local bin
                         (cons "PATH" "\"$HOME/.bin:$HOME/dotfiles/home-files/bin:$HOME/.local/bin:$PATH\"")
                         ;; Temp fix for Evolution to see installed plugins
                         ;; TODO: Replace with a service or a wrapper
                         (cons "EDS_EXTRA_PREFIXES" "/run/current-system/profile/")
                         ;; Fix scaling issues in Alacritty
                         (cons "WINIT_X11_SCALE_FACTOR" "1")
                         ;; Input method
                         (cons "GTK_IM_MODULE" "fcitx")
                         (cons "QT_IM_MODULE" "fcitx")
                         (cons "XMODIFIERS" "@im=fcitx")
                         ;; (cons "GTK_IM_MODULE" "ibus")
                         ;; (cons "QT_IM_MODULE" "ibus")
                         ;; (cons "XMODIFIERS" "@im=ibus")
                         ;; Respect immodule cache (TODO: This shouldn't be necessary...)
                         (cons "GUIX_GTK2_IM_MODULE_FILE" "/run/current-system/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache")
                         (cons "GUIX_GTK3_IM_MODULE_FILE" "/run/current-system/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache")
                         ;; Respect QT Plugins (TODO: This shouldn't be necessary...)
                         (cons "QT_PLUGIN_PATH" "/run/current-system/profile/lib/qt5/plugins")))
        (service sx:sx-service-type
                 (sx:sx-configuration
                  (sxrc
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils)
                                      (ice-9 popen)
                                      (ice-9 textual-ports))

                         (define (xset . args)
                           (apply invoke #$(file-append xorg:xset "/bin/xset") args))

                         (define (setxkbmap . args)
                           (apply invoke #$(file-append xorg:setxkbmap "/bin/setxkbmap") args))

                         (define (invoke/capture-stdout program . args)
                           (get-string-all (apply open-pipe* OPEN_READ program args)))

                         (define (spawn program . args)
                           (close-port (apply open-pipe* OPEN_WRITE program args)))

                         ;; Load Xresources
                         (invoke #$(file-append xorg:xrdb "/bin/xrdb")
                                 "-merge" (string-append "-I" (getenv "HOME"))
                                 #$(rsauex-home-file ".Xresources" "Xresources"))
                         ;; Disable screen saver
                         (xset "s" "off")
                         ;; DPMS settings
                         (xset "dpms" "0" "600" "900")
                         ;; Cursor speed
                         (xset "r" "rate" "400" "44")
                         ;; Keyboard settings (default)
                         (setxkbmap "-option"
                                    "-option" "ctrl:nocaps"
                                    "-layout" "us"
                                    "-variant" "dvp")
                         ;; Keyboard settings (Ergodox EZ)
                         (let ((device (invoke/capture-stdout #$(file-append xorg:xinput "/bin/xinput")
                                                              "list"
                                                              "--id-only" "keyboard:ZSA Technology Labs Inc ErgoDox EZ")))
                           (unless (string-null? device)
                             (setxkbmap "-device" device
                                        "-option"
                                        "-layout" "us")))
                         ;; TODO: use shepherd for these
                         ;; Screen locking (TODO: locking script)
                         (spawn #$(file-append xdisorg:xss-lock "/bin/xss-lock")
                                "-l" "--" "lock")
                         ;; Blueman applet
                         (spawn #$(file-append networking:blueman "/bin/blueman-applet"))
                         ;; Polkit authenticator
                         (spawn #$(file-append polkit:polkit-gnome "/libexec/polkit-gnome-authentication-agent-1"))
                         ;; Network Manager applet
                         (spawn #$(file-append gnome:network-manager-applet "/bin/nm-applet"))
                         ;; Start I3
                         (execl #$(file-append glib:dbus "/bin/dbus-launch")
                                "--exit-with-session"
                                #$(file-append wm:i3-wm "/bin/i3")))))))
        (simple-service 'tmux
                        home-files-service-type
                        `((".tmux.conf"
                           ,(rsauex-home-file ".tmux.conf" "tmux.conf"))))
        (service cursor-theme:cursor-theme-service-type
                 (cursor-theme:cursor-theme-configuration
                  (theme-package the-dot:the-dot-cursor-theme)
                  (theme-name "Dot-Light")))
        (simple-service 'kvantum-theme
                        home-xdg-configuration-files-service-type
                        `(("Kvantum/kvantum.kvconfig"
                           ,(plain-file
                             "kvantum.kvconfig"
                             "theme=Nordic-Darker\n"))))
        (simple-service 'dunst
                        home-xdg-configuration-files-service-type
                        `(("dunst/dunstrc"
                           ,(rsauex-home-file "dunstrc" "dunstrc"))))
        (simple-service 'font-config
                        home-xdg-configuration-files-service-type
                        `(("fontconfig/fonts.conf"
                           ,(rsauex-home-file "fonts.conf" "fonts.conf"))))
        (simple-service 'alacritty
                        home-xdg-configuration-files-service-type
                        `(("alacritty/alacritty.yml"
                           ,(rsauex-home-file "alacritty.yml" "alacritty.yml"))))
        (simple-service 'gtk2
                        home-files-service-type
                        `((".gtkrc-2.0"
                           ,(rsauex-home-file ".gtkrc-2.0" "gtkrc-2.0"))))
        (simple-service 'gtk3
                        home-xdg-configuration-files-service-type
                        `(("gtk-3.0/settings.ini"
                           ,(rsauex-home-file "gtk-3.0-settings.ini" "gtk-3.0-settings.ini"))))
        (simple-service 'x-resources
                        home-files-service-type
                        `((".Xresources"
                           ,(rsauex-home-file ".Xresources" "Xresources"))
                          (".x3270pro"
                           ,(rsauex-home-file ".x3270pro" "x3270pro"))))
        (simple-service 'terminal-x-resources
                        home-xdg-configuration-files-service-type
                        `(("urxvt"
                           ,(rsauex-home-file "urxvt" "urxvt" #:recursive? #t))))
        (simple-service 'git-conf
                        home-files-service-type
                        `((".gitconfig"
                           ,(rsauex-home-file ".gitconfig" "gitconfig"))))
        (simple-service 'powershell
                        home-xdg-configuration-files-service-type
                        `(("powershell"
                           ,(rsauex-home-file "powershell" "powershell-config" #:recursive? #t))))
        (simple-service 'ukrainian-layout
                        home-files-service-type
                        `((".m17n.d/uk-translit.mim"
                           ,(rsauex-home-file "uk-translit.mim" "uk-translit.mim"))))
        (simple-service 'i3blocks
                        home-xdg-configuration-files-service-type
                        `(("i3/i3blocks.conf"
                           ,(rsauex-home-file "i3blocks.conf" "i3blocks.conf"))
                          ("i3/i3blocks"
                           ,(rsauex-home-file "i3blocks" "i3blocks-libexec" #:recursive? #t))))
        (simple-service 'i3
                        home-xdg-configuration-files-service-type
                        `(("i3/config"
                           ,(computed-file
                             "i3-config"
                             (let ((files (list (rsauex-home-file "i3/00_base.conf" "i3-00_base.conf")
                                                (rsauex-home-file "i3/05_colors.conf" "i3-05_colors.conf")
                                                (rsauex-home-file "i3/10_keys.conf" "i3-10_keys.conf")
                                                (rsauex-home-file "i3/15_keys.wm.conf" "i3-15_keys.wm.conf")
                                                (rsauex-home-file "i3/20_menus.conf" "i3-20_menus.conf")
                                                (rsauex-home-file "i3/30_bar.conf" "i3-30_bar.conf")
                                                (rsauex-home-file "i3/40_client.conf" "i3-40_client.conf")))
                                   (m4 (file-append m4:m4 "/bin/m4"))
                                   (cassis-type (call-with-input-file "/sys/class/dmi/id/chassis_type"
                                                  (lambda (port)
                                                    (get-line port)))))
                               #~(begin
                                   (use-modules (ice-9 popen)
                                                (ice-9 textual-ports))
                                   (with-fluids ((%default-port-encoding "UTF-8"))
                                     (call-with-output-file #$output
                                       (lambda (port)
                                         (put-string port (get-string-all (open-pipe* OPEN_READ #$m4 (string-append "-DPC_TYPE=" #$cassis-type) #$@files)))))))))))))))
