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
           (list (local-file
                  "/home/rsauex/dotfiles/guix/home-files/.bash_profile"
                  "bash_profile")))
          (bashrc
           (list (local-file
                  "/home/rsauex/dotfiles/guix/home-files/.bashrc"
                  "bashrc")))
          (bash-logout
           (list (local-file
                  "/home/rsauex/dotfiles/guix/home-files/.bash_logout"
                  "bash_logout")))))
        (service rofi:rofi-service-type
                 (rofi:rofi-configuration
                  (config
                   '(("font" . "Monospace 12")))
                  (theme rofi-nord-theme)))
        (service xdg:home-xdg-user-directories-service-type)
        (simple-service 'my-environment
                        home-environment-variables-service-type
                        (list
                         ;; Better QT5
                         (cons "QT_QPA_PLATFORMTHEME" "gtk2")
                         (cons "QT_STYLE_OVERRIDE" "gtk2")
                         ;; Local bin
                         (cons "PATH" "\"$HOME/.bin:$HOME/dotfiles/guix/home-files/bin:$HOME/.local/bin:$PATH\"")
                         ;; Temp fix for Evolution to see installed plugins
                         ;; TODO: Replace with a service or a wrapper
                         (cons "EDS_EXTRA_PREFIXES" "/run/current-system/profile/")
                         ;; Fix scaling issues in Alacritty
                         (cons "WINIT_X11_SCALE_FACTOR" "1")))
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
                                 #$(local-file "/home/rsauex/dotfiles/guix/home-files/.Xresources" "Xresources"))
                         ;; Disable screen saver
                         (xset "s" "off")
                         ;; DPMS settings
                         (xset "dpms" "0" "600" "900")
                         ;; Cursor speed
                         (xset "r" "rate" "400" "44")
                         ;; Keyboard settings (default)
                         (setxkbmap "-option"
                                    "-option" "ctrl:nocaps,grp:win_space_toggle,gep_led:scroll"
                                    "-layout" "us,ua"
                                    "-variant" "dvp,")
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
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/.tmux.conf" "tmux.conf"))))
        (service cursor-theme:cursor-theme-service-type
                 (cursor-theme:cursor-theme-configuration
                  (theme-package the-dot:the-dot-cursor-theme)
                  (theme-name "Dot-Light")))
        (simple-service 'dunst
                        home-xdg-configuration-files-service-type
                        `(("dunst/dunstrc"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/dunstrc" "dunstrc"))))
        (simple-service 'font-config
                        home-xdg-configuration-files-service-type
                        `(("fontconfig/fonts.conf"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/fonts.conf" "fonts.conf"))))
        (simple-service 'alacritty
                        home-xdg-configuration-files-service-type
                        `(("alacritty/alacritty.yml"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/alacritty.yml" "alacritty.yml"))))
        (simple-service 'gtk2
                        home-files-service-type
                        `((".gtkrc-2.0"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/.gtkrc-2.0" "gtkrc-2.0"))))
        (simple-service 'gtk3
                        home-xdg-configuration-files-service-type
                        `(("gtk-3.0/settings.ini"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/gtk-3.0-settings.ini" "gtk-3.0-settings.ini"))))
        (simple-service 'x-resources
                        home-files-service-type
                        `((".Xresources"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/.Xresources" "Xresources"))
                          (".x3270pro"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/.x3270pro" "x3270pro"))))
        (simple-service 'terminal-x-resources
                        home-xdg-configuration-files-service-type
                        `(("urxvt"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/urxvt" "urxvt" #:recursive? #t))))
        (simple-service 'git-common
                        home-files-service-type
                        `((".gitcommonconfig"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/.gitcommonconfig" "gitcommonconfig"))))
        (simple-service 'powershell
                        home-xdg-configuration-files-service-type
                        `(("powershell"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/powershell" "powershell-config" #:recursive? #t))))
        (simple-service 'i3blocks
                        home-xdg-configuration-files-service-type
                        `(("i3/i3blocks.conf"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/i3blocks.conf" "i3blocks.conf"))
                          ("i3/i3blocks"
                           ,(local-file "/home/rsauex/dotfiles/guix/home-files/i3blocks" "i3blocks-libexec" #:recursive? #t))))
        (simple-service 'i3
                        home-xdg-configuration-files-service-type
                        `(("i3/config"
                           ,(computed-file
                             "i3-config"
                             (let ((files (list (local-file "/home/rsauex/dotfiles/guix/home-files/i3/00_base.conf" "i3-00_base.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/05_colors.conf" "i3-05_colors.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/10_keys.conf" "i3-10_keys.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/15_keys.wm.conf" "i3-15_keys.wm.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/20_menus.conf" "i3-20_menus.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/30_bar.conf" "i3-30_bar.conf")
                                                (local-file "/home/rsauex/dotfiles/guix/home-files/i3/40_client.conf" "i3-40_client.conf")))
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
