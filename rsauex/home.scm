(define-module (rsauex home)
  #:use-module ((gnu home services fontutils) #:prefix fontutils:)
  #:use-module ((gnu home services shells)    #:prefix shells:)
  #:use-module ((gnu home services xdg)       #:prefix xdg:)
  #:use-module ((gnu home services))
  #:use-module ((gnu home))
  #:use-module ((gnu packages admin)              #:prefix admin:)
  #:use-module ((gnu packages base)               #:prefix base-packages:)
  #:use-module ((gnu packages compression)        #:prefix compression:)
  #:use-module ((gnu packages docker)             #:prefix docker:)
  #:use-module ((gnu packages emacs)              #:prefix emacs:)
  #:use-module ((gnu packages fonts)              #:prefix fonts:)
  #:use-module ((gnu packages freedesktop)        #:prefix freedesktop:)
  #:use-module ((gnu packages gimp)               #:prefix gimp:)
  #:use-module ((gnu packages gnome)              #:prefix gnome:)
  #:use-module ((gnu packages gnome-xyz)          #:prefix gnome-xyz:)
  #:use-module ((gnu packages gtk)                #:prefix gtk:)
  #:use-module ((gnu packages image-viewers)      #:prefix image-viewers:)
  #:use-module ((gnu packages kde-frameworks)     #:prefix kde-frameworks:)
  #:use-module ((gnu packages libreoffice)        #:prefix libreoffice:)
  #:use-module ((gnu packages linux)              #:prefix linux:)
  #:use-module ((gnu packages music)              #:prefix music:)
  #:use-module ((gnu packages networking)         #:prefix networking:)
  #:use-module ((gnu packages password-utils)     #:prefix passwd-utils:)
  #:use-module ((gnu packages pdf)                #:prefix pdf:)
  #:use-module ((gnu packages perl)               #:prefix perl:)
  #:use-module ((gnu packages polkit)             #:prefix polkit:)
  #:use-module ((gnu packages pulseaudio)         #:prefix pulseaudio:)
  #:use-module ((gnu packages qt)                 #:prefix qt:)
  #:use-module ((gnu packages rsync)              #:prefix rsync:)
  #:use-module ((gnu packages scanner)            #:prefix scanner:)
  #:use-module ((gnu packages security-token)     #:prefix security-token:)
  #:use-module ((gnu packages ssh)                #:prefix ssh:)
  #:use-module ((gnu packages syncthing)          #:prefix syncthing:)
  #:use-module ((gnu packages terminals)          #:prefix terms:)
  #:use-module ((gnu packages text-editors)       #:prefix text-editors:)
  #:use-module ((gnu packages version-control)    #:prefix vc:)
  #:use-module ((gnu packages video)              #:prefix video:)
  #:use-module ((gnu packages web)                #:prefix web:)
  #:use-module ((gnu packages wine)               #:prefix wine:)
  #:use-module ((gnu packages wm)                 #:prefix wm:)
  #:use-module ((gnu packages xdisorg)            #:prefix xdisorg:)
  #:use-module ((gnu packages xfce)               #:prefix xfce:)
  #:use-module ((gnu packages xorg)               #:prefix xorg:)
  #:use-module ((gnu services))
  #:use-module ((guix channels)                   #:prefix channels:)
  #:use-module ((guix gexp))
  #:use-module ((guix git))
  #:use-module ((guix modules))
  #:use-module ((guix packages))
  #:use-module ((ice-9 match))
  #:use-module ((ice-9 textual-ports))
  #:use-module ((nongnu packages mozilla)             #:prefix mozilla:)
  #:use-module ((rsauex channels)                     #:prefix my-channels:)
  #:use-module ((rsauex home config)                  #:prefix my-config:)
  #:use-module ((rsauex home services channels)       #:prefix my-channels-service:)
  #:use-module ((rsauex home services cursor-theme)   #:prefix my-cursor-theme:)
  #:use-module ((rsauex home services dunst)          #:prefix my-dunst-service:)
  #:use-module ((rsauex home services fcitx5)         #:prefix my-fcitx5-service:)
  #:use-module ((rsauex home services git)            #:prefix my-git:)
  #:use-module ((rsauex home services gui-startup)    #:prefix my-gui-startup:)
  #:use-module ((rsauex home services picom)          #:prefix my-picom-service:)
  #:use-module ((rsauex home services rofi)           #:prefix my-rofi:)
  #:use-module ((rsauex home services shepherd)       #:prefix my-shepherd:)
  #:use-module ((rsauex home services ssh)            #:prefix my-ssh-service:)
  #:use-module ((rsauex home services xdg-portal)     #:prefix my-xdg-portal-service:)
  #:use-module ((rsauex home services pipewire)       #:prefix my-pipewire-service:)
  #:use-module ((rsauex home services screensaver)    #:prefix my-screensaver-service:)
  #:use-module ((rsauex home services xsettingsd)     #:prefix my-xsettingsd-service:)
  #:use-module ((rsauex home services xresources)     #:prefix my-xresources-service:)
  #:use-module ((rsauex packages fcitx5)              #:prefix my-fcitx5:)
  #:use-module ((rsauex packages kvantum)             #:prefix kvantum:)
  #:use-module ((rsauex packages nordic-theme)        #:prefix nordic-theme:)
  #:use-module ((rsauex packages powershell)          #:prefix powershell:)
  #:use-module ((rsauex packages the-dot)             #:prefix the-dot:)
  #:use-module ((rsauex packages gns3)                #:prefix gns3:)
  #:use-module ((rsauex packages))
  #:use-module ((rsauex script template))
  #:use-module ((rsauex services))
  #:use-module ((srfi srfi-1))
  #:use-module ((srfi srfi-26)))

(define (rsauex-module-name? name)
  (match name
    ((('rsauex _ ...)) #t)
    (('rsauex _ ...) #t)
    (_ #f)))

(define (program-fn-file module fn)
  (program-file
   (string-join (map symbol->string (append module (list fn))) "-")
   (with-imported-modules
       (append (list module)
               (live-module-closure
                (list module)
                #:select? rsauex-module-name?))
     #~(apply (@ #$module #$fn) (cdr (program-arguments))))))

(define (git-prevent-push-to-important-branches)
  (cons #:pre-push (program-fn-file '(rsauex home services git hooks ask-on-push-to-master) 'check)))

(define (git-dont-push-wip-commits)
  (cons #:pre-push (program-fn-file '(rsauex home services git hooks dont-push-wip-commits) 'check)))

(define (my-essential-services he)
  (modify-services ((@@ (gnu home) home-environment-default-essential-services) he)
    (delete fontutils:home-fontconfig-service-type)))

(define (i3lock-with-login-pam-service)
  (package
    (inherit wm:i3lock)
    (version (string-append (package-version wm:i3lock)
                            "-with-login-pam-service"))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'change-pam-service
                    (lambda _
                      (substitute* '("i3lock.c")
                        (("pam_start\\(\"i3lock\"") "pam_start(\"screen-locker\"")))))))
    (synopsis (string-append (package-synopsis wm:i3lock)
                             " (with 'login' pam service)"))))

(define (i3-config-service)
  (anon-service i3blocks-config
    (home-profile-service-type
     (list wm:i3-wm
           wm:i3status
           wm:i3blocks
           ;; For scripts in i3blocks (TODO: Remove)
           linux:acpi
           linux:sysstat
           perl:perl
           xorg:xset
           xdisorg:xclip
           music:playerctl
           ;; Called from i3 scripts (TODO: Remove)
           xdisorg:xdotool
           web:jq))
    (home-xdg-configuration-files-service-type
     `(("i3/i3blocks.conf"
        ,(rsauex-home-file "i3blocks.conf" "i3blocks.conf"))
       ("i3/i3blocks"
        ,(rsauex-home-file "i3blocks" "i3blocks-libexec" #:recursive? #t))
       ("i3/config"
        ,(computed-file
          "i3-config"
          (let ((files (list (rsauex-home-template-file "i3/00_base.conf" "i3-00_base.conf")
                             (rsauex-home-template-file "i3/05_colors.conf" "i3-05_colors.conf")
                             (rsauex-home-template-file "i3/10_keys.conf" "i3-10_keys.conf")
                             (rsauex-home-template-file "i3/15_keys.wm.conf" "i3-15_keys.wm.conf")
                             (rsauex-home-template-file "i3/20_menus.conf" "i3-20_menus.conf")
                             (rsauex-home-template-file "i3/30_bar.conf" "i3-30_bar.conf")
                             (rsauex-home-template-file "i3/40_client.conf" "i3-40_client.conf")))
                (cat (file-append base-packages:coreutils "/bin/cat")))
            #~(begin
                (use-modules (ice-9 popen)
                             (ice-9 textual-ports))
                (with-fluids ((%default-port-encoding "UTF-8"))
                  (call-with-output-file #$output
                    (lambda (port)
                      (put-string port (get-string-all (open-pipe* OPEN_READ #$cat #$@files))))))))))))))

(define (syncthing-service)
  (anon-service syncthing-applet-autostart
    (my-gui-startup:gui-startup-service-type
     (my-gui-startup:gui-startup-extension
      (services
       (list (my-shepherd:simple-forkexec-shepherd-service
              'syncthing-applet
              "Run `syncthing-gtk'"
              #~`(#$(file-append syncthing:syncthing-gtk "/bin/syncthing-gtk"))
              #:data-packages (list syncthing:syncthing-gtk))))))))

(define (host-dpi)
  (let ((host-dpi (getenv "HOST_DPI")))
    (if host-dpi (string->number host-dpi) 96)))

(define xft-config
  (my-config:xft-config
   (hint-style "hintmedium")
   (dpi (host-dpi))))

(define-public %home-environment
  (home-environment
   (packages (list fonts:font-iosevka-term
                   fonts:font-google-roboto
                   fonts:font-google-noto
                   fonts:font-adobe-source-sans-pro
                   fonts:font-adobe-source-serif-pro
                   fonts:font-adobe-source-han-sans
                   fonts:font-awesome
                   fonts:font-google-material-design-icons
                   fonts:font-terminus

                   gnome:gnome-themes-standard
                   gnome:gnome-themes-extra
                   gnome:hicolor-icon-theme
                   gnome:adwaita-icon-theme
                   kde-frameworks:breeze-icons
                   nordic-theme:nordic-darker-theme

                   ;; TODO: make separate service
                   kvantum:kvantum
                   qt:qt5ct

                   ;; TODO: should be specified only in syncthing-gtk's config
                   syncthing:syncthing

                   ;; TODO: needed for my emacs package
                   rsync:rsync

                   compression:zip
                   compression:unzip
                   compression:p7zip
                   security-token:yubikey-personalization
                   security-token:python-yubikey-manager
                   ssh:openssh
                   admin:htop
                   powershell:powershell
                   text-editors:texmacs
                   mozilla:firefox
                   terms:alacritty
                   xdisorg:arandr
                   libreoffice:libreoffice
                   scanner:xsane
                   passwd-utils:keepassxc
                   pulseaudio:pavucontrol
                   xfce:gigolo
                   pdf:qpdfview
                   gnome:system-config-printer
                   docker:docker-compose
                   wine:wine
                   video:vlc
                   emacs:emacs
                   gimp:gimp
                   image-viewers:viewnior
                   vc:git
                   (list vc:git "gui")
                   gns3:gns3-gui))
   (essential-services
    (my-essential-services this-home-environment))
   (services
    (list (service
           shells:home-bash-service-type
           (shells:home-bash-configuration
            (bash-profile
             (list (rsauex-home-file ".bash_profile" "bash_profile")))
            (bashrc
             (list (rsauex-home-file ".bashrc" "bashrc")))
            (bash-logout
             (list (rsauex-home-file ".bash_logout" "bash_logout")))))
          (service my-channels-service:channels-service-type
                   (cons* my-channels:nonguix-channel
                          channels:%default-channels))
          (service my-rofi:rofi-service-type
                   (my-rofi:rofi-configuration
                    (config
                     `(("font" . "Monospace 12")
                       ("dpi"  . ,(host-dpi))))
                    (theme
                     (file-append nordic-theme:rofi-nord-theme
                                  "/share/rofi/themes/nord.rasi"))))
          (service xdg:home-xdg-user-directories-service-type)
          (simple-service 'my-environment
                          home-environment-variables-service-type
                          (let ((qt-platform-plugin-path (string-append (getenv "HOME") "/.guix-home/profile/lib/qt5/plugins"))
                                (qt-plugin-paths #~(list #$(string-append (getenv "HOME") "/.guix-home/profile/lib/qt5/plugins")
                                                         "/run/current-system/profile/lib/qt5/plugins"
                                                         #$(file-append qt:qtsvg-5 "/lib/qt5/plugins")))
                                (gtk-engine-paths #~(list #$(file-append gtk:murrine "/lib/gtk-2.0")
                                                          #$(file-append gnome:gnome-themes-extra "/lib/gtk-2.0"))))
                            (list
                             ;; Fix scaling issues in Alacritty
                             (cons "WINIT_X11_SCALE_FACTOR" (number->string (/ (host-dpi) 96.0)))
                             ;; Better DE compatibility
                             (cons "XDG_CURRENT_DESKTOP" "XFCE")
                             ;; Better QT
                             (cons "QT_QPA_PLATFORMTHEME" "qt5ct")
                             ;; Local bin
                             (cons "PATH" "$HOME/.bin:$HOME/dotfiles/home-files/bin:$HOME/.local/bin:$PATH")
                             ;; Respect immodule cache (TODO: This shouldn't be necessary...)
                             (cons "GUIX_GTK2_IM_MODULE_FILE" (string-append (getenv "HOME") "/.guix-home/profile/lib/gtk-2.0/2.10.0/immodules-gtk2.cache"))
                             (cons "GUIX_GTK3_IM_MODULE_FILE" (string-append (getenv "HOME") "/.guix-home/profile/lib/gtk-3.0/3.0.0/immodules-gtk3.cache"))
                             ;; Respect QT Plugins (TODO: This shouldn't be necessary...)
                             (cons "QT_QPA_PLATFORM_PLUGIN_PATH" qt-platform-plugin-path)
                             (cons "QT_PLUGIN_PATH" #~(string-join #$qt-plugin-paths ":"))
                             ;; GTK2 engines (TODO: this should be search-paths from nordic-theme)
                             (cons "GUIX_GTK2_PATH" #~(string-join #$gtk-engine-paths ":"))
                             ;; Enable video hardware acceleration (TODO: Send a patch to mozilla?)
                             (cons "MOZ_DISABLE_RDD_SANDBOX" "1")
                             ;; Make packages from dotfiles available everywhere
                             (cons "GUIX_PACKAGE_PATH" (string-append (getenv "HOME") "/dotfiles")))))
          (service my-xresources-service:xresources-service-type
                   (my-xresources-service:xresources-configuration
                    (xresources
                     `(,(rsauex-home-file "urxvt/colors/nord" "colors")
                       ("Xft.dpi" . ,(host-dpi))
                       ("*dpi"    . ,(host-dpi))))))
          ;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (service my-gui-startup:gui-startup-service-type
                   (my-gui-startup:gui-startup-configuration
                    (program (program-file
                              "my-gui-startup"
                              #~(begin
                                  ;; QT applications rely on StatusNotifierWatcher (appindicator) to send
                                  ;; 'Balloon' notifications but when it's not available (and in case of i3bar
                                  ;; it's not) they display ugly custom notifications that don't match
                                  ;; anything else in the system...
                                  ;; (See showMessage_sys in src/widgets/util/qsystemtrayicon_x11.cpp in qtbase)
                                  ;; (Affects KeePassXC)
                                  ;; Other possible solution is to use a different panel...
                                  (system* #$(file-append freedesktop:snixembed "/bin/snixembed")
                                           "--fork")
                                  (execl #$(file-append wm:i3-wm "/bin/i3")
                                         #$(file-append wm:i3-wm "/bin/i3")))))))
          (i3-config-service)
          (service my-dunst-service:dunst-service-type
                   (my-dunst-service:dunst-configuration
                    (config (rsauex-home-file "dunstrc" "dunstrc"))))
          (syncthing-service)
          ;; (service my-picom-service:picom-service-type
          ;;          (my-picom-service:picom-configuration
          ;;           (config (rsauex-home-file "picom.conf" "picom.conf"))))
          (service my-pipewire-service:pipewire-service-type)
          (service my-xdg-portal-service:xdg-desktop-portal-service-type
                   (my-xdg-portal-service:xdg-desktop-portal-configuration
                    (backends (list my-xdg-portal-service:xdg-desktop-portal-gtk-backend))))
          (service my-fcitx5-service:fcitx5-service-type
                   (my-fcitx5-service:fcitx5-configuration
                    (addons (list my-fcitx5:fcitx5-m17n))))
          (service my-screensaver-service:xss-lock-service-type
                   (my-screensaver-service:xss-lock-configuration
                    (screen-off-timeout 600)
                    (locker-expr #~(let ((i3lock #$(file-append (i3lock-with-login-pam-service) "/bin/i3lock")))
                                     `(,i3lock "--nofork" "-n" "-c" "1D1F21")))))
          (service my-xsettingsd-service:xsettingsd-service-type
                   (my-xsettingsd-service:xsettingsd-configuration
                    (xsettings
                     `(("Net/ThemeName"        . "Nordic-Darker")
                       ("Net/IconThemeName"    . "breeze-dark")
                       ("Gtk/FontName"         . "System-UI 11")
                       ("Gtk/DecorationLayout" . "menu:")
                       ("Xft/DPI"              . ,(* (host-dpi) 1024))
                       ;; TODO: This is only needed for java...
                       ("Xft/Antialias"        . 1)))))
          ;; Autostart ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (service my-ssh-service:ssh-agent-service-type)
          (anon-service update-xlfd-fonts
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-one-shot-shepherd-service
                      'update-xlfd-fonts
                      "Update XLFD fonts list"
                      #~(lambda ()
                          (let ((font-paths '("built-ins")))
                            (for-each
                             (lambda (path)
                               (ftw path
                                    (lambda (file info flag)
                                      (when (string= "fonts.dir" (basename file))
                                        (set! font-paths (cons (dirname file) font-paths)))
                                      #t)))
                             (list (string-append (getenv "HOME") "/.guix-home/profile/share/fonts/")
                                   #$(file-append xorg:font-alias "/share/fonts/")
                                   #$(file-append xorg:font-micro-misc "/share/fonts/")
                                   #$(file-append xorg:font-adobe75dpi "/share/fonts/")))
                            (invoke #$(file-append xorg:xset "/bin/xset")
                                    "fp="
                                    (string-join font-paths ","))
                            (invoke #$(file-append xorg:xset "/bin/xset")
                                    "fp" "rehash"))
                          #t)
                      #:extra-modules '((ice-9 ftw))))))))
          (anon-service xset-settings
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-one-shot-shepherd-service
                      'xset-settings
                      "Set `xset' settings"
                      #~(lambda ()
                          (let ((xset (lambda args
                                        (apply invoke #$(file-append xorg:xset "/bin/xset") args))))
                            ;; Cursor speed
                            (xset "r" "rate" "400" "44"))
                          #t)))))))
          (anon-service network-manager-applet-autostart
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-forkexec-shepherd-service
                      'network-manager-applet
                      "Run `nm-applet'"
                      #~`(#$(file-append gnome:network-manager-applet "/bin/nm-applet"))))))))
          (anon-service blueman-applet-autostart
            (home-profile-service-type
             (list networking:blueman))
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-forkexec-shepherd-service
                      'blueman-applet
                      "Run `blueman-applet'"
                      #~`(#$(file-append networking:blueman "/bin/blueman-applet"))))))))
          (anon-service polkit-authentication-agent-autostart
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-forkexec-shepherd-service
                      'polkit-authentication-agent
                      "Run polkit-authenticator agent"
                      #~`(#$(file-append polkit:polkit-gnome "/libexec/polkit-gnome-authentication-agent-1"))))))))
          (anon-service keepassxc-autostart
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-forkexec-shepherd-service
                      'keepassxc
                      "Run `keepassxc'"
                      #~`(#$(file-append passwd-utils:keepassxc "/bin/keepassxc"))))))))
          (anon-service jgmenu-autostart
            (my-gui-startup:gui-startup-service-type
             (my-gui-startup:gui-startup-extension
              (services
               (list (my-shepherd:simple-forkexec-shepherd-service
                      'jgmenu
                      "Run `jgmenu'"
                      #~`(#$(file-append xdisorg:jgmenu "/bin/jgmenu")
                          "--hide-on-startup"))))))
            (home-xdg-configuration-files-service-type
             `(("jgmenu"
                ,(rsauex-home-file "jgmenu" "jgmenu-config" #:recursive? #t)))))
          ;; Git settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (service my-git:git-service-type
                   (my-git:git-configuration
                    (hooks (list (git-dont-push-wip-commits)
                                 (git-prevent-push-to-important-branches)))))
          ;; Other settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (simple-service 'tmux
                          home-files-service-type
                          `((".tmux.conf"
                             ,(rsauex-home-file ".tmux.conf" "tmux.conf"))))
          (service my-cursor-theme:cursor-theme-service-type
                   (my-cursor-theme:cursor-theme-configuration
                    (theme-package the-dot:the-dot-cursor-theme)
                    (theme-name "Dot-Light")))
          (simple-service 'kvantum-theme
                          home-xdg-configuration-files-service-type
                          `(("Kvantum/kvantum.kvconfig"
                             ,(plain-file
                               "kvantum.kvconfig"
                               "theme=Nordic-Darker\n"))))
          (simple-service 'qt5ct-conf
                          home-xdg-configuration-files-service-type
                          `(("qt5ct/qt5ct.conf"
                             ,(rsauex-home-file "qt5ct.conf" "qt5ct.conf"))))
          (simple-service 'font-config
                          home-xdg-configuration-files-service-type
                          `(("fontconfig/fonts.conf"
                             ,(let ((args `((#:xft . ,xft-config))))
                                (rsauex-home-template-file "fonts.conf" "fonts.conf" args)))))
          (simple-service 'alacritty
                          home-xdg-configuration-files-service-type
                          `(("alacritty/alacritty.yml"
                             ,(rsauex-home-file "alacritty.yml" "alacritty.yml"))))
          (simple-service 'gtk2
                          home-files-service-type
                          `((".gtkrc-2.0"
                             ,(let ((args `((#:xft . ,xft-config))))
                                (rsauex-home-template-file ".gtkrc-2.0" "gtkrc-2.0" args)))))
          (simple-service 'gtk3
                          home-xdg-configuration-files-service-type
                          `(("gtk-3.0/settings.ini"
                             ,(let ((args `((#:xft . ,xft-config))))
                                (rsauex-home-template-file "gtk-3.0-settings.ini" "gtk-3.0-settings.ini" args)))
                            ("gtk-3.0/gtk.css"
                             ,(rsauex-home-file "gtk-3.0.css" "gtk-3.0.css"))))
          (anon-service x3270
            (home-files-service-type
             `((".x3270pro"
                ,(rsauex-home-file ".x3270pro" "x3270pro"))))
            (my-xresources-service:xresources-service-type
             (my-xresources-service:xresources-extension
              (xresources
               `(,(rsauex-home-file "x3270-xresources" "x3270-xresources"))))))
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
          (simple-service 'ssh-config
                          home-files-service-type
                          `((".ssh/config"
                             ,(rsauex-home-file "ssh-config" "ssh-config"))))))))

%home-environment
