(use-modules
 (gnu home services fontutils)
 (gnu home services shells)
 ((gnu home services xdg) #:prefix xdg:)
 (gnu home services)
 (gnu home)
 (gnu packages xdisorg)
 (gnu packages)
 (gnu services configuration)
 (gnu services)
 (guix gexp)
 (guix git)
 (guix packages)
 (ice-9 match)
 (rsauex home services rofi)
 (srfi srfi-1)
 (srfi srfi-26))

(define (my-essential-services he)
  ((compose
    (cut remove (compose (cut eq? home-fontconfig-service-type <>) service-kind) <>))
   ((@@ (gnu home) home-environment-default-essential-services) he)))

(home-environment
 (packages (map specification->package (list)))
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
        (service
         rofi-service-type
         (rofi-configuration
          (config
           '(("font" . "Monospace 12")))
          (theme
           (file-append (git-checkout
                         (url "https://github.com/amayer5125/nord-rofi/")
                         (commit "71c76dcf31e38b426519198276cf6b5ba626e61c"))
                        "/nord.rasi"))))
        (service xdg:home-xdg-user-directories-service-type)
        (simple-service 'my-environment
                        home-environment-variables-service-type
                        (list
                         ;; Better QT5
                         (cons "QT_QPA_PLATFORMTHEME" "gtk2")
                         (cons "QT_STYLE_OVERRIDE" "gtk2")
                         ;; Local bin
                         (cons "PATH" "\"$HOME/.bin:$HOME/.local/bin:$PATH\"")
                         ;; Temp fix for Evolution to see installed plugins
                         ;; TODO: Replace with a service or a wrapper
                         (cons "EDS_EXTRA_PREFIXES" "/run/current-system/profile/")
                         ;; Fix scaling issues in Alacritty
                         (cons "WINIT_X11_SCALE_FACTOR" "1"))))))
