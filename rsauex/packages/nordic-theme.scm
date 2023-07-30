(define-module (rsauex packages nordic-theme)
  #:use-module ((gnu packages inkscape)   #:prefix inkscape:)
  #:use-module ((gnu packages python)     #:prefix python:)
  #:use-module ((gnu packages web)        #:prefix web:)
  #:use-module ((guix build-system copy)  #:prefix copy-build-system:)
  #:use-module ((guix build-system meson) #:prefix meson-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)       #:prefix git-download:)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)))

(define-public nordic-darker-theme
  (package
    (name "nordic-theme")
    (version "2.2.0-c52c0f9")
    (source (origin
              (method git-download:git-fetch)
              (uri (git-download:git-reference
                    (url "https://github.com/rsauex/Nordic")
                    (commit "c52c0f9a8c19803e4d218eb2464c9d18109cba18")))
              (sha256
               (base32
                "0vf83d5ns0wbs853bzaswh86i0bhgmbnk28bjnybgbfhznhf2adh"))))
    (build-system meson-build-system:meson-build-system)
    (native-inputs
     (list web:sassc
           inkscape:inkscape
           python:python-3))
    (home-page "https://github.com/rsauex/Nordic")
    (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
    (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
    (license license:gpl3)))

(define-public rofi-nord-theme
  (let ((commit "71c76dcf31e38b426519198276cf6b5ba626e61c"))
    (package
      (name "rofi-nord-theme")
      (version (git-download:git-version "1.0" "0" commit))
      (source
       (origin
         (method git-download:git-fetch)
         (uri (git-download:git-reference
               (url "https://github.com/amayer5125/nord-rofi/")
               (commit commit)))
         (sha256
          (base32
           "18kflmsr8351y7jljwdw07ncwni2q0hfq5c772zdhcir1bzpi6al"))
         (file-name (git-download:git-file-name name version))))
      (build-system copy-build-system:copy-build-system)
      (arguments
       `(#:install-plan
         `(("./nord.rasi" "share/rofi/themes/"))))
      (home-page "https://github.com/amayer5125/nord-rofi/")
      (synopsis "A Rofi theme based on Nord theme by Arctic Ice Studio.")
      (description "A Rofi theme based on Nord theme by Arctic Ice Studio.")
      (license license:expat))))
