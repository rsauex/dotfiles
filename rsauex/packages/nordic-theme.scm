(define-module (rsauex packages nordic-theme)
  #:use-module ((guix build-system copy) #:prefix copy-build-system:)
  #:use-module ((guix git-download)      #:prefix git-download:)
  #:use-module ((guix licenses)          #:prefix license:)
  #:use-module ((guix packages)))

(define nordic-source
  (origin
    (method git-download:git-fetch)
    (uri (git-download:git-reference
          (url "https://github.com/rsauex/Nordic")
          (commit "294cd2ee45a1bceb7a3ac356945ae1319a8ff0eb")))
    (sha256
     (base32
      "10bmbn3balv30dn9bq707s5043kzpm6bhrf2f8x1jn7rcj7m4mrc"))))

(define nordic-version
  "2.2.0-294cd2e")

(define-public nordic-darker-theme
  (package
    (name "nordic-theme")
    (version nordic-version)
    (source nordic-source)
    (build-system copy-build-system:copy-build-system)
    (arguments
     `(#:install-plan
       `(("./gtk-2.0" "share/themes/Nordic-Darker/gtk-2.0")
         ("./gtk-3.0" "share/themes/Nordic-Darker/gtk-3.0")
         ("./gtk-4.0" "share/themes/Nordic-Darker/gtk-4.0")
         ("./kde/kvantum/Nordic-Darker-Solid" "share/Kvantum/Nordic-Darker-Solid"))))
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
