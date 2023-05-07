(define-module (rsauex packages nordic-theme)
  #:use-module ((guix build-system copy) #:prefix copy-build-system:)
  #:use-module ((guix git-download)      #:prefix git-download:)
  #:use-module ((guix licenses)          #:prefix license:)
  #:use-module ((guix packages)))

(define-public nordic-darker-theme
  (package
    (name "nordic-theme")
    (version "2.2.0")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit "v2.2.0")))
       (sha256
        (base32
         "02aa1dphzjfy1fi7ln4554di24fa0kcgz8baabm15m9zm9sqfdf1"))
       (file-name (git-download:git-file-name name version))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     `(#:install-plan
       `(("." "share/themes/Nordic-Darker"
          #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                     "package-lock.json" "Gulpfile.js")))))
    (home-page "https://github.com/EliverLara/Nordic")
    (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
    (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
    (license license:gpl3)))

(define-public nordic-darker-kvantum-theme
  (package
    (name "nordic-darker-kvantum-theme")
    (version "2.2.0")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit "v2.2.0")))
       (sha256
        (base32
         "02aa1dphzjfy1fi7ln4554di24fa0kcgz8baabm15m9zm9sqfdf1"))
       (file-name (git-download:git-file-name name version))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     `(#:install-plan
       `(("./kde/kvantum/Nordic-Darker" "share/Kvantum/Nordic-Darker"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-transparency
           (lambda _
             (substitute* `("./kde/kvantum/Nordic-Darker/Nordic-Darker.kvconfig")
               (("translucent_windows=true")
                "translucent_windows=false")
               (("composite=true")
                "composite=false"))
             #t)))))
    (home-page "https://github.com/EliverLara/Nordic")
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
