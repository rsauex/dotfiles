(define-module (rsauex packages nordic-theme)
  #:use-module (gnu packages)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public nordic-darker-theme
  (let ((commit "6d38d6af363528f42619f663e3ecd4c08dfd2411")
	(revision "0"))
  (package
   (name "nordic-theme")
   (version (git-version "1.9.0" revision commit))
   (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit commit)))
     (sha256
       (base32
         "0m1jr05qb3pkxq5yf46i7r5jvrkp736azbwxrmnkjzdj9r46594d"))
     (file-name (git-file-name name version))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("." "share/themes/Nordic-Darker"
         #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                    "package-lock.json" "Gulpfile.js")))))
   (home-page "https://github.com/EliverLara/Nordic")
   (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
   (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
   (license license:gpl3))))

(define-public nordic-darker-kvantum-theme
  (let ((commit "07d764c5ebd5706e73d2e573f1a983e37b318915")
	(revision "0"))
    (package
      (name "nordic-darker-kvantum-theme")
      (version (git-version "1.9.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/EliverLara/Nordic")
               (commit commit)))
         (sha256
          (base32
           "0y2s9d6h1b195s6afp1gb5rb1plfslkpbw2brd30a9d66wfvsqk0"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("./kde/kvantum/Nordic-Darker" "share/Kvantum/Nordic-Darker"))))
      (home-page "https://github.com/EliverLara/Nordic")
      (synopsis "Dark Gtk3.20+ theme using the Nord color palette")
      (description "Nordic is a Gtk3.20+ theme created using the Nord color
palette.")
      (license license:gpl3))))

(define-public rofi-nord-theme
  (let ((commit "71c76dcf31e38b426519198276cf6b5ba626e61c"))
    (package
      (name "rofi-nord-theme")
      (version (git-version "1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/amayer5125/nord-rofi/")
               (commit commit)))
         (sha256
          (base32
           "18kflmsr8351y7jljwdw07ncwni2q0hfq5c772zdhcir1bzpi6al"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         `(("./nord.rasi" "share/rofi/themes/"))))
      (home-page "https://github.com/amayer5125/nord-rofi/")
      (synopsis "A Rofi theme based on Nord theme by Arctic Ice Studio.")
      (description "A Rofi theme based on Nord theme by Arctic Ice Studio.")
      (license license:expat))))
