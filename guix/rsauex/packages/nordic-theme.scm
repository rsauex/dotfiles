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
