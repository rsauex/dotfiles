(define-module (rsauex packages rclone)
  #:use-module ((guix build-system copy)  #:prefix copy-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix download)           #:prefix download:)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)))

(define-public rclone-next
  (package
    (name "rclone-next")
    (version "1.71.0")
    (source
     (origin
       (method download:url-fetch/zipbomb)
       (uri (string-append "https://github.com/rclone/rclone/releases/download/v"
                           version
                           "/rclone-v"
                           version
                           "-linux-amd64.zip"))
       (sha256
        (base32
         "14wyd767nbk3whgfvblb3kpiyzskcqbq61nh1jq6wbpg6pawznrx"))))
    (build-system copy-build-system:copy-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'install 'enter-subdirectory
                 (lambda* (#:key #:allow-other-keys)
                   (lambda _
                     (chdir (string-append "rclone-v" #$version "-linux-amd64"))))))
           #:install-plan
           #~'((#$(string-append "rclone-v" version "-linux-amd64/rclone")   "/bin/rclone")
               (#$(string-append "rclone-v" version "-linux-amd64/rclone.1") "/share/man/man1/rclone.1"))))
    (synopsis "@code{rsync} for cloud storage")
    (description "@code{Rclone} is a command line program to sync files and
directories to and from different cloud storage providers.

Features include:
@itemize
@item MD5/SHA1 hashes checked at all times for file integrity
@item Timestamps preserved on files
@item Partial syncs supported on a whole file basis
@item Copy mode to just copy new/changed files
@item Sync (one way) mode to make a directory identical
@item Check mode to check for file hash equality
@item Can sync to and from network, e.g., two different cloud accounts
@item Optional encryption (Crypt)
@item Optional cache (Cache)
@item Optional FUSE mount (rclone mount)
@end itemize")
    (home-page "https://rclone.org/")
    (license license:expat)))
