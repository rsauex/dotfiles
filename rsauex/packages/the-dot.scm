(define-module (rsauex packages the-dot)
  #:use-module ((guix build-system trivial) #:prefix trivial-build-system:)
  #:use-module ((guix download)             #:prefix download:)
  #:use-module ((guix licenses)             #:prefix licenses:)
  #:use-module ((guix packages))
  #:use-module ((rsauex packages)           #:prefix my-packages:)
  #:use-module ((web uri)))

(define-public the-dot-cursor-theme
  (package
    (name "the-dot-cursor-theme")
    (version "0.6")
    (source
     (origin
       (method download:url-fetch/tarbomb)
       (uri (uri->string (build-uri 'file #:path (my-packages:search-rsauex-aux-file "thedot0.6.tar.gz"))))
       (sha256 (base32 "1iwvlv9qcrjyfbzab00vjqafmp3vdybz1hi02r6lwbgvwyfyrifg"))))
    (build-system trivial-build-system:trivial-build-system)
    (arguments (list #:modules '((guix build utils))
                     #:builder `(begin
                                  (use-modules (guix build utils))
                                  (let ((source (string-append (assoc-ref %build-inputs "source") "/thedot0.6"))
                                        (output (string-append (assoc-ref %outputs "out") "/share/icons")))
                                    (mkdir-p output)
                                    (copy-recursively source output)))))
    (synopsis "A Gtk theme based on Material Design Refresh.")
    (description "A Gtk theme based on Material Design Refresh.")
    (home-page "https://gitlab.com/tista500/plata-theme")
    (license licenses:gpl2)))
