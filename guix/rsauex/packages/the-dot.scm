(define-module (rsauex packages the-dot)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system trivial)
  #:use-module (rsauex packages)
  #:use-module (web uri))

(define-public the-dot-cursor-theme
  (package
    (name "the-dot-cursor-theme")
    (version "0.6")
    (source
      (origin
        (method url-fetch/tarbomb)
        (uri (uri->string (build-uri 'file #:path (search-rsauex-aux-file "thedot0.6.tar.gz"))))
        (sha256 (base32 "1iwvlv9qcrjyfbzab00vjqafmp3vdybz1hi02r6lwbgvwyfyrifg"))))
    (build-system trivial-build-system)
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
    (license gpl2)))
