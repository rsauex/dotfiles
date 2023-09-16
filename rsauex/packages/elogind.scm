(define-module (rsauex packages elogind)
  #:use-module ((gnu packages freedesktop) #:prefix freedesktop:)
  #:use-module ((guix download))
  #:use-module ((guix packages))
  #:use-module ((rsauex packages) #:prefix my-packages:))

(define-public elogind
  (package-with-extra-patches
   (package
     (inherit freedesktop:elogind)
     (version (string-append (package-version freedesktop:elogind)
                             "-with-234-fix"))
     (synopsis (string-append (package-synopsis freedesktop:elogind)
                              " (with a fix for issue 234)")))
   (list (my-packages:search-rsauex-patch "elogind-234.patch"))))
