(define-module (rsauex packages fonts)
  #:use-module ((gnu packages fonts)   #:prefix fonts:)
  #:use-module ((gnu packages))
  #:use-module ((guix download))
  #:use-module ((guix gexp))
  #:use-module ((guix licenses)        #:prefix license:)
  #:use-module ((guix packages))
  #:use-module ((guix utils)))

(define-public font-iosevka-fixed
  (package
    (inherit fonts:font-iosevka)
    (name "font-iosevka-fixed")
    (version (package-version fonts:font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/PkgTTF-IosevkaFixed-" version ".zip"))
       (sha256
        (base32 "11dgd79ipprzxs208mpyhi730pc1gb6fjj1dfj9swwjm433127nl"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))
