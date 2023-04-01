(define-module (rsauex packages xorg)
  #:use-module ((gnu packages xorg)  #:prefix xorg:)
  #:use-module ((guix gexp))
  #:use-module ((guix packages)))

;; Adopted from Alezost's config:
;; https://notabug.org/alezost/guix-config/commit/161d5e2e36e53c0d9c46181e10e1595e607748dd

(define-public libxfont2
  (package
    (inherit xorg:libxfont2)
    (version (string-append (package-version xorg:libxfont2)
                            "-follow-symlinks"))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-fonts-can-be-links
           (lambda _
             ;; Fix the problem introduced by
             ;; <https://cgit.freedesktop.org/xorg/lib/libXfont/commit/?id=7b377456f95d2ec3ead40f4fb74ea620191f88c8>:
             ;; if "fonts.dir" is a symlink, do not ignore it as all
             ;; files in Guix profiles are symlinks!
             (substitute* '("src/fontfile/dirfile.c"
                            "src/fontfile/fileio.c")
               (("\\| ?O_NOFOLLOW") "")))))))
    (synopsis (string-append (package-synopsis xorg:libxfont2)
                             " (with fixed fonts determination)"))))

(define-public xorg-server
  (package
    (inherit xorg:xorg-server)
    (inputs (modify-inputs (package-inputs xorg:xorg-server)
              (replace "libxfont2" libxfont2)))
    (synopsis (string-append (package-synopsis xorg:xorg-server)
                             " (with my modifications)"))))
