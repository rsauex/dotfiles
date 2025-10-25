(define-module (rsauex packages flexbv)
  #:use-module ((gnu packages base)           #:prefix base:)
  #:use-module ((gnu packages compression)    #:prefix compression:)
  #:use-module ((gnu packages fontutils)      #:prefix fontutils:)
  #:use-module ((gnu packages gcc)            #:prefix gcc:)
  #:use-module ((gnu packages glib)           #:prefix glib:)
  #:use-module ((gnu packages gtk)            #:prefix gtk:)
  #:use-module ((guix build-system copy)      #:prefix copy-build-system:)
  #:use-module ((guix download)               #:prefix download:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)           #:prefix git-download:)
  #:use-module ((nonguix licenses)            #:prefix non-license:)
  #:use-module ((guix packages))
  #:use-module ((nonguix build-system binary) #:prefix non-binary-build-system:))

(define-public flexbv
  (package
    (name "flexbv")
    (version "5.1243")
    (source
     (origin
       (method download:url-fetch/tarbomb)
       (uri (string-append "https://pldaniels.com/flexbv5/free/FlexBVFree-"
                           version
                           "-linux.tar.gz"))
       (sha256
        (base32
         "1zv9zpimbnqa4czhgxiapv1ni13dx310wfp3b3j988l8i7hqk3ss"))))
    (build-system non-binary-build-system:binary-build-system)
    (arguments
     (list #:patchelf-plan
           #~`((#$(string-append "FlexBVFree-" version "-linux/flexbv")
                ("libc"
                 "gcc"
                 "zlib"
                 "glibc"
                 "fontconfig-minimal"
                 "gtk+"
                 "glib")))
           #:install-plan
           #~'((#$(string-append "FlexBVFree-" version "-linux/flexbv")
                "/bin/"))))
    (inputs
     (list `(,gcc:gcc "lib")
           compression:zlib
           base:glibc
           fontutils:fontconfig
           gtk:gtk+
           glib:glib))
    (home-page "https://pldaniels.com/flexbv5")
    (synopsis "A boardview files viewer.")
    (description "FlexBV integrates your boardview files with schematics to
substantially ease the process of tracking down faults and understanding
damaged boards.")
    (license (non-license:undistributable "https://pldaniels.com/flexbv5"))))
