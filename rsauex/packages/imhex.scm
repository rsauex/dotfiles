(define-module (rsauex packages imhex)
  #:use-module ((gnu packages commencement) #:prefix commencement:)
  #:use-module ((gnu packages file)         #:prefix file:)
  #:use-module ((gnu packages fontutils)    #:prefix fontutils:)
  #:use-module ((gnu packages gl)           #:prefix gl:)
  #:use-module ((gnu packages glib)         #:prefix glib:)
  #:use-module ((gnu packages maths)        #:prefix maths:)
  #:use-module ((gnu packages pkg-config)   #:prefix pkg-config:)
  #:use-module ((gnu packages tls)          #:prefix tls:)
  #:use-module ((guix build-system cmake))
  #:use-module ((guix build-system copy))
  #:use-module ((guix gexp))
  #:use-module ((guix git-download))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix packages)))

(define-public imhex-patterns
  (package
    (name "imhex-patterns")
    (version "1.28.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WerWolv/ImHex-Patterns.git")
                    (recursive? #t)
                    (commit (string-append "ImHex-v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fx8bl079pkm1ja4pfhllfjcwf76cjhrk7wsq7daamkfj9id90mg"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("." "share/imhex/"))))
    (home-page "https://imhex.werwolv.net/")
    (synopsis "Patterns for the ImHex Hex Editor")
    (description
     "Hex patterns, include patterns and magic files for the use with the ImHex Hex
Editor.")
    (license (list license:gpl2))))

(define-public imhex
  (package
    (name "imhex")
    (version "1.28.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WerWolv/ImHex.git")
                    (recursive? #t)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gq18v4519fyfhw6x00xkrr56crga3dja144dfp74h7lvrsrrb0f"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:build-type "Release"
           #:configure-flags #~'("-DIMHEX_OFFLINE_BUILD=ON")))
    (native-inputs
     `(,commencement:gcc-toolchain-12
       ,pkg-config:pkg-config))
    (inputs
     `(,fontutils:freetype
       ,gl:glfw
       ,maths:glm
       ,file:file
       ,tls:mbedtls-apache
       ,glib:dbus))
    (home-page "https://imhex.werwolv.net/")
    (synopsis "A Hex Editor")
    (description
     "A Hex Editor for Reverse Engineers, Programmers and people who value their
retinas when working at 3 AM.")
    (license (list license:gpl2))))
