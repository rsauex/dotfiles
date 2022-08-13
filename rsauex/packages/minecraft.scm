(define-module (rsauex packages minecraft)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages java)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public polymc
  (package
    (name "polymc")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PolyMC/PolyMC.git")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09kwxiq8jxgnf97mdkdw4w6vqkjjryb5imjq6mjf1azfpg7qq64p"))))
    (build-system cmake-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DLauncher_PORTABLE=0")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jdk (assoc-ref inputs "jdk")))
               (substitute* (list "launcher/java/JavaUtils.cpp")
                 (("scanJavaDir\\(\"/usr/lib/jvm\"\\)")
                  (string-append "javas.append(\"" jdk "/bin/java\")"))
                 ;; (("scanJavaDir\\(\"/usr/lib32/jvm\"\\)")
                 ;;  (string-append "javas.append(\"" jdk "/bin/java\")"))
                 ))
             #t))
         (add-after 'install 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin"))
                    (exe            (string-append bin "/polymc"))
                    (qtwayland      (assoc-ref inputs "qtwayland"))
                    (xrandr         (assoc-ref inputs "xrandr"))
                    (jdk            (assoc-ref inputs "jdk")))
               (wrap-program exe
                 `("PATH" ":" prefix (,(string-append xrandr "/bin")
                                      ,(string-append jdk "/bin")))
                 `("GAME_LIBRARY_PATH" ":" prefix
                   (,@(map (lambda (dep)
                             (string-append (assoc-ref inputs dep)
                                            "/lib"))
                           '("libx11" "libxext" "libxcursor"
                             "libxrandr" "libxxf86vm" "pulseaudio" "mesa")))))
               #t))))))
    (inputs
     `(("jdk" ,icedtea "jdk")
       ("zlib" ,zlib)
       ("qtbase" ,qtbase-5)
       ("xrandr" ,xrandr)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxcursor" ,libxcursor)
       ("libxrandr" ,libxrandr)
       ("libxxf86vm" ,libxxf86vm)
       ("pulseaudio" ,pulseaudio)
       ("freeglut" ,freeglut)
       ("glfw" ,glfw)))
    (home-page "https://polymc.org/")
    (synopsis "Launcher for Minecraft")
    (description
     "This package allows you to have multiple, separate instances of
Minecraft (each with their own mods, texture packs, saves, etc) and
helps you manage them and their associated options with a simple
interface.")
    (license (list license:gpl3+))))
