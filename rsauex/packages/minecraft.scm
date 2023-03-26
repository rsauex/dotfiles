(define-module (rsauex packages minecraft)
  #:use-module ((gnu packages compression) #:prefix compression:)
  #:use-module ((gnu packages gl)          #:prefix gl:)
  #:use-module ((gnu packages java)        #:prefix java:)
  #:use-module ((gnu packages pulseaudio)  #:prefix pulseaudio:)
  #:use-module ((gnu packages qt)          #:prefix qt:)
  #:use-module ((gnu packages xorg)        #:prefix xorg:)
  #:use-module ((guix build-system cmake)  #:prefix cmake-build-system:)
  #:use-module ((guix git-download)        #:prefix git-download:)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module ((guix packages)))

(define-public polymc
  (package
    (name "polymc")
    (version "1.2.2")
    (source (origin
              (method git-download:git-fetch)
              (uri (git-download:git-reference
                    (url "https://github.com/PolyMC/PolyMC.git")
                    (recursive? #t)
                    (commit version)))
              (file-name (git-download:git-file-name name version))
              (sha256
               (base32
                "09kwxiq8jxgnf97mdkdw4w6vqkjjryb5imjq6mjf1azfpg7qq64p"))))
    (build-system cmake-build-system:cmake-build-system)
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
     `(("jdk" ,java:icedtea "jdk")
       ("zlib" ,compression:zlib)
       ("qtbase" ,qt:qtbase-5)
       ("xrandr" ,xorg:xrandr)
       ("libx11" ,xorg:libx11)
       ("libxext" ,xorg:libxext)
       ("libxcursor" ,xorg:libxcursor)
       ("libxrandr" ,xorg:libxrandr)
       ("libxxf86vm" ,xorg:libxxf86vm)
       ("pulseaudio" ,pulseaudio:pulseaudio)
       ("freeglut" ,gl:freeglut)
       ("glfw" ,gl:glfw)))
    (home-page "https://polymc.org/")
    (synopsis "Launcher for Minecraft")
    (description
     "This package allows you to have multiple, separate instances of
Minecraft (each with their own mods, texture packs, saves, etc) and
helps you manage them and their associated options with a simple
interface.")
    (license (list license:gpl3+))))
