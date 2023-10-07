(define-module (rsauex packages kvantum)
  #:use-module ((gnu packages kde-frameworks) #:prefix kde-frameworks:)
  #:use-module ((gnu packages qt)             #:prefix qt:)
  #:use-module ((gnu packages xorg)           #:prefix xorg:)
  #:use-module ((guix build-system qt)        #:prefix qt-build-system:)
  #:use-module ((guix gexp))
  #:use-module ((guix git-download)           #:prefix git-download:)
  #:use-module ((guix licenses)               #:prefix licenses:)
  #:use-module ((guix packages)))

(define-public kvantum
  (package
    (name "kvantum")
    (version "1.0.5")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/tsujan/Kvantum")
             (commit (string-append "V" version))))
       (sha256
        (base32
         "0xdx4i0c27advkzc645ckyvgn323kamfnvgnibaiv256k4x5750c"))))
    (build-system qt-build-system:qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DENABLE_QT5=ON"
                           "-DENABLE_QT4=OFF"
                           "-DCMAKE_BUILD_TYPE=Release")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "Kvantum")
             #t))
         (add-after 'enter-subdirectory 'patch-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (with-output-to-file "CMakeLists.txt"
               (lambda _
                 (display "project(kvantum)\n")
                 (display "cmake_minimum_required(VERSION 3.0)\n")
                 (display "add_definitions (-Wall)\n")
                 (display "add_subdirectory(style)\n")))
             (substitute* `("style/CMakeLists.txt")
               (("\\$\\{_Qt5_PLUGIN_INSTALL_DIR\\}")
                (string-append (assoc-ref outputs "out") "/lib/qt5/plugins"))
               (("add_definitions\\(-DDATADIR=\"\\$\\{CMAKE_INSTALL_PREFIX\\}/share\"\\)")
                ;; TODO: Allow exprs in DATADIR (replace QLiteralString with QString)
                ;; "add_definitions(-DDATADIR=\"/run/current-system/profile/share\")"
                "add_definitions(-DDATADIR=\"/home/rsauex/.guix-home/profile/share\")"))
             #f)))))
    (native-inputs
     `(,kde-frameworks:extra-cmake-modules
       ,qt:qttools))
    (inputs
     `(,qt:qtbase-5
       ,qt:qtsvg-5
       ,qt:qtx11extras
       ,xorg:libx11
       ,xorg:libxext
       ,kde-frameworks:kwindowsystem))
    (synopsis "SVG-based Qt5 theme engine plus a config tool and extra themes")
    (description "SVG-based Qt5 theme engine plus a config tool and extra themes")
    (home-page "https://github.com/tsujan/Kvantum")
    (license licenses:gpl3+)))

(define-public kvantum-preview
  (package
    (name "kvantum-preview")
    (version "1.0.5")
    (source
     (origin
       (method git-download:git-fetch)
       (uri (git-download:git-reference
             (url "https://github.com/tsujan/Kvantum")
             (commit (string-append "V" version))))
       (sha256
        (base32
         "0xdx4i0c27advkzc645ckyvgn323kamfnvgnibaiv256k4x5750c"))))
    (build-system qt-build-system:qt-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DENABLE_QT5=ON"
                           "-DENABLE_QT4=OFF"
                           "-DCMAKE_BUILD_TYPE=Release")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-subdirectory
           (lambda _
             (chdir "Kvantum")
             #t))
         (add-after 'enter-subdirectory 'patch-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (with-output-to-file "CMakeLists.txt"
               (lambda _
                 (display "project(kvantum)\n")
                 (display "cmake_minimum_required(VERSION 3.0)\n")
                 (display "add_definitions (-Wall)\n")
                 (display "add_subdirectory(kvantumpreview)\n")))
             (substitute* `("style/CMakeLists.txt")
               (("\\$\\{_Qt5_PLUGIN_INSTALL_DIR\\}")
                (string-append (assoc-ref outputs "out") "/lib/qt5/plugins"))
               (("add_definitions\\(-DDATADIR=\"\\$\\{CMAKE_INSTALL_PREFIX\\}/share\"\\)")
                ;; TODO: Allow exprs in DATADIR (replace QLiteralString with QString)
                ;; "add_definitions(-DDATADIR=\"/run/current-system/profile/share\")"
                ;; "add_definitions(-DDATADIR=\"/home/rsauex/.guix-home/profile/share\")"
                "add_definitions(-DDATADIR=\"/home/rsauex/Projects/Nordic/kde\")"
                ))
             #f)))))
    (native-inputs
     `(,kde-frameworks:extra-cmake-modules
       ,qt:qttools))
    (inputs
     `(,qt:qtbase-5
       ,qt:qtsvg-5
       ,qt:qtx11extras
       ,xorg:libx11
       ,xorg:libxext
       ,kde-frameworks:kwindowsystem))
    (synopsis "Kvantum theme engine theme preview tool")
    (description "Kvantum theme engine theme preview tool")
    (home-page "https://github.com/tsujan/Kvantum")
    (license licenses:gpl3+)))
