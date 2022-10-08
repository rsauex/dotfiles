(define-module (rsauex packages kvantum)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix build-system qt)
  #:use-module (guix utils))

(define-public kvantum
  (package
    (name "kvantum")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tsujan/Kvantum")
             (commit (string-append "V" version))))
       (sha256
        (base32
         "0yvxj7r9z890nfq5cadw7ys144c2mnvaplvx4v4ndv7238b741l8"))))
    (build-system qt-build-system)
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
     `(,extra-cmake-modules
       ,qttools))
    (inputs
     `(,qtbase-5
       ,qtsvg-5
       ,qtx11extras
       ,libx11
       ,libxext
       ,kwindowsystem))
    (synopsis "SVG-based Qt5 theme engine plus a config tool and extra themes")
    (description "SVG-based Qt5 theme engine plus a config tool and extra themes")
    (home-page "https://github.com/tsujan/Kvantum")
    (license gpl3+)))
