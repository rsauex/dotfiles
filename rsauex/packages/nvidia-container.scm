(define-module (rsauex packages nvidia-container)
  #:use-module ((gnu packages base) #:prefix base:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (rsauex packages)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages nvidia))

(define-public nvidia-modprobe
  (package
    (name "nvidia-modprobe")
    (version (package-version nvidia-driver))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-modprobe")
                     (commit version)))
              (file-name (git-file-name name version))
              (patches (list (search-rsauex-patch "nvidia-modprobe.patch")))
              (sha256
               (base32 "1xjjk3436k9dv9y44kzwvwhd8903yzkkvqqq7l5r2v3592biyr35"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list "EXTRA_CFLAGS=-fPIC"
                           (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'build 'build-static-link-libraries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (invoke "ar" "rcs" "_out/Linux_x86_64/libnvidia-modprobe-utils.a" "_out/Linux_x86_64/nvidia-modprobe-utils.o" "_out/Linux_x86_64/pci-sysfs.o")
              (copy-recursively "_out/Linux_x86_64/" (string-append #$output "/lib"))))
          (add-after 'patch-source-shebangs 'install-modeprobe-utils-includes
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (copy-recursively "modprobe-utils/" (string-append #$output "/include")))))))
    (native-inputs
     (list gcc-toolchain m4))
    (synopsis "Load the NVIDIA kernel module and create NVIDIA character device files.")
    (description "Load the NVIDIA kernel module and create NVIDIA character device files.")
    (home-page "https://github.com/NVIDIA/nvidia-modprobe")
    (license gpl2)))

(define-public libnvidia-container
  (package
    (name "libnvidia-container")
    (version "1.17.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/libnvidia-container")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (list (search-rsauex-patch "libnvidia-container.patch")))
              (sha256
               (base32
                "1kddj4d1k4w13frxsc024jva63dakipnkz9dllxxn5bxzk88zxwj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list "WITH_LIBELF=yes"
                           (string-append "GIT_TAG=" #$version)
                           (string-append "REVISION=" #$version)
                           (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (add-after 'unpack 'ensure-writable-source
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (setenv "HOME" "/tmp")
              (make-file-writable "src/ldcache.c")
              (make-file-writable "src/ldcache.h")
              (make-file-writable "src/nvc_info.c")))
          (add-after 'patch-source-shebangs 'replace-prefix
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "Makefile"
                (("/usr/local") (assoc-ref outputs "out")) ;this overrides the prefix
                (("debug??libdir?") "debug") ;ensure debug files get installed in the correct subdir
                ((".*nvidia-modprobe.mk.*") "\n")
                (("^all: shared static tools")
                 "all: shared tools")
                ((".*LIB_STATIC.*libdir.*$") ""))
              (substitute* "mk/nvcgo.mk"
                ((".*-rf.*")
                 "\tmkdir -p ${SRCS_DIR} && echo \"sources dir: ${SRCS_DIR}\"\n")
                (("CURDIR./src/..PREFIX.")
                 "CURDIR)/src/$(PREFIX)/*")) ;deleting sources fails
              (substitute* "src/cli/libnvc.c"
                (("libnvidia-ml.so.1")
                 (search-input-file inputs "/lib/libnvidia-ml.so.1")))
              (substitute* "src/nvc_internal.h"
                (("libnvidia-ml.so.1")
                 (search-input-file inputs "/lib/libnvidia-ml.so.1")))

              (setenv "C_INCLUDE_PATH" (string-append (getenv "C_INCLUDE_PATH") ":" (string-append #$libtirpc "/include/tirpc")))
              (setenv "LIBRARY_PATH" (string-append (getenv "LIBRARY_PATH") ":" (string-append #$libtirpc "/lib")))
              (setenv "LDFLAGS" (string-append (or (getenv "LDFLAGS") "") " -ltirpc -lseccomp -lcap -Wl,-rpath=" (assoc-ref outputs "out") "/lib"))
              (setenv "CFLAGS" (string-append (or (getenv "CFLAGS") "") " -DWITH_TIRPC -g")))))))
    (native-inputs
     (list libseccomp
           nvidia-modprobe
           base:which
           libtirpc
           libcap
           libelf
           git-minimal
           curl
           docker
           go
           rpcsvc-proto
           pkgconf
           nvda))
    (synopsis "Build and run containers leveraging NVIDIA GPUs")
    (description "The NVIDIA Container Toolkit allows users to build and run GPU accelerated containers. The toolkit includes a container runtime library and utilities to automatically configure containers to leverage NVIDIA GPUs.")
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (license asl2.0)))

(define-public nvidia-container-toolkit
  (package
    (name "nvidia-container-toolkit")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/NVIDIA/nvidia-container-toolkit")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01gh57jfpcv07c4442lbf9wiy0l1iwl85ig9drpp0637gbkzgwa4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/NVIDIA/nvidia-container-toolkit"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "src/github.com/NVIDIA/nvidia-container-toolkit/internal/config/config.go"
                (("/usr/bin")
                 (string-append #$output "/bin")))
              (substitute* "src/github.com/NVIDIA/nvidia-container-toolkit/internal/lookup/path.go"
                (("\"/usr/local/sbin\", \"/usr/local/bin\", \"/usr/sbin\", \"/usr/bin\", \"/sbin\", \"/bin\"")
                 "\"/run/current-system/profile/bin\", \"/run/current-system/profile/sbin\""))))
          (replace 'build
            (lambda arguments
              (for-each
               (lambda (directory)
                 (apply (assoc-ref %standard-phases 'build)
                        (append arguments (list #:import-path directory))))
               '("github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-ctk"
                 "github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-container-runtime"
                 "github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-container-runtime-hook")))))
      #:tests? #f
      #:install-source? #f))
    (propagated-inputs
     (list libnvidia-container))
    (synopsis "Build and run containers leveraging NVIDIA GPUs")
    (description "The NVIDIA Container Toolkit allows users to build and run GPU accelerated containers. The toolkit includes a container runtime library and utilities to automatically configure containers to leverage NVIDIA GPUs.")
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (license asl2.0)))
