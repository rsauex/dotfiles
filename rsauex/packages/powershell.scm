(define-module (rsauex packages powershell)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages less)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages instrumentation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system trivial))

(define-public powershell
  (package
    (name "powershell")
    (version "7.2.6")
    (source (origin (method url-fetch/tarbomb)
                    (uri (string-append "https://github.com/PowerShell/PowerShell/releases/download/v" version "/powershell-" version "-linux-x64.tar.gz"))
                    (sha256 (base32 "1lip3855jgn7m7i62srgi8vhvhcn4xkgxy1q943qddnb9g7zc9wr"))))
    (build-system trivial-build-system)
    (arguments (list #:modules '((guix build utils))
                     #:builder `(begin
                                  (use-modules (guix build utils))
                                  (let* ((source (assoc-ref %build-inputs "source"))
                                         (output (assoc-ref %outputs "out"))
                                         (pwsh-output (string-append output "/share/powershell")))
                                    (mkdir-p output)
                                    (mkdir-p pwsh-output)
                                    (copy-recursively source pwsh-output)
                                    (for-each make-file-writable (find-files pwsh-output))
                                    (let* ((patchelf (string-append (assoc-ref %build-inputs "patchelf") "/bin/patchelf"))
                                           (pwsh (string-append pwsh-output "/pwsh")))
                                      (let ((glibc (assoc-ref %build-inputs "glibc")))
                                        (invoke patchelf "--set-interpreter" (string-append glibc "/lib/ld-linux-x86-64.so.2") pwsh))
                                      (let ((rpath (string-append (assoc-ref %build-inputs "gcc:lib") "/lib")))
                                        (invoke patchelf "--set-rpath" rpath pwsh))
                                      (let* ((bin (string-append output "/bin"))
                                             (wrapper (string-append bin "/pwsh"))
                                             (bash (assoc-ref %build-inputs "bash")))
                                        (mkdir-p bin)
                                        (with-output-to-file wrapper
                                          (lambda _
                                            (format #t "#!~a/bin/bash~%" bash)
                                            (format #t "export LD_LIBRARY_PATH='~{~a~^:~}'~%"
                                                    (map (lambda (package)
                                                           (string-append (assoc-ref %build-inputs package) "/lib"))
                                                         '("libunwind" "util-linux" "icu4c" "curl" "openssl" "linux-pam" "lttng-ust" "zlib")))
                                            (format #t "export TERMINFO='~a/share/terminfo'~%" (assoc-ref %build-inputs "ncurses"))
                                            (format #t "exec ~a/pwsh \"$@\"~%" pwsh-output)))
                                        (chmod wrapper #o755)))))))
    (inputs `(("gcc:lib" ,gcc-9 "lib")
              ("glibc" ,glibc)
              ("libunwind" ,libunwind)
              ("util-linux" ,util-linux)
              ("icu4c" ,icu4c-68)
              ("curl" ,curl)
              ("openssl" ,openssl)
              ("lttng-ust" ,lttng-ust)
              ("linux-pam" ,linux-pam)
              ("less" ,less)
              ("bash" ,bash)
              ("ncurses" ,ncurses)
              ("zlib" ,zlib)))
    (native-inputs `(("patchelf" ,patchelf)))
    (synopsis "Powershell shell and language")
    (description "Powerfull cross-platform (Windows, Linux and macOS) shell and scripting language based on .NET")
    (home-page "https://github.com/PowerShell/PowerShell")
    (supported-systems (list "x86_64-linux"))
    (license license:expat)))
