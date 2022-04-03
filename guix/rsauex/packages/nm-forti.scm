(define-module (rsauex packages nm-forti)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public network-manager-openfortivpn
  (package
    (name "network-manager-openfortivpn")
    (version "1.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-fortisslvpn/"
                    (version-major+minor version)
                    "/NetworkManager-fortisslvpn-" version ".tar.xz"))
              (sha256
               (base32
                "1sw66cxgs4in4cjp1cm95c5ijsk8xbbmq4ykg2jwqwgz6cf2lr3s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var"
                           "--with-gnome=yes")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-command-file-names
           (lambda _
             (substitute* `("Makefile.in")
               (("^install-data-hook:")
                "install-data-hook-disabled-for-guix:"))))
         (add-after 'configure 'pathch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((ofvpn (search-input-file inputs "/bin/openfortivpn"))
                    (pretty-ofvpn (string-append "\"" ofvpn "\"")))
               (substitute* "src/nm-fortisslvpn-service.c"
                 (("\"/bin/openfortivpn\"") pretty-ofvpn))
               (substitute* "src/nm-fortisslvpn-service.c"
                 (("#define NM_FORTISSLVPN_WAIT_PPPD 10000")
                  "#define NM_FORTISSLVPN_WAIT_PPPD 60000")))
             #t)))))
    (native-inputs
     (list pkg-config
           intltool))
    (inputs
     (list openfortivpn
           network-manager
           `(,glib "bin")
           ppp
           gtk+
           libnma
           libsecret))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenFortiVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via OpenFortiVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-fortisslvpn")))))
