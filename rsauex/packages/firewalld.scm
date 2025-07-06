(define-module (rsauex packages firewalld)
  #:use-module ((gnu packages python)      #:prefix python:)
  #:use-module ((gnu packages python-xyz)  #:prefix python-xyz:)
  #:use-module ((gnu packages glib)        #:prefix glib:)
  #:use-module ((gnu packages pkg-config)  #:prefix pkg-config:)
  #:use-module ((gnu packages docbook)     #:prefix docbook:)
  #:use-module ((gnu packages xml)         #:prefix xml:)
  #:use-module ((gnu packages autotools)   #:prefix autotools:)
  #:use-module ((gnu packages gnome)       #:prefix gnome:)
  #:use-module ((gnu packages linux)       #:prefix linux:)
  #:use-module ((guix build-system gnu)    #:prefix gnu-build-system:)
  #:use-module ((guix build-system python) #:prefix python-build-system:)
  #:use-module ((guix download)            #:prefix download:)
  #:use-module ((guix git-download)        #:prefix git-download:)
  #:use-module ((guix licenses)            #:prefix licenses:)
  #:use-module ((guix packages))
  #:use-module ((guix gexp))
  #:use-module ((rsauex packages)))

(define-public python-nftables
  (package
    (name "python-nftables")
    (version (package-version linux:nftables))
    (source (package-source linux:nftables))
    (build-system python-build-system:python-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'enter-subdirectory
                     (lambda _
                       (chdir "py")))
                   (add-after 'enter-subdirectory 'patch-libnftables-location
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "nftables.py"
                         (("libnftables.so.1")
                          (string-append (assoc-ref inputs "nftables") "/lib/libnftables.so.1"))))))))
    (inputs (list linux:nftables))
    (home-page "https://www.nftables.org")
    (synopsis "Userspace utility for Linux packet filtering")
    (description "nftables is the project that aims to replace the existing
{ip,ip6,arp,eb}tables framework.  Basically, this project provides a new packet
filtering framework, a new userspace utility and also a compatibility layer for
{ip,ip6}tables.  nftables is built upon the building blocks of the Netfilter
infrastructure such as the existing hooks, the connection tracking system, the
userspace queueing component and the logging subsystem.")
    (license licenses:gpl2)))

(define-public firewalld
  (package
    (name "firewalld")
    (version "2.3.1")
    (source (origin
              (method git-download:git-fetch)
              (uri (git-download:git-reference
                    (url "https://github.com/firewalld/firewalld.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "14hf84a9zxa42amfssqn11sz1bkz1pp6b7abkq29k7y8k0j75niq"))
              (patches
               (list (search-rsauex-patch "firewalld-respect-xml-catalog-files-var.patch")
                     (search-rsauex-patch "firewalld-nm-connection-manager.patch")))))
    (build-system gnu-build-system:gnu-build-system)
    (arguments
     `(#:tests? #f                      ; TODO
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* (list "src/gtk3_niceexpander.py"
                                "src/gtk3_chooserbutton.py")
               (("/usr/bin/python")
                (string-append (assoc-ref inputs "python") "/bin/python3")))
             (substitute* "config/xmlschema/check.sh"
               (("/usr/bin/xmllint")
                (string-append (assoc-ref inputs "libxml2") "/bin/xmllint")))
             (substitute* "src/firewall-applet.in"
               (("NM_CONNECTION_EDITOR = \"\"")
                (string-append "NM_CONNECTION_EDITOR = \""
                               (assoc-ref inputs "network-manager-applet") "/bin/nm-connection-editor"
                               "\"")))
             (substitute* "src/firewall/config/__init__.py.in"
               (("DATADIR = \"/usr/share/\"")
                (string-append "DATADIR = \""
                               (assoc-ref outputs "out") "/share/"
                               "\"")))))
         (add-before 'install 'make-local-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "doc/xml/firewalld.xml"
                                "doc/xml/firewalld.direct.xml"
                                "doc/xml/firewalld.helper.xml"
                                "doc/xml/firewalld.xml.in"
                                "doc/xml/firewall-cmd.xml"
                                "doc/xml/firewall-cmd.xml.in"
                                "doc/xml/firewalld.service.xml"
                                "doc/xml/firewalld.zone.xml"
                                "doc/xml/firewalld.policy.xml"
                                "doc/xml/firewall-config.xml"
                                "doc/xml/firewalld.icmptype.xml"
                                "doc/xml/firewalld.zones.xml"
                                "doc/xml/firewalld.conf.xml"
                                "doc/xml/firewall-applet.xml"
                                "doc/xml/firewalld.dbus.xml"
                                "doc/xml/firewall-offline-cmd.xml"
                                "doc/xml/firewalld.richlanguage.xml"
                                "doc/xml/firewalld.policies.xml"
                                "doc/xml/firewalld.ipset.xml")
               (("http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/docbookx.dtd")))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define (python-version python)
               (let* ((version     (car (last-pair (string-split python #\-))))
                      (components  (string-split version #\.))
                      (major+minor (list-head components 2)))
                 (string-join major+minor ".")))

             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref inputs "python"))
                    (programs
                     (append
                      (map (lambda (p)
                             (string-append out "/bin/" p))
                           '("firewall-applet"
                             "firewall-cmd"
                             "firewall-config"
                             "firewall-offline-cmd"))
                      (map (lambda (p)
                             (string-append out "/sbin/" p))
                           '("firewalld")))))
               (for-each (lambda (program)
                           (wrap-program program
                             `("GUIX_PYTHONPATH" prefix
                               ,(cons* (string-append out "/lib/python" (python-version python) "/site-packages")
                                       ;; (string-append out "/share/firewalld")
                                       (search-path-as-string->list
                                        (or (getenv "GUIX_PYTHONPATH") ""))))
                             `("GI_TYPELIB_PATH" =
                               ,(search-path-as-string->list
                                 (or (getenv "GI_TYPELIB_PATH") "")))))
                         programs)))))))
    (native-inputs
     (list glib:intltool
           autotools:autoconf
           autotools:automake
           pkg-config:pkg-config))
    (inputs
     (list python:python-3
           python-xyz:python-dbus
           python-nftables
           glib:python-pygobject
           glib:glib
           glib:gobject-introspection
           `(,glib:glib "bin")
           docbook:docbook-xsl
           docbook:docbook-xml-4.2
           xml:libxml2
           xml:libxslt
           gnome:network-manager-applet
           linux:iptables
           linux:ipset))
    (home-page "https://firewalld.org/")
    (synopsis "Stateful zone based firewall daemon with D-Bus interface.")
    (description
     "Stateful zone based firewall daemon with D-Bus interface.")
    (license licenses:gpl2+)))
