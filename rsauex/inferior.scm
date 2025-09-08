(define-module (rsauex inferior)
  #:use-module ((guix inferior) #:prefix inferior:)
  #:use-module ((guix store))
  #:autoload   (guix ui) (build-notifier)
  #:use-module ((ice-9 match))
  #:export (inferior-for-channels
            lookup-package-in-channel))

(define* (inferior-for-channels channels
                                #:key
                                (cache-directory (inferior:%inferior-cache-directory))
                                (ttl (* 3600 24 30))
                                (authenticate? #t))
  "Return an inferior for CHANNELS, a list of channels.  Use the cache at
CACHE-DIRECTORY, where entries can be reclaimed after TTL seconds.  This
procedure opens a new connection to the build daemon.

This is a convenience procedure that people may use in manifests passed to
'guix package -m', for instance."
  (define cached
    (with-store store
      ;; XXX: Install a build notifier out of convenience, so users know
      ;; what's going on.  However, we cannot be sure that its options, such
      ;; as #:use-substitutes?, correspond to the daemon's default settings.
      (with-build-handler (build-notifier)
        (inferior:cached-channel-instance store
                                          channels
                                          #:cache-directory cache-directory
                                          #:ttl ttl
                                          #:authenticate? authenticate?))))
  (inferior:open-inferior cached))

(define* (lookup-package-in-channel channel package
                                    #:key
                                    (authenticate? #t))
  (define inferior
    (inferior-for-channels (list channel)
                           #:authenticate? authenticate?))

  (match (inferior:lookup-inferior-packages inferior package)
    (()
     (error "Package not found"))
    ((package)
     package)
    ((_ _ ...)
     (error "More that one package found"))))
