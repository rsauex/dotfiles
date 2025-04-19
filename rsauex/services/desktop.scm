(define-module (rsauex services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module ((gnu system file-systems)
                #:select (%control-groups
                          %elogind-file-systems
                          file-system))
  #:autoload   (gnu services sddm) (sddm-service-type)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (gnu system uuid)
  #:use-module (gnu system pam)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages sugar)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages enlightenment)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (elogind-configuration
            elogind-configuration?
            elogind-service-type))

(define-record-type* <elogind-configuration> elogind-configuration
  make-elogind-configuration
  elogind-configuration?
  (elogind                          elogind-package
                                    (default elogind))
  (kill-user-processes?             elogind-kill-user-processes?
                                    (default #f))
  (kill-only-users                  elogind-kill-only-users
                                    (default '()))
  (kill-exclude-users               elogind-kill-exclude-users
                                    (default '("root")))
  (inhibit-delay-max-seconds        elogind-inhibit-delay-max-seconds
                                    (default 5))
  (handle-power-key                 elogind-handle-power-key
                                    (default 'poweroff))
  (handle-suspend-key               elogind-handle-suspend-key
                                    (default 'suspend))
  (handle-hibernate-key             elogind-handle-hibernate-key
                                    (default 'hibernate))
  (handle-lid-switch                elogind-handle-lid-switch
                                    (default 'suspend))
  (handle-lid-switch-docked         elogind-handle-lid-switch-docked
                                    (default 'ignore))
  (handle-lid-switch-external-power elogind-handle-lid-switch-external-power
                                    (default *unspecified*))
  (power-key-ignore-inhibited?      elogind-power-key-ignore-inhibited?
                                    (default #f))
  (suspend-key-ignore-inhibited?    elogind-suspend-key-ignore-inhibited?
                                    (default #f))
  (hibernate-key-ignore-inhibited?  elogind-hibernate-key-ignore-inhibited?
                                    (default #f))
  (lid-switch-ignore-inhibited?     elogind-lid-switch-ignore-inhibited?
                                    (default #t))
  (holdoff-timeout-seconds          elogind-holdoff-timeout-seconds
                                    (default 30))
  (idle-action                      elogind-idle-action
                                    (default 'ignore))
  (idle-action-seconds              elogind-idle-action-seconds
                                    (default (* 30 60)))
  (runtime-directory-size-percent   elogind-runtime-directory-size-percent
                                    (default 10))
  (runtime-directory-size           elogind-runtime-directory-size
                                    (default #f))
  (remove-ipc?                      elogind-remove-ipc?
                                    (default #t))

  (suspend-state                    elogind-suspend-state
                                    (default '("mem" "standby" "freeze")))
  (suspend-mode                     elogind-suspend-mode
                                    (default '()))
  (hibernate-state                  elogind-hibernate-state
                                    (default '("disk")))
  (hibernate-mode                   elogind-hibernate-mode
                                    (default '("platform" "shutdown")))
  (hybrid-sleep-state               elogind-hybrid-sleep-state
                                    (default '("disk")))
  (hybrid-sleep-mode                elogind-hybrid-sleep-mode
                                    (default
                                      '("suspend" "platform" "shutdown")))
  (hibernate-delay-seconds          elogind-hibernate-delay-seconds
                                    (default *unspecified*))
  (suspend-estimation-seconds       elogind-suspend-estimation-seconds
                                    (default *unspecified*))
  (system-sleep-hook-files          elogind-system-sleep-hook-files
                                    (default '()))
  (system-shutdown-hook-files       elogind-system-shutdown-hook-files
                                    (default '()))
  (allow-power-off-interrupts?      elogind-allow-power-off-interrupts?
                                    (default #f))
  (allow-suspend-interrupts?        elogind-allow-suspend-interrupts?
                                    (default #f))
  (broadcast-power-off-interrupts?  elogind-broadcast-power-off-interrupts?
                                    (default #t))
  (broadcast-suspend-interrupts?    elogind-broadcast-suspend-interrupts?
                                    (default #t)))

(define (elogind-configuration-file config kind)
  (define (yesno x)
    (match x
      (#t "yes")
      (#f "no")
      (_ (error "expected #t or #f, instead got:" x))))
  (define char-set:user-name
    (string->char-set "abcdefghijklmnopqrstuvwxyz0123456789_-"))
  (define (valid-list? l pred)
    (and-map (lambda (x) (string-every pred x)) l))
  (define (user-name-list users)
    (unless (valid-list? users char-set:user-name)
      (error "invalid user list" users))
    (string-join users " "))
  (define (enum val allowed)
    (unless (memq val allowed)
      (error "invalid value" val allowed))
    (symbol->string val))
  (define (non-negative-integer x)
    (unless (exact-integer? x) (error "not an integer" x))
    (when (negative? x) (error "negative number not allowed" x))
    (number->string x))
  (define (maybe-non-negative-integer x)
    (or (and (unspecified? x) x)
        (non-negative-integer x)))
  (define handle-actions
    '(ignore poweroff reboot halt kexec suspend hibernate hybrid-sleep suspend-then-hibernate lock))
  (define (handle-action x)
    (if (unspecified? x)
        x                               ;let the unspecified value go through
        (enum x handle-actions)))
  (define (sleep-list tokens)
    (unless (valid-list? tokens char-set:user-name)
      (error "invalid sleep list" tokens))
    (string-join tokens " "))
  (define-syntax ini-file-clause
    (syntax-rules ()
      ;; Produce an empty line when encountering an unspecified value.  This
      ;; is better than an empty string value, which can, in some cases, cause
      ;; warnings such as "Failed to parse handle action setting".
      ((_ config (prop (parser getter)))
       (let ((value (parser (getter config))))
         (if (unspecified? value)
             ""
             (string-append prop "=" value "\n"))))
      ((_ config str)
       (if (unspecified? str)
           ""
           (string-append str "\n")))))
  (define-syntax-rule (ini-file config file clause ...)
    (plain-file file (string-append (ini-file-clause config clause) ...)))
  (case kind
    ((#:logind)
     (ini-file
      config "logind.conf"
      "[Login]"
      ("KillUserProcesses" (yesno elogind-kill-user-processes?))
      ("KillOnlyUsers" (user-name-list elogind-kill-only-users))
      ("KillExcludeUsers" (user-name-list elogind-kill-exclude-users))
      ("InhibitDelayMaxSec" (non-negative-integer elogind-inhibit-delay-max-seconds))
      ("HandlePowerKey" (handle-action elogind-handle-power-key))
      ("HandleSuspendKey" (handle-action elogind-handle-suspend-key))
      ("HandleHibernateKey" (handle-action elogind-handle-hibernate-key))
      ("HandleLidSwitch" (handle-action elogind-handle-lid-switch))
      ("HandleLidSwitchDocked" (handle-action elogind-handle-lid-switch-docked))
      ("HandleLidSwitchExternalPower" (handle-action elogind-handle-lid-switch-external-power))
      ("PowerKeyIgnoreInhibited" (yesno elogind-power-key-ignore-inhibited?))
      ("SuspendKeyIgnoreInhibited" (yesno elogind-suspend-key-ignore-inhibited?))
      ("HibernateKeyIgnoreInhibited" (yesno elogind-hibernate-key-ignore-inhibited?))
      ("LidSwitchIgnoreInhibited" (yesno elogind-lid-switch-ignore-inhibited?))
      ("HoldoffTimeoutSec" (non-negative-integer elogind-holdoff-timeout-seconds))
      ("IdleAction" (handle-action elogind-idle-action))
      ("IdleActionSec" (non-negative-integer elogind-idle-action-seconds))
      ("RuntimeDirectorySize"
       (identity
        (lambda (config)
          (match (elogind-runtime-directory-size-percent config)
            (#f (non-negative-integer (elogind-runtime-directory-size config)))
            (percent (string-append (non-negative-integer percent) "%"))))))
      ("RemoveIPC" (yesno elogind-remove-ipc?))))
    ((#:sleep)
     (ini-file
      config "sleep.conf"
      "[Sleep]"
      ("SuspendState" (sleep-list elogind-suspend-state))
      ("SuspendMode" (sleep-list elogind-suspend-mode))
      ("HibernateState" (sleep-list elogind-hibernate-state))
      ("HibernateMode" (sleep-list elogind-hibernate-mode))
      ("HybridSleepState" (sleep-list elogind-hybrid-sleep-state))
      ("HybridSleepMode" (sleep-list elogind-hybrid-sleep-mode))
      ("HibernateDelaySec" (maybe-non-negative-integer elogind-hibernate-delay-seconds))
      ("SuspendEstimationSec" (maybe-non-negative-integer elogind-suspend-estimation-seconds))
      ("AllowPowerOffInterrupts" (yesno elogind-allow-power-off-interrupts?))
      ("AllowSuspendInterrupts" (yesno elogind-allow-suspend-interrupts?))
      ("BroadcastPowerOffInterrupts" (yesno elogind-broadcast-power-off-interrupts?))
      ("BroadcastSuspendInterrupts" (yesno elogind-broadcast-suspend-interrupts?))))))

(define (elogind-etc-directory config)
  "Return the /etc/elogind directory for CONFIG."
  (define logind-config-file
    (elogind-configuration-file config #:logind))

  (define sleep-config-file
    (elogind-configuration-file config #:sleep))

  (with-imported-modules (source-module-closure '((guix build utils)))
    (computed-file
     "etc-elogind"

     #~(begin
         (use-modules (guix build utils))

         (define sleep-directory (string-append #$output "/system-sleep/"))
         (define shutdown-directory (string-append #$output "/system-shutdown/"))

         (define (copy-script file directory)
           "Copy FILE into DIRECTORY, giving rx (500) permissions."
           (let ((dest (string-append directory "/" (basename file))))
             (mkdir-p directory)
             (copy-file file dest)
             (chmod dest #o500)))

         (mkdir-p #$output)            ;in case neither directory gets created
         (for-each (lambda (f)
                     (copy-script f sleep-directory))
                   '#$(elogind-system-sleep-hook-files config))
         (for-each (lambda (f)
                     (copy-script f shutdown-directory))
                   '#$(elogind-system-shutdown-hook-files config))

         (mkdir-p (string-append #$output "/logind.conf.d/"))
         (copy-file #$logind-config-file (string-append #$output "/logind.conf.d/10-elogind.conf"))

         (mkdir-p (string-append #$output "/sleep.conf.d/"))
         (copy-file #$sleep-config-file (string-append #$output "/sleep.conf.d/10-elogind.conf"))))))

(define (elogind-dbus-service config)
  "Return a @file{org.freedesktop.login1.service} file that tells D-Bus how to
\"start\" elogind.  In practice though, our elogind is started when booting by
shepherd.  Thus, the @code{Exec} line of this @file{.service} file does not
explain how to start elogind; instead, it spawns a wrapper that waits for the
@code{elogind} shepherd service.  This avoids a race condition where both
@command{shepherd} and @command{dbus-daemon} would attempt to start elogind."
  ;; For more info on the elogind startup race, see
  ;; <https://issues.guix.gnu.org/55444>.

  (define elogind
    (elogind-package config))

  (define wrapper
    (program-file "elogind-dbus-shepherd-sync"
                  (with-imported-modules '((gnu services herd))
                    #~(begin
                        (use-modules (gnu services herd)
                                     (srfi srfi-34))

                        (guard (c ((service-not-found-error? c)
                                   (format (current-error-port)
                                           "no elogind shepherd service~%")
                                   (exit 1))
                                  ((shepherd-error? c)
                                   (format (current-error-port)
                                           "elogind shepherd service not \
started~%")
                                   (exit 2)))
                          (wait-for-service 'elogind))))))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define service-directory
            "/share/dbus-1/system-services")

          (mkdir-p (dirname (string-append #$output service-directory)))
          (copy-recursively (string-append #$elogind service-directory)
                            (string-append #$output service-directory))
          (symlink (string-append #$elogind "/etc") ;for etc/dbus-1
                   (string-append #$output "/etc"))
          ;; Also expose the D-Bus policy configurations (.conf) files, now
          ;; installed under '/share' instead of the legacy '/etc' prefix.
          (symlink (string-append #$elogind "/share/dbus-1/system.d")
                   (string-append #$output "/share/dbus-1/system.d"))

          ;; Replace the "Exec=" line of the 'org.freedesktop.login1.service'
          ;; file with one that refers to WRAPPER instead of elogind.
          (match (find-files #$output "\\.service$")
            ((file)
             (substitute* file
               (("Exec[[:blank:]]*=.*" _)
                (string-append "Exec=" #$wrapper "\n"))))))))

  (list (computed-file "elogind-dbus-service-wrapper" build)))

(define (pam-extension-procedure config)
  "Return an extension for PAM-ROOT-SERVICE-TYPE that ensures that all the PAM
services use 'pam_elogind.so', a module that allows elogind to keep track of
logged-in users (run 'loginctl' to see elogind's world view of users and
seats.)"
  (define pam-elogind
    (pam-entry
     (control "required")
     (module (file-append (elogind-package config)
                          "/lib/security/pam_elogind.so"))))

  (list (pam-extension
         (transformer
          (lambda (pam)
            (pam-service
             (inherit pam)
             (session (cons pam-elogind (pam-service-session pam))))))
         (shepherd-requirements '(elogind)))))

(define (elogind-shepherd-service config)
  "Return a Shepherd service to start elogind according to @var{config}."
  (list (shepherd-service
         (requirement '(user-processes dbus-system))
         (provision '(elogind))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (elogind-package config)
                                        "/libexec/elogind/elogind"))
                   #:environment-variables
                   (list "ABCDE=123")))
         (stop #~(make-kill-destructor)))))

(define elogind-service-type
  (service-type (name 'elogind)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          elogind-dbus-service)
                       (service-extension udev-service-type
                                          (compose list elogind-package))
                       (service-extension polkit-service-type
                                          (compose list elogind-package))

                       ;; Start elogind from the Shepherd rather than waiting
                       ;; for bus activation.  This ensures that it can handle
                       ;; events like lid close, etc.
                       (service-extension shepherd-root-service-type
                                          elogind-shepherd-service)

                       ;; Provide the 'loginctl' command.
                       (service-extension profile-service-type
                                          (compose list elogind-package))

                       ;; Extend PAM with pam_elogind.so.
                       (service-extension pam-root-service-type
                                          pam-extension-procedure)

                       ;; Install sleep/shutdown hook files.
                       (service-extension etc-service-type
                                          (lambda (config)
                                            `(("elogind"
                                               ,(elogind-etc-directory config)))))

                       ;; We need /run/user, /run/systemd, etc.
                       (service-extension file-system-service-type
                                          (const %elogind-file-systems))))
                (default-value (elogind-configuration))
                (description "Run the @command{elogind} login and seat
management service.  The @command{elogind} service integrates with PAM to
allow other system components to know the set of logged-in users as well as
their session types (graphical, console, remote, etc.).  It can also clean up
after users when they log out.")))
