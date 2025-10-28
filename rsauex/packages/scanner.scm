(define-module (rsauex packages scanner)
  #:use-module (gnu packages)
  #:use-module (gnu packages scanner)
  #:use-module (guix build-system gnu)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix utils))

(define-public sane-backends-fixed
  (package/inherit sane-backends
    (name "sane-backends-fixed")
    (arguments
     (substitute-keyword-arguments (package-arguments sane-backends)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'remove-dll.conf)))))))
