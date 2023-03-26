(define-module (rsauex script template)
  #:use-module ((guix gexp))
  #:use-module ((ice-9 match))
  #:use-module ((ice-9 popen))
  #:use-module ((ice-9 ports))
  #:use-module ((ice-9 textual-ports))
  #:use-module ((rsauex packages))
  #:use-module ((rsauex script template eval) #:prefix template-eval-module:)

  #:export (rsauex-home-template-file))

(define (make-list-collector)
  (let* ((result (cons #nil (list)))
         (list-end result))
    (lambda (value)
      (if (nil? value)
          (cdr result)
          (let ((new-list-end (cons value #nil)))
            (set-cdr! list-end new-list-end)
            (set! list-end new-list-end)
            #f)))))

(define (call-in-lexer thunk)
  (let* ((collector (make-list-collector)))
    (letrec ((%loop (lambda (thunk collector)
                      (let ((cont (call-with-prompt 'collect-lexeme
                                    (lambda _
                                      (thunk)
                                      (abort-to-prompt 'lexer-end))
                                    (lambda (cont lexeme)
                                      (collector lexeme)
                                      cont))))
                        (%loop cont collector)))))
      (call-with-prompt 'lexer-end
        (lambda _
          (%loop thunk collector))
        (lambda _
          (collector #nil))))))

(define (lexeme! type value)
  (abort-to-prompt 'collect-lexeme (cons type value)))

(define (char! char)
  (lexeme! #:char char))

(define (on-rbrace)
  (let ((next-char (read-char)))
    (cond
     ((char=? next-char #\#)
      #t)
     (#t
      (error "Expected #")))))

(define (read-expr-type)
  (case (read-char)
    ((#\#) #:expr-with-output)
    ((#\!) #:expr-sans-output)
    (else (error "Expected #, !, or $"))))

(define (read-scheme)
  (let ((expr-type (read-expr-type)))
    (lexeme! expr-type (read)))
  (while (char-set-contains? char-set:whitespace (peek-char))
    (read-char))
  (let ((next-char (read-char)))
    (cond
     ((char=? next-char #\})
      (on-rbrace))
     (#t
      (error "Expected }")))))

(define (on-sharp)
  (let ((next-char (read-char)))
    (cond
     ((char=? next-char #\{)
      (read-scheme))
     (#t
      (char! #\#)
      (char! next-char)))))

(define (port->lexemes port)
  (call-in-lexer
   (lambda _
     (with-input-from-port port
       (lambda _
         (let ((next-char (read-char)))
           (while (not (eof-object? next-char))
             (cond
              ((char=? next-char #\#)
               (on-sharp))
              (#t
               (char! next-char)))
             (set! next-char (read-char)))))))))

(define (string->lexemes str)
  (call-with-input-string str
    (lambda (port)
      (port->lexemes port))))

(define (lexemes->code-aux lexemes display-body-collector)
  (if (null? lexemes)
      `(gexp (begin ,@(display-body-collector #nil)))
      (match (car lexemes)
        (('#:char . char)
         (display-body-collector `(display ,char))
         (lexemes->code-aux (cdr lexemes) display-body-collector))
        (('#:expr-with-output . expr)
         (let ((sym (gensym "value-")))
           (display-body-collector `(display (ungexp ,sym)))
           `(let ((,sym ,expr))
              ,(lexemes->code-aux (cdr lexemes) display-body-collector))))
        (('#:expr-sans-output . expr)
         `(begin
            ,expr
            ,(lexemes->code-aux (cdr lexemes) display-body-collector))))))

(define (lexemes->code lexemes)
  `((lambda ()
      ,(lexemes->code-aux lexemes (make-list-collector)))))

(define (template-file->gexp file)
  (let* ((lexemes (call-with-input-file (search-rsauex-home-file file)
                    port->lexemes))
         (code (lexemes->code lexemes)))
    (eval code (resolve-module '(rsauex script template eval)))))

(define (rsauex-home-template-file file name)
  (define template-gexp
    #~(begin
        (use-modules (ice-9 popen)
                     (ice-9 textual-ports))
        (with-fluids ((%default-port-encoding "UTF-8"))
          (with-output-to-file #$output
            (lambda _
              #$(template-file->gexp file))))))
  (computed-file name template-gexp))
