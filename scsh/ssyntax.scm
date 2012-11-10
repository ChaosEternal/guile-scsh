(define-module (scsh ssyntax)
  :export-syntax (define-simple-syntax))

(define-syntax define-simple-syntax
  (syntax-rules ()
    ((define-simple-syntax (name . pattern) result)
     (define-syntax name (syntax-rules () ((name . pattern) result))))))
