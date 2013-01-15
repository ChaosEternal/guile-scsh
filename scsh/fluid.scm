;; implementation of scheme48 fluid variables using libguile fluids.

(define-module (scsh fluid))

(export make-scsh-fluid set-scsh-fluid! scsh-fluid let-scsh-fluid)

(define guile-make-fluid
  (module-ref (resolve-module '(guile)) 'make-fluid))

(define (make-scsh-fluid value)
  (let ((result (guile-make-fluid)))
    (fluid-set! result value)
    result))

(define set-scsh-fluid! fluid-set!)

(define scsh-fluid fluid-ref)

(define (let-scsh-fluid fluid value thunk)
  (with-fluids* (list fluid) (list value) thunk))
