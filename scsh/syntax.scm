;;; Syntax definitions for scsh.
;;; Translating process forms into Scheme code.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

(define-module (scsh syntax)
  :use-module (scsh ssyntax)
  :use-module (scsh syntax-helpers)
  :use-module (scsh scsh)
  :use-module (scsh procobj)  
;  :use-module (scsh run-extras)
  :export-syntax (pwd cd cd* exec-epf & run || && run/collecting
                   run/port+proc run/port run/strings run/file run/string
                   run/sexp run/sexps))



;;; The three basic forms for running an extended process form:
;;; EXEC-EPF, &, and RUN. EXEC-EPF is the foundation.

(define-syntax exec-epf
  (syntax-rules ()
    ((_ . x)
     (transcribe-extended-process-form . x))))


(define-simple-syntax (& . epf)
  (fork (lambda () (exec-epf . epf))))

(define-simple-syntax (run . epf)
  (wait (& . epf)))

;;; Sequencing operators:
;;;
;;; (|| pf1 ... pfn)
;;;     Run each proc until one completes successfully (i.e., exit status 0).
;;;     Return true if some proc completes successfully; otherwise #f.
;;;
;;; (&& pf1 ... pfn) 
;;;     Run each proc until one fails (i.e., exit status non-0).
;;;     Return true if all procs complete successfully; otherwise #f.

;;; WARNING: || is not a readable symbol in R4RS.

(define-simple-syntax (|| pf ...) (or  (zero? (run pf)) ...))
(define-simple-syntax (:or: pf ...) (or  (zero? (run pf)) ...))
(define-simple-syntax (&& pf ...) (and (zero? (run pf)) ...))

(define-simple-syntax (run/collecting fds . epf)
  (run/collecting* `fds (lambda () (exec-epf . epf))))

(define-simple-syntax (run/port+proc . epf)
  (run/port+proc* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/port . epf)
  (run/port* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/strings . epf)
  (run/strings* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/file . epf)
  (run/file* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/string . epf)
  (run/string* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/sexp . epf)
  (run/sexp* (lambda () (exec-epf . epf))))

(define-simple-syntax (run/sexps . epf)
  (run/sexps* (lambda () (exec-epf . epf))))

;(define-simple-syntax (run/pty . epf)
;  (run/pty* (lambda () (exec-epf . epf))))

;(define (expand-mac transformer form)
;  (transformer form (lambda (x) x) eq?))

;(define-simple-syntax (test-mac trans . form)
;  (pp (expand-mac trans (quote form))))

(define-syntax cd
  (syntax-rules ()
    ((_) (cd* (getenv "HOME")))
    ((_ p ...) (apply cd* `(p ...)))))

(define (cd* . p)
  (let ((cwd (getcwd))
	(oldcwd (getenv "OLDPWD")))
    (setenv "OLDPWD" cwd)
    (let ((destdir (car p)))
      (if (eq? destdir '-)
	  (chdir oldcwd)
	  (chdir (stringify destdir))))))


(define (pwd)
  (run (pwd)))

  