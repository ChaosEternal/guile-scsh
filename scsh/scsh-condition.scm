;;; Copyright (c) 1994 by Olin Shivers. See file COPYING.
;;; Add scsh conditions to s48.

(define-module (scsh scsh-condition)
  :use-module (ice-9 stack-catch)
  :use-module (scsh errno)
  :export (errno-error with-errno-handler* with-errno-handler))


;;; A syscall-error condition-type:
;;(define-condition-type 'syscall-error '(error))
;;(define syscall-error? (condition-predicate 'syscall-error))

(define (errno-error errno syscall . stuff)
  (let ((msg (errno-msg errno)))
    (scm-error 'system-error syscall "%s" msg (list errno))))

(define (with-errno-handler* handler thunk)
  (stack-catch 'system-error
	       thunk
	       (lambda (key subr msg msg-args rest)
		 (let ((errno (car rest)))
		   (handler errno (list msg
					subr
					'()))	; data
		   (throw key subr msg msg-args rest)))))


;;; (with-errno-handler
;;;   ((errno data) ; These are vars bound in this scope.
;;;    ((errno/exist) . body1)
;;;    ((errno/wouldblock errno/again) . body2)
;;;    (else . body3))
;;; 
;;;   . body)

(define-syntax with-errno-handler
  (syntax-rules ()
    ((_ ((err-var data-var) . clauses) . body)     
     (call/cc (lambda (ret)
                (with-errno-handler*
                 (e-step () ret clauses err-var data-var)
                 (lambda () . body)))))))

(define-syntax with-errno-handler+
  (syntax-rules ()
    ((_ (arms ...) err-var data-var)
     (lambda (err-var data-var)
       (cond arms ...)))))


(define-syntax e-step
  (syntax-rules (else)
    ((_ (r ...) ret ((else . x) . cs) . l)
     (e-step (r ... (else (call-with-values (lambda () . x) ret)))
             ret cs . l))

    ((_ (r ...) ret (((errs ...)    . x) . cs) err-var . l)
     (e-step (r ... ((or (= errs err-var) ...)
                     (call-with-values (lambda () . x) ret)))
             ret cs err-var . l))

    ((_ arms ret () . l)
     (with-errno-handler+ arms . l))))
