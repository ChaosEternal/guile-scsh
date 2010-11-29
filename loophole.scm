; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.

;; original file: alt/loophole.scm.
(define-module (scsh loophole)
  :export (loophole))

(define-syntax loophole
  (syntax-rules ()
    ((loophole type form)
     (begin (lambda () type)    ;Elicit unbound-variable warnings, etc.
	    form))))
    
