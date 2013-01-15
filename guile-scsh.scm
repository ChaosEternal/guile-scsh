#!/usr/bin/guile
-L .
!#
(use-modules (ice-9 readline))
(use-modules (ice-9 regex))
(use-modules (scsh syntax))
(use-modules (ice-9 buffered-input))
(activate-readline)

(define prmpt " >>> ")

(define (update-prmpt!)
  (set! prmpt
	(string-append (regexp-substitute #f 
			   (string-match 
			    (getenv "HOME") 
			    (getcwd))
			   'pre "~" 
			   'post)
		       " >>> "
		       )))

(update-prmpt!)  
(set-readline-prompt! prmpt "... ")
;;(read-and-eval!)
;;(run (cat ) (<< "hello world\n"))

(define $? 0)
(let loop ()
  (catch #t 
	 (lambda ()
	   (let ((e (read)))
	     (set-buffered-input-continuation?! (current-input-port ) #f) 
	     (update-prmpt!)
	     (set-readline-prompt! prmpt "... ")
	     (begin
	       (if (eof-object? e)
		   (quit)
		   (set! $?
		   (eval e (interaction-environment))))
	       )))
	 (lambda (key . args)
	   (if (eq? key 'quit)
	       (quit))
	   (display "key ")
	   (display key)
	   (display args)
	   (newline)))
  (loop))