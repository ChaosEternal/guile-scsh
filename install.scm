#!/usr/bin/guile
!#

(add-to-load-path (dirname (car (command-line))))
(use-modules (scsh syntax)
	     (scsh scsh)
	     (scsh glob))

(&& (test "!" -d ,(%site-dir)) 
 (mkdir -p ,(%site-dir)))

(let ((scsh-lib-dir (string-append (%site-dir) "/scsh")))
  (&& (test -d ,scsh-lib-dir)
      (rm -fr ,scsh-lib-dir)))
(run (cp -r "scsh" ,(%site-dir)))

(let* ((scsh-ccache-dir (string-append (%site-ccache-dir) "/scsh"))
       (scsh-lib-dir (string-append (%site-dir) "/scsh"))
       (compile-lambda
	(lambda (fn)
	  (let* ((p (string-index-right fn #\.))
		 (fn-go (string-replace fn ".go" p))
		 (pn-go (basename fn-go))
		 (path-go (string-append scsh-ccache-dir "/" pn-go)))
	    (run (guild compile -o ,path-go ,fn))))))
  (begin (&& (test -d ,scsh-ccache-dir)
	     (rm -fr ,scsh-ccache-dir)
	     (mkdir -p ,scsh-ccache-dir)
	     )
	 (for-each compile-lambda 
		   (glob ,(string-append scsh-lib-dir "/*")))
	 (for-each compile-lambda
		   (glob ,(string-append scsh-lib-dir "/syntax-helpers.scm")))))
