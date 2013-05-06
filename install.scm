#!/usr/bin/guile
!#

(add-to-load-path (dirname (car (command-line))))
(use-modules (scsh syntax)
	     (scsh scsh)
	     (scsh glob))

(&& (test "!" -d ,(%site-dir)) 
 (mkdir -p ,(%site-dir)))

(run (cp -r "scsh" ,(%site-dir)))
