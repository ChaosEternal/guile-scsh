
(define-module (tests scsh-test-suite)
  #:use-module (test-suite lib)
  #:use-module (scsh scsh)
  #:use-module (scsh syntax)
  #:use-module (scsh glob)
  #:use-module (scsh procobj))

(with-test-prefix
  "scsh/scsh.scm"
  ;; (pass-if "call-terminally" call-terminally)
  ;; (pass-if "fork/pipe" fork/pipe)
  ;; (pass-if "%fork/pipe" %fork/pipe)
  ;; (pass-if "tail-pipe" tail-pipe)
  ;; (pass-if "tail-pipe+" tail-pipe+)
  ;; (pass-if "alist-update" alist-update)
  ;; (pass-if "alist-compress" alist-compress)
  ;; (pass-if "add-before" add-before)
  ;; (pass-if "add-after" add-after)
  ;; (pass-if "with-env*" with-env*)
  ;; (pass-if "with-total-env*" with-total-env*)
  ;; (pass-if "with-cwd*" with-cwd*)
  ;; (pass-if "with-umask*" with-umask*)
  ;; (pass-if "create-temp-file" create-temp-file)
  ;; (pass-if "temp-file-channel" temp-file-channel)
  (pass-if "call/temp-file" 
	   (eq? 0
		(call/temp-file 
		 (lambda () (display (run/string (ls)) ))
		 (lambda (t) (run (test -f ,t))))))
  ;; (pass-if "open-string-source" open-string-source)
  ;; (pass-if "run/port+proc*" run/port+proc*)
  ;; (pass-if "run/port*" run/port*)
  ;; (pass-if "run/string*" run/string*)
  ;; (pass-if "run/strings*" run/strings*)
  ;; (pass-if "run/sexp*" run/sexp*)
  ;; (pass-if "run/sexps*" run/sexps*)
  ;; (pass-if "port->string" port->string)
  ;; (pass-if "port->string-list" port->string-list)
  ;; (pass-if "port->sexp-list" port->sexp-list)
  ;; (pass-if "port->list" port->list)
  ;; (pass-if "port-fold" port-fold)
  ;; (pass-if "fork/pipe+" fork/pipe+)
  ;; (pass-if "char-filter" char-filter)
  ;; (pass-if "string-filter" string-filter)
  ;; (pass-if "y-or-n?" y-or-n?)
  ;; (pass-if "stdio->stdports" stdio->stdports)
  ;; (pass-if "with-stdio-ports*" with-stdio-ports*)
  ;; (pass-if "stdports->stdio" stdports->stdio)
  ;; (pass-if
  ;;   "command-line-arguments"
  ;;   command-line-arguments)
  ;; (pass-if "arg*" arg*)
  ;; (pass-if "arg" arg)
  ;; (pass-if "argv" argv)
  ;; (pass-if "home-directory" home-directory)
  ;; (pass-if "exec-path-list" exec-path-list)
  ;; (pass-if "suspend" suspend)
  ;; (pass-if "exec/env" exec/env)
  ;; (pass-if "exec-path/env" exec-path/env)
  ;; (pass-if "exec-path" exec-path)
  ;; (pass-if "exec" exec)
  ;; (pass-if "fork" fork)
  ;; (pass-if "%fork" %fork)
  (pass-if "stringify" 
	   (equal? "-1a!@#$%" 
		   (stringify '-1a!@#$%)))
  (pass-if "with-cwd" 
	   (equal? "/tmp" 
		   (with-cwd "/tmp" 
			     (car (run/strings (pwd))))))
  (pass-if "with-umask" (eq? 066 (with-umask 066 (umask))))
  (pass-if "with-env" (equal? "abcd" (with-env (("abcd" . "abcd"))
					       (getenv "abcd"))))
  (pass-if "with-total-env" 
	   (with-total-env 
	    (("abcd" . "abcd"))
	    (and (equal? "abcd" (getenv "abcd"))
		 (not (getenv "HOME"))))))
  ;; (pass-if "with-stdio-ports" with-stdio-ports))
(with-test-prefix
  "scsh/syntax.scm"
  ;; (pass-if "pwd" pwd)
  ;; (pass-if "cd" cd)
  ;; (pass-if "cd*" cd*)
  ;; (pass-if "exec-epf" exec-epf)
  (pass-if "&" (> (proc:pid (& (echo))) 0))
  (pass-if "run" (eq? 0 (run (echo) )))
  ;; (pass-if "||" ||)
  ;; (pass-if "&&" &&)
  (pass-if "run/collecting" 
	   (call-with-values (lambda () (run/collecting (1) (echo "abcd")))
	     (lambda (s p)
	       (and (eq? s 0)
		    (equal? "abcd" (car (port->string-list p)))))))
  ;; (pass-if "run/port+proc" run/port+proc)
  ;; (pass-if "run/port" run/port)
  (pass-if "run/strings" 
	   (equal? '("a" "b") 
		   (run/strings 
		    (begin (run (echo a)) (run (echo b))))))
  ;; (pass-if "run/file" run/file)
  ;; (pass-if "run/string" run/string)
 (pass-if "run/sexp"  (let ((s '(a b (c d))))
			(equal? s 
				(run/sexp (cat) (<< ,s))
				)))
 (pass-if "run/sexps" (let ((s '(a b (c d))))
			(equal? (list s s)
				(run/sexps (begin (run (cat) (<< ,s))
						  (run (cat) (<< ,s)))
					   )))))
(with-test-prefix
  "scsh/glob.scm"
  (pass-if "glob" 
	   (equal? "/bin/ls"
		   (car (glob /*/ls)))))
  ;; (pass-if "glob*" glob*)
  ;; (pass-if "directory-files" directory-files)
  ;; (pass-if "glob->regexp-list" glob->regexp-list))
