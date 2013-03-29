;;; A Scheme shell.
;;; Copyright (c) 1992-1999 by Olin Shivers.
;;; Copyright (c) 1994 by Brian D. Carlstrom.
;;; See file COPYING.

(define-module (scsh scsh)
  :use-module (ice-9 receive)
  :use-module (ice-9 format)
  :use-module (ice-9 optargs)
  :use-module (scsh optional)
  :use-module (srfi srfi-1)
  :use-module (scsh utilities)
  :use-module (scsh ssyntax)
  :use-module (scsh syntax)
  :use-module (scsh syscalls)
  :use-module (scsh bitwise)
  :use-module (scsh fluid)
  :use-module (scsh newports)
  :use-module (scsh scsh-condition)
  :use-module (scsh stringcoll)
  :use-module (scsh fileinfo)
  :use-module (scsh fname)
  :use-module (scsh rw)
  :use-module (scsh rdelim)
  :use-module (scsh sighandlers)
  :use-module (scsh procobj)
  :use-module (scsh errno)
  :export (call-terminally fork/pipe %fork/pipe tail-pipe tail-pipe+
                           alist-update alist-compress add-before add-after
                           with-env* with-total-env* with-cwd* with-umask*
                           create-temp-file temp-file-channel call/temp-file
                           open-string-source run/port+proc*
                           run/port* run/string* run/strings* 
                           run/sexp* run/sexps*
                           port->string port->string-list port->sexp-list
                           port->list port-fold fork/pipe+
                           char-filter string-filter y-or-n?
                           stdio->stdports with-stdio-ports* stdports->stdio
                           command-line-arguments arg* arg argv
                           home-directory exec-path-list suspend
                           exec/env exec-path/env exec-path exec
                           fork %fork stringify)
  :export-syntax (with-cwd with-umask with-env with-total-env
                           with-stdio-ports))


;;; Call THUNK, then die.
;;; A clever definition in a clever implementation allows the caller's stack
;;; and dynamic env to be gc'd away, since this procedure never returns.

;;; (define (call-terminally thunk)
;;;  (with-continuation (loophole :escape #f)	; Bogus
;;;		     (lambda () (thunk) (exit 0))))
;;;  ;; Alternatively: (with-continuation #f thunk)

;;; More portably, but less usefully:
(define (call-terminally thunk)
  (catch #t
        (lambda () (thunk))
        (lambda (key . args)
          (apply display-error #f
                         (current-error-port)
                         args
                         )
          (primitive-exit 111)))
  (primitive-exit 0))

;;; Like FORK, but the parent and child communicate via a pipe connecting
;;; the parent's stdin to the child's stdout. This function side-effects
;;; the parent by changing his stdin.

(define (fork/pipe . maybe-thunk)
  (really-fork/pipe fork maybe-thunk))

(define (%fork/pipe . maybe-thunk)
  (really-fork/pipe %fork maybe-thunk))
  
;;; Common code for FORK/PIPE and %FORK/PIPE.
(define (really-fork/pipe forker maybe-thunk)
  (receive (r w) (vpipe)
    (let ((proc (forker)))
      (cond (proc		; Parent
	     (close w)
	     (move->fdes r 0))
	    (else		; Child
	     (close r)
	     (move->fdes w 1)
	     (if (pair? maybe-thunk)
		 (call-terminally (car maybe-thunk)))))
      proc)))


;;; FORK/PIPE with a connection list.
;;; (FORK/PIPE . m-t) = (apply fork/pipe+ '((1 0)) m-t)

(define (%fork/pipe+ conns . maybe-thunk)
  (really-fork/pipe+ %fork conns maybe-thunk))

(define (fork/pipe+ conns . maybe-thunk)
  (really-fork/pipe+ fork conns maybe-thunk))

;;; Common code.
(define (really-fork/pipe+ forker conns maybe-thunk)
  (let* ((pipes (map (lambda (conn) (call-with-values vpipe cons))
		     conns))
	 (rev-conns (map reverse conns))
	 (froms (map (lambda (conn) (reverse (cdr conn)))
		     rev-conns))
	 (tos (map car rev-conns)))

    (let ((proc (forker)))
      (cond (proc			; Parent
	     (for-each (lambda (to r/w)
			 (let ((w (cdr r/w))
			       (r (car r/w)))
			   (close w)
			   (move->fdes r to)))
		       tos pipes))

	    (else		; Child
	     (for-each (lambda (from r/w)
			 (let ((r (car r/w))
			       (w (cdr r/w)))
			   (close r)
			   (for-each (lambda (fd) (dup w fd)) from)
			   (close w))) ; Unrevealed ports win.
		       froms pipes)
	     (if (pair? maybe-thunk)
		 (call-terminally (car maybe-thunk)))))
      proc)))

(define (tail-pipe a b)
  (fork/pipe a)
  (call-terminally b))

(define (tail-pipe+ conns a b)
  (fork/pipe+ conns a)
  (call-terminally b))

;;; Lay a pipeline, one process for each thunk. Last thunk is called
;;; in this process. PIPE* never returns.

(define (pipe* . thunks)
  (letrec ((lay-pipe (lambda (thunks)
		       (let ((thunk (car thunks))
			     (thunks (cdr thunks)))
			 (if (pair? thunks)
			     (begin (fork/pipe thunk)
				    (lay-pipe thunks))
			     (call-terminally thunk)))))) ; Last one.
    (if (pair? thunks)
	(lay-pipe thunks)
	(error "No thunks passed to PIPE*"))))

;;; Splice the processes into the i/o flow upstream from us.
;;; First thunk's process reads from our stdin; last thunk's process'
;;; output becomes our new stdin. Essentially, n-ary fork/pipe.
;;;
;;; This procedure is so trivial it isn't included.
;;; (define (pipe-splice . thunks) (for-each fork/pipe thunks))



;;; Environment stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These two functions are obsoleted by the more general INFIX-SPLITTER and
;;; JOIN-STRINGS functions. However, we keep SPLIT-COLON-LIST defined
;;; internally so the top-level startup code (INIT-SCSH) can use it
;;; to split up $PATH without requiring the field-splitter or regexp code.

(define (split-colon-list clist)
  (let ((len (string-length clist)))
    (if (= 0 len) '()			; Special case "" -> ().

	;; Main loop.
	(let split ((i 0))
	  (cond ((string-index clist #\: i) =>
		 (lambda (colon)
		   (cons (substring clist i colon)
			 (split (+ colon 1)))))
		(else (list (substring clist i len))))))))

;;; Unix colon lists typically use colons as separators, which
;;; is not as clean to deal with as terminators, but that's Unix.
;;; Note ambiguity: (s-l->c-l '()) = (s-l->c-l '("")) = "".

; (define (string-list->colon-list slist)
;   (if (pair? slist)
;       (apply string-append
; 	     (let colonise ((lis slist))	; LIS is always
; 	       (let ((tail (cdr lis))) 		; a pair.
; 		 (cons (car lis)
; 		       (if (pair? tail)
; 			   (cons ":" (colonise tail))
; 			   '())))))
;       ""))	; () case.


(define (alist-delete key alist)
  (filter (lambda (key/val) (not (equal? key (car key/val)))) alist))

(define (alist-update key val alist)
  (cons (cons key val)
	(alist-delete key alist)))

;;; Remove shadowed entries from ALIST. Preserves element order.
;;; (This version shares no structure.)

(define (alist-compress alist) 
  (reverse (let compress ((alist alist) (ans '()))
	     (if (pair? alist)
		 (let ((key/val (car alist))
		       (alist (cdr alist)))
		   (compress alist (if (assoc (car key/val) ans) ans
				       (cons key/val ans))))
		 ans))))

;; Tail-recursive loops suck.
;; (define (alist-compress alist)
;;   (loop (initial (ans '()))
;;	   (for key/val in alist)
;;   
;;	   (when (not (assoc (car key/val) ans)))
;;	   (next (ans (cons key/val ans)))
;;   
;;	   (result (reverse ans))))

(define (add-before elt before list)
  (let rec ((list list))
    (if (pair? list)
	(let ((x (car list)))
	  (if (equal? x before)
	      (cons elt list)
	      (cons x (rec (cdr list)))))
	(cons elt list))))

;;; In ADD-AFTER, the labelled LET adds ELT after the last occurrence of AFTER
;;; in LIST, and returns the list. However, if the LET finds no occurrence 
;;; of AFTER in LIST, it returns #F instead.

(define (add-after elt after list)
  (or (let rec ((list list))
	(if (pair? list)
	    (let* ((x (car list))
		   (tail (cdr list))
		   (ans (rec tail))) ; #f if AFTER wasn't encountered.
	      (cond (ans (cons x ans))
		    ((equal? x after)
		     (cons x (cons elt tail)))
		    (else #f)))		; AFTER doesn't appear in LIST.
	    #f))			; AFTER doesn't appear in LIST.
      (cons elt list))) 

;;; Or, just say...
;;; (reverse (add-before elt after (reverse list)))

(define (with-env* alist-delta thunk)
  (let* ((old-env #f)
	 (new-env (fold (lambda (key/val alist)
			  (alist-update (car key/val) (cdr key/val) alist))
			(env->alist)
			alist-delta)))
    (dynamic-wind
      (lambda ()
	(set! old-env (env->alist))
	(alist->env new-env))
      thunk
      (lambda ()
	(set! new-env (env->alist))
	(alist->env old-env)))))

(define (with-total-env* alist thunk)
  (let ((old-env (env->alist)))
    (dynamic-wind
      (lambda ()
	(set! old-env (env->alist))
	(alist->env alist))
      thunk
      (lambda ()
	(set! alist (env->alist))
	(alist->env old-env)))))


(define (with-cwd* dir thunk)
  (let ((old-wd #f))
    (dynamic-wind
      (lambda ()
	(set! old-wd (cwd))
	(chdir dir))
      thunk
      (lambda ()
	(set! dir (cwd))
	(chdir old-wd)))))

(define (with-umask* mask thunk)
  (let ((old-mask #f))
    (dynamic-wind
      (lambda ()
	(set! old-mask (umask))
	(set-umask mask))
      thunk
      (lambda ()
	(set! mask (umask))
	(set-umask old-mask)))))

;;; Sugar:

(define-simple-syntax (with-cwd dir . body)
  (with-cwd* dir (lambda () . body)))

(define-simple-syntax (with-umask mask . body)
  (with-umask* mask (lambda () . body)))

(define-simple-syntax (with-env delta . body)
  (with-env* `delta (lambda () . body)))

(define-simple-syntax (with-total-env env . body)
  (with-total-env* `env (lambda () . body)))

(define (call/temp-file writer user)
  (let ((fname #f))
    (dynamic-wind
      (lambda () (if fname (error "Can't wind back into a CALL/TEMP-FILE")
		     (set! fname (create-temp-file))))
      (lambda ()
	(with-output-to-file fname writer)
	(user fname))
      (lambda () (if fname (delete-file fname))))))

;;; Create a new temporary file and return its name.
;;; The optional argument specifies the filename prefix to use, and defaults
;;; to "/usr/tmp/<pid>.", where <pid> is the current process' id. The procedure
;;; scans through the files named <prefix>0, <prefix>1, ... until it finds a
;;; filename that doesn't exist in the filesystem. It creates the file with 
;;; permission #o600, and returns the filename.
;;; 

(define (create-temp-file . maybe-prefix)
  (let ((oflags (bitwise-ior open/write
			     (bitwise-ior open/create open/exclusive))))
    (apply temp-file-iterate
	   (lambda (fname)
	     (close-fdes (open-fdes fname oflags #o600))
	     fname)
	   (if (null? maybe-prefix) '()
	       (list (string-append (constant-format-string (car maybe-prefix))
				    ".~a"))))))


(define *temp-file-template*
  (make-scsh-fluid (string-append "/tmp/" (number->string (pid)) ".~a")))


(define (temp-file-iterate maker . maybe-template)
  (let ((template (:optional maybe-template (scsh-fluid *temp-file-template*))))
    (let loop ((i 0))
      (if (> i 10000) (error "Can't create temp-file")
	  (let ((fname (format #f template (number->string (random 100000 (random-state-from-platform))))))
	    (receive retvals (with-errno-handler
			       ((errno data)
				((errno/exist) #f))
			       (maker fname))
	      (if (car retvals) (apply values retvals)
		  (loop (+ i 1)))))))))


;; Double tildes in S. 
;; Using the return value as a format string will output exactly S.
(define (constant-format-string s)	; Ugly code. Would be much clearer
  (let* ((len (string-length s))	; if written with string SRFI.
	 (tilde? (lambda (s i) (char=? #\~ (string-ref s i))))
	 (newlen (do ((i (- len 1) (- i 1))
		      (ans 0 (+ ans (if (tilde? s i) 2 1))))
		     ((< i 0) ans)))
	 (fs (make-string newlen)))
    (let lp ((i 0) (j 0))
      (cond ((< i len)
	     (let ((j (cond ((tilde? s i) (string-set! fs j #\~) (+ j 1))
			    (else j))))
	       (string-set! fs j (string-ref s i))
	       (lp (+ i 1) (+ j 1))))))
    fs))


;;; Roughly equivalent to (pipe).
;;; Returns two file ports [iport oport] open on a temp file.
;;; Use this when you may have to buffer large quantities between
;;; writing and reading. Note that if the consumer gets ahead of the
;;; producer, it won't hang waiting for input, it will just return
;;; EOF. To play it safe, make sure that the producer runs to completion
;;; before starting the consumer.
;;;
;;; The temp file is deleted before TEMP-FILE-CHANNEL returns, so as soon
;;; as the ports are closed, the file's disk storage is reclaimed.

(define (temp-file-channel)
  (let* ((fname (create-temp-file))
	 (iport (open-input-file fname))
	 (oport (open-output-file fname)))
    (delete-file fname)
    (values iport oport)))
    

;; Return a Unix port such that reads on it get the chars produced by
;; DISPLAYing OBJ. For example, if OBJ is a string, then reading from
;; the port produces the characters of OBJ.
;; 
;; This implementation works by writing the string out to a temp file,
;; but that isn't necessary. It could work, for example, by forking off a 
;; writer process that outputs to a pipe, i.e.,
;;     (run/port (begin (display obj (fdes->outport 1))))

(define (open-string-source obj)
  (receive (inp outp) (temp-file-channel)
    (display obj outp)
    (close-output-port outp)
    inp))


;;;; Process->Scheme interface forms: run/collecting, run/port, run/string, ...

;;; (run/collecting FDS . EPF)
;;; --------------------------
;;; RUN/COLLECTING and RUN/COLLECTING* run processes that produce multiple
;;; output streams and return ports open on these streams.
;;;
;;; To avoid issues of deadlock, RUN/COLLECTING first runs the process
;;; with output to temp files, then returns the ports open on the temp files.
;;;
;;; (run/collecting (1 2) (ls))
;;; runs ls with stdout (fd 1) and stderr (fd 2) redirected to temporary files.
;;; When ls is done, RUN/COLLECTING returns two ports open on the temporary
;;; files. The files are deleted before RUN/COLLECTING returns, so when
;;; the ports are closed, they vanish.
;;;
;;; The FDS list of file descriptors is implicitly backquoted.
;;;
;;; RUN/COLLECTING* is the procedural abstraction of RUN/COLLECTING.


;;; Single-stream collectors:
;;; Syntax: run/port, run/file, run/string, run/strings, run/sexp, run/sexps
;;; Procedures: run/port*, run/file*, run/string*, run/strings*, run/sexp*,
;;;             run/sexps*
;;;             port->string, port->string-list, port->sexp-list, 
;;;             port->list
;;; 
;;; Syntax:
;;; (run/port . epf)
;;; 	Fork off the process EPF and return a port on its stdout.
;;; (run/file . epf)
;;; 	Run process EPF with stdout redirected into a temp file.
;;;     When the process exits, return the name of the file.
;;; (run/string . epf)
;;;     Read the process' stdout into a string and return it.
;;; (run/strings . epf)
;;; 	Run process EPF, reading newline-terminated strings from its stdout
;;;     until EOF. After process exits, return list of strings read. Delimiting
;;;	newlines are trimmed from the strings.
;;; (run/sexp . epf)
;;;     Run process EPF, read and return one sexp from its stdout with READ.
;;; (run/sexps . epf)
;;;     Run process EPF, read sexps from its stdout with READ until EOF.
;;;	After process exits, return list of items read.
;;;
;;; Procedural abstractions:
;;; run/port*, run/file*, run/string*, run/strings*, run/sexp*, run/sexps*
;;;
;;; These are all procedural equivalents for the macros. They all take
;;; one argument: the process to be executed passed as a thunk. For example,
;;; (RUN/PORT . epf) expands into (RUN/PORT* (LAMBDA () (EXEC-EPF . epf)))
;;;
;;; Other useful procedures:
;;; 
;;; (port->string port) 
;;; 	Read characters from port until EOF; return string collected.
;;; (port->string-list port)
;;;     Read newline-terminated strings from port until EOF. Return
;;;     the list of strings collected.
;;; (port->sexp-list port)
;;;     Read sexps from port with READ until EOF. Return list of items read.
;;; (port->list reader port)
;;;     Repeatedly applies READER to PORT, accumulating results into a list.
;;;     On EOF, returns the list of items thus collected.
;;; (port-fold port reader op . seeds)
;;;     Repeatedly read things from PORT with READER. Each time you read
;;;     some value V, compute a new set of seeds with (apply OP V SEEDS).
;;;     (More than 1 seed means OP must return multiple values).
;;;     On eof, return the seeds: (apply value SEEDS).
;;;     PORT->LIST is just (PORT-FOLD PORT READ CONS '())

(define (run/port+proc* thunk)
  (receive (r w) (vpipe)
    (let ((proc (fork (lambda ()
			(close r)
			(move->fdes w 1)
			(with-current-output-port* w thunk)))))
      (close w)
      (values r proc))))

(define (run/port* thunk)
  (receive (port proc) (run/port+proc* thunk)
    port))

(define (run/string* thunk) 
  (close-after (run/port* thunk) port->string))

(define (run/sexp* thunk)
  (close-after (run/port* thunk) read))

(define (run/sexps* thunk)
  (close-after (run/port* thunk) port->sexp-list))

(define (run/strings* thunk)
  (close-after (run/port* thunk) port->string-list))


;;; Read characters from PORT until EOF, collect into a string.

(define (port->string port)
  (let ((sc (make-string-collector)))
    (letrec ((lp (lambda ()
		   (cond ((read-string 1024 port) =>
			  (lambda (s)
			    (collect-string! sc s)
			    (lp)))
			 (else (string-collector->string sc))))))
      (lp))))

;;; (loop (initial (sc (make-string-collector)))
;;;       (bind (s (read-string 1024 port)))
;;;       (while s)
;;;       (do (collect-string! sc s))
;;;       (result (string-collector->string sc)))

;;; Read items from PORT with READER until EOF. Collect items into a list.

(define (port->list reader port)
  (let lp ((ans '()))
    (let ((x (reader port)))
      (if (eof-object? x) (reverse! ans)
	  (lp (cons x ans))))))

(define (port->sexp-list port)
  (port->list read port))

(define (port->string-list port)
  (port->list read-line port))

(define (port-fold port reader op . seeds)
  (letrec ((fold (lambda seeds
		     (let ((x (reader port)))
		       (if (eof-object? x) (apply values seeds)
			   (call-with-values (lambda () (apply op x seeds))
					     fold))))))
    (apply fold seeds)))

(define reduce-port
  (deprecated-proc port-fold 'reduce-port "Use port-fold instead."))

;;; Not defined:
;;; (field-reader field-delims record-delims)
;;; Returns a reader that reads strings delimited by 1 or more chars from
;;; the string FIELD-DELIMS. These strings are collected in a list until
;;; eof or until 1 or more chars from RECORD-DELIMS are read. Then the
;;; accumulated list of strings is returned. For example, if we want
;;; a procedure that reads one line of input, splitting it into 
;;; whitespace-delimited strings, we can use 
;;;     (field-reader " \t" "\n")
;;; for a reader.



;; Loop until EOF reading characters or strings and writing (FILTER char)
;; or (FILTER string). Useful as an arg to FORK or FORK/PIPE.

(define (char-filter filter)
  (lambda ()
    (let lp ()
      (let ((c (read-char)))
	(if (not (eof-object? c))
	    (begin (write-char (filter c))
		   (lp)))))))

(define (string-filter filter . maybe-buflen)
  (let* ((buflen (:optional maybe-buflen 1024))
	 (buf (make-string buflen)))
    (lambda ()
      (let lp ()
	(cond ((read-string! buf 0 buflen) =>
	       (lambda (nread)
		 (display (filter (if (= nread buflen) buf
				      (substring buf 0 nread)))) ; last one.
		 (lp))))))))

(define (y-or-n? question . maybe-eof-value)
  (let loop ((count *y-or-n-eof-count*))
    (display question)
    (display " (y/n)? ")
    (let ((line (read-line)))
      (cond ((eof-object? line)
	     (newline)
	     (if (= count 0)
		 (:optional maybe-eof-value (error "EOF in y-or-n?"))
		 (begin (display "I'll only ask another ")
			(write count)
			(display " times.")
			(newline)
			(loop (- count 1)))))
	    ((< (string-length line) 1) (loop count))
	    ((char=? (string-ref line 0) #\y) #t)
	    ((char=? (string-ref line 0) #\n) #f)
	    (else (loop count))))))

(define *y-or-n-eof-count* 100)


;;; Stdio/stdport sync procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stdio->stdports)
  (set-current-input-port!  (fdes->inport 0))
  (set-current-output-port! (fdes->outport 1))
  (set-error-output-port!   (fdes->outport 2)))

(define (with-stdio-ports* thunk)
  (with-current-input-port (fdes->inport 0)
    (with-current-output-port (fdes->outport 1)
      (with-error-output-port (fdes->outport 2)
	(thunk)))))

(define-simple-syntax (with-stdio-ports body ...)
  (with-stdio-ports* (lambda () body ...)))


(define (stdports->stdio)
  (dup (current-input-port)  0)
  (dup (current-output-port) 1)
  (dup (error-output-port)   2))


;;; Command-line argument access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some globals.
;;(define %command-line '())		; Includes program.
(define command-line-arguments 	; Doesn't include program.
  (cdr (command-line)))

;; not implemented in Guile.
;;(define (set-command-line-args! args)
;;  (set! %command-line args)
;;  (set! command-line-arguments (append (cdr args) '())))

(define (arg* arglist n . maybe-default-thunk)
  (let ((oops (lambda () (error "argument out of bounds" arglist n))))
    (if (< n 1) (oops)
	(let lp ((al arglist) (n n))
	  (if (pair? al)
	      (if (= n 1) (car al)
		  (lp (cdr al) (- n 1)))
	      (if (and (pair? maybe-default-thunk)
		       (null? (cdr maybe-default-thunk)))
		  ((car maybe-default-thunk))
		  (oops)))))))

(define (arg arglist n . maybe-default)
  (if (pair? maybe-default) (arg* arglist n (lambda () (car maybe-default)))
      (arg* arglist n)))

(define (argv n . maybe-default)
  (apply arg (cdr (command-line)) n maybe-default))

;; Guile primitive.
;;(define (command-line) (append %command-line '()))

;;; EXEC support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assumes a low-level %exec procedure:
;;; (%exec prog arglist env)
;;;   ENV is either #t, meaning the current environment, or a string->string
;;;       alist.
;;;   %EXEC stringifies PROG and the elements of ARGLIST.

(define (stringify thing)
  (cond ((string? thing) thing)
	((symbol? thing)
	 (symbol->string thing))
;	((symbol? thing)
;	 (list->string (map char-downcase
;			    (string->list (symbol->string thing)))))
	((integer? thing)
	 (number->string thing))
	(else (error "Can only stringify strings, symbols, and integers."
		     thing))))

(define (exec-path-search prog path-list)
  (if (file-name-absolute? prog)
      (and (file-executable? prog) prog)
      (first? (lambda (dir)
		(let ((fname (string-append dir "/" prog)))
		  (and (file-executable? fname) fname)))
	     path-list)))
		    
(define (exec/env prog env . arglist)
  (flush-all-ports)
  (%exec prog (cons prog arglist) env))

;(define (exec-path/env prog env . arglist)
;  (cond ((exec-path-search (stringify prog) (scsh-fluid exec-path-list)) =>
;	 (lambda (binary)
;	   (apply exec/env binary env arglist)))
;	(else (error "No executable found." prog arglist))))

;;; This procedure is bummed by tying in directly to %%exec/errno
;;; and pulling some of %exec's code out of the inner loop so that
;;; the inner loop will be fast. Folks don't like waiting...

(define (exec-path/env prog env . arglist)
  (flush-all-ports)
  (let ((prog (stringify prog)))
    (if (string-index prog #\/)

	;; Contains a slash -- no path search.
	(%exec prog (cons prog (map stringify arglist)) env)

	;; Try each directory in PATH-LIST.
	(let ((arglist (cons prog (map stringify arglist))))
	  (for-each (lambda (dir)
		      (let ((binary (string-append dir "/" prog)))
			(false-if-exception (%exec binary arglist env))))
		    (scsh-fluid exec-path-list)))))

    (error "No executable found." prog arglist))
	 
(define (exec-path prog . arglist)
  (apply exec-path/env prog #t arglist))

(define (exec prog . arglist)
  (apply exec/env prog #t arglist))


;;; Assumes niladic primitive %%FORK.

(define (fork . maybe-thunk)
  (flush-all-ports)
  (really-fork #t maybe-thunk))

(define (%fork . maybe-thunk)
  (really-fork #f maybe-thunk))

(define (really-fork clear-interactive? maybe-thunk)
  ((with-enabled-interrupts 0
     (let ((pid (%%fork)))
       (if (zero? pid)				

	   ;; Child
	   (lambda ()	; Do all this outside the WITH-INTERRUPTS.
	     (set! reaped-procs '())
	     (if clear-interactive?
		 (ensure-batch-mode!))	; Children are non-interactive.
	     (and (pair? maybe-thunk)
		  (call-terminally (car maybe-thunk))))

	   ;; Parent
	   (let ((proc (new-child-proc pid)))
	     (lambda () proc)))))))


;(define (exit . maybe-status)
;  (flush-all-ports)
;  (primitive-exit (:optional  maybe-status 0))
;  (display "The evil undead walk the earth." 2)
;  (error "(exit) returned."))


;;; The classic T 2.0 primitive.
;;; This definition works for procedures running on top of Unix systems.
(define (halts? proc) #t)


;;; Low-level init absolutely required for any scsh program.

;;(define (init-scsh-hindbrain relink-ff?)
;;  (if relink-ff? (lookup-all-externals)) ; Re-link C calls.
;;  (init-fdports!)
;;  (%install-unix-scsh-handlers))


;;; Some globals:
(define home-directory "")
(define exec-path-list (make-scsh-fluid '()))

(define (init-scsh-vars quietly?)
  (set! home-directory
	(cond ((getenv "HOME") => ensure-file-name-is-nondirectory)
	      (else (if (not quietly?)
			(warn "Starting up with no home directory ($HOME)."))
		    "/")))
  (set-scsh-fluid! exec-path-list
	      (cond ((getenv "PATH") => split-colon-list)
		    (else (if (not quietly?)
			      (warn "Starting up with no path ($PATH)."))
			  '()))))

(init-scsh-vars #f)

; SIGTSTP blows s48 away. ???
(define (suspend) (signal-process 0 signal/stop))
