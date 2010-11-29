;;; Delimited readers
;;; for guile: read-delimited and read-delimited! are implemented in guile and 
;;; modified below to use scsh char-sets and multiple values.
;;; read-line is redefined below.
;;; skip-char-set isn't mentioned in the scsh manual, but is used in fr.scm.

(define-module (scsh rdelim)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-14)
  :use-module (ice-9 regex)
  :use-module (scsh errno)
  :use-module (ice-9 optargs)
  :use-module (scsh optional)
  :export (read-line read-paragraph read-delimited read-delimited!
                     %read-delimited! skip-char-set))

(define guile-read-delimited
  (module-ref (resolve-module '(ice-9 rdelim)) 'read-delimited))
(define guile-read-delimited!
  (module-ref (resolve-module '(ice-9 rdelim)) 'read-delimited!))

(define (read-delimited delims . args)
  (let ((rv
	 (apply guile-read-delimited (char-set->string delims) args)))
    (if (pair? rv)
	(values (car rv) (cdr rv))
	rv)))

(define (read-delimited! delims . args)
  (let ((rv
	 (apply guile-read-delimited! (char-set->string delims) args)))
    (if (pair? rv)
	(values (car rv) (cdr rv))
	rv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These procedures run their inner I/O loop in a C primitive, so they
;;; should be quite fast.
;;;
;;; N.B.:
;;; The C primitives %READ-DELIMITED-FDPORT!/ERRNO and 
;;; %SKIP-CHAR-SET-FDPORT/ERRNO rely on knowing the representation of
;;; character sets. If these are changed from their current representation, 
;;; this code must be changed as well. 

;;; (read-delimited delims [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a string or the EOF object. DELIM-ACTION determines what to do
;;; with the terminating delimiter:
;;; - PEEK
;;;   Leave it in the input stream for later reading.
;;; - TRIM (the default)
;;;   Drop it on the floor.
;;; - CONCAT
;;;   Append it to the returned string.
;;; - SPLIT
;;;   Return it as a second return value.
;;;
;;; We repeatedly allocate a buffer and fill it with READ-DELIMITED!
;;; until we hit a delimiter or EOF. Each time through the loop, we
;;; double the total buffer space, so the loop terminates with a log
;;; number of reads, but uses at most double the optimal buffer space.

;(define (read-delimited delims . args)
;  (let-optionals args ((port         (current-input-port))
;		       (delim-action 'trim))
;    (let ((substr (lambda (s end)		; Smart substring.
;		    (if (= end (string-length s)) s
;			(substring s 0 end))))
;	  (delims (->char-set delims))
;	  (gobble? (not (eq? delim-action 'peek))))
	
;      ;; BUFLEN is total amount of buffer space allocated to date.
;      (let lp ((strs '()) (buflen 80) (buf (make-string 80)))
;	(receive (terminator num-read)
;	         (%read-delimited! delims buf gobble? port)
;	  (if terminator

;	      ;; We are done. NUM-READ is either a read count or EOF.
;	      (let ((retval (if (and (zero? num-read)
;				     (eof-object? terminator)
;				     (null? strs))
;				terminator		; EOF -- got nothing.

;				;; Got something. Stick all the strings
;				;; together, plus the terminator if the
;				;; client said 'CONCAT.
;				(let ((s (substr buf num-read)))
;				  (cond ((and (eq? delim-action 'concat)
;					      (char? terminator))
;					 (apply string-append
;						(reverse `(,(string terminator)
;							   ,s . ,strs))))

;					((null? strs) s)    ; Gratuitous opt.
;					(else (apply string-append
;						     (reverse (cons s strs)))))))))
;		(if (eq? delim-action 'split)
;		    (values retval terminator)
;		    retval))

;	      ;; We are not done. Loop and read in some more.
;	      (lp (cons buf strs)
;		  (+ buflen buflen)
;		  (make-string buflen))))))))


;;; (read-delimited! delims buf [port delim-action start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns:
;;; - EOF if at end of file, and a non-zero read was requested.
;;; - Integer j if that many chars read into BUF.
;;; - #f if the buffer was filled w/o finding a delimiter.
;;;
;;; DELIM-ACTION determines what to do with the terminating delimiter;
;;; it is as in READ-DELIMITED.
;;;
;;; In determining the return value, there is an ambiguous case: when the 
;;; buffer is full, *and* the following char is a delimiter char or EOF.
;;; Ties are broken favoring termination over #f -- after filling the buffer,
;;; READ-DELIMITED! won't return #f until it has peeked one past the end
;;; of the buffer to ensure the next char doesn't terminate input (or is EOF).
;;; However, this rule is relaxed with delim-action = CONCAT -- if the buffer
;;; is full, READ-DELIMITED! won't wait around trying to peek at the following
;;; char to determine whether or not it is a delimiter char, since it doesn't
;;; have space to store the character anyway. It simply immediately returns #f;
;;; a following read can pick up the delimiter char.

;(define (read-delimited! delims buf . args) ; [port delim-action start end]
;  (let-optionals args ((port         (current-input-port))
;		       (delim-action 'trim)
;		       (start        0)
;		       (end          (string-length buf)))
;    (receive (terminator num-read)
;	     (%read-delimited! delims buf
;			       (not (eq? delim-action 'peek)) ;Gobble delim?
;			       port
;			       start
;			       (if (eq? delim-action 'concat)
;				   (- end 1) ; Room for terminator.
;				   end))

;      (if terminator	; Check for buffer overflow.
;	  (let ((retval (if (and (zero? num-read)
;				 (eof-object? terminator))
;			    terminator	; EOF -- got nothing.
;			    num-read))) ; Got something.

;	    (case delim-action
;	      ((peek trim)	retval)
;	      ((split)	(values retval terminator))
;	      ((concat)	(cond ((char? terminator)
;			       (string-set! buf (+ start num-read) terminator)
;			       (+ num-read 1))
;			      (else retval)))))

;	  ;; Buffer overflow.
;	  (case delim-action
;	    ((peek trim) #f)
;	    ((split)     (values #f #f))
;	    ((concat)    (let ((last (read-char port)))
;			   (if (char? last)
;			       (string-set! buf (+ start num-read) last))
;			   (and (or (eof-object? last)
;				    (char-set-contains? (->char-set delims)
;							last))
;				(+ num-read 1)))))))))
		  

(define guile-%read-delimited!
  (module-ref (resolve-module '(ice-9 rdelim)) '%read-delimited!))

(define (%read-delimited! delims buf gobble? . rest)
  (let ((rv (apply guile-%read-delimited! (char-set->string delims)
		   buf gobble? rest)))
    (values (car rv) (cdr rv))))

;;; (%read-delimited! delims buf gobble? [port start end])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This low-level routine uses a different interface. It returns two values:
;;; - TERMINATOR: A value describing why the read was terminated:
;;;   + character or eof-object => read terminated by this value; 
;;;   + #f                      => filled buffer w/o terminating read.
;;; - NUM-READ: Number of chars read into buf.
;;; 
;;; Note:
;;; - Invariant: TERMINATOR = #f  =>  NUM-READ = END - START.
;;; - Invariant: TERMINATOR = eof-object and NUM-READ = 0 => at EOF.
;;; - When determining the TERMINATOR return value, ties are broken
;;;   favoring character or the eof-object over #f. That is, if the buffer
;;;   fills up, %READ-DELIMITED! will peek at one more character from the
;;;   input stream to determine if it terminates the input. If so, that
;;;   is returned, not #f.
;;;
;;; If GOBBLE? is true, then a terminator character is removed from
;;; the input stream. Otherwise, it is left in place for a following input
;;; operation.

;(define (%read-delimited! delims buf gobble? . args)
;  (let-optionals args ((port  (current-input-port))
;		       (start 0)
;		       (end   (string-length buf)))

;    (check-arg input-port? port %read-delimited!)	; Arg checking.
;    (check-arg char-set? delims %read-delimited!)	; Required, since
;    (if (bogus-substring-spec? buf start end)		; we're calling C.
;	(error "Illegal START/END substring indices"
;	       buf start end %read-delimited!))

;    (let* ((delims (->char-set delims))
;	    (sdelims (char-set:s delims)))
 

;      (if (fdport? port)

;	  ;; Direct C support for Unix file ports -- zippy quick.
;	  (let lp ((start start) (total 0))
;	    (receive (terminator num-read)
;		     (%read-delimited-fdport!/errno sdelims buf gobble?
;						    port start end)
;	      (let ((total (+ num-read total)))
;		(cond ((not (integer? terminator)) (values terminator total))
;		      ((= terminator errno/intr) (lp (+ start num-read) total))
;		      (else (errno-error terminator %read-delimited!
;					 num-read total
;					 delims buf gobble? port start end))))))

;	  ;; This is the code for other kinds of ports.
;	  ;; Mighty slow -- we read each char twice (peek first, then read).
;	  (let lp ((i start))
;	    (let ((c (peek-char port)))
;	      (cond ((or (eof-object? c)	; Found terminating char or eof
;			 (char-set-contains? delims c))
;		     (if gobble? (read-char port))
;		     (values c (- i start)))

;		    ((>= i end)			; Filled the buffer.
;		     (values #f (- i start)))
			
;		    (else (string-set! buf i (read-char port))
;			  (lp (+ i 1))))))))))


;(foreign-source
;  "#include <sys/types.h>"
;  ""
;  "/* Make sure foreign-function stubs interface to the C funs correctly: */"
;  "#include \"fdports1.h\""
;  "" "")

;(define-foreign %read-delimited-fdport!/errno (read_delim (string delims)
;							  (var-string buf)
;							  (bool gobble?)
;							  (desc port)
;							  (fixnum start)
;							  (fixnum end))
;  desc	; int => errno; char => terminating char; eof-object; #f => buf ovflow
;  fixnum)   ; number of chars read into BUF.


;(define-foreign %skip-char-set-fdport/errno (skip_chars (string skip-set)
;							(desc port))
;  desc		; int => errno; #f => win.
;  fixnum)	; number of chars skipped.


(define (skip-char-set skip-chars . maybe-port)
  (let* ((port (:optional maybe-port (current-input-port)))
	 (cset (->char-set skip-chars)))
;	 (scset (char-set:s cset)))

      (cond ((not (input-port? port))
	     (error "Illegal value -- not an input port." port))
	  
;	     ;; Direct C support for Unix file ports -- zippy quick.
;	     ((fdport? port)
;	      (let lp ((total 0))
;		(receive (err num-read) (%skip-char-set-fdport/errno scset port)
;		  (let ((total (+ total num-read)))
;		    (cond ((not err) total)
;			  ((= errno/intr err) (lp total))
;			  (errno-error err skip-char-set cset port total))))))

	     ;; This is the code for other kinds of ports.
	     ;; Mighty slow -- we read each char twice (peek first, then read).
	     (else (let lp ((i 0))
		     (let ((c (peek-char port)))
		       (cond ((and (char? c) (char-set-contains? cset c))
			      (read-char port)
			      (lp (+ i 1)))
			     (else i))))))))




;;; (read-line [port delim-action])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read in a line of data. Input is terminated by either a newline or EOF.
;;; The newline is trimmed from the string by default.

(define charset:newline (char-set #\newline))

(define (read-line . rest) (apply read-delimited charset:newline rest))


;;; (read-paragraph [port handle-delim])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define blank-line-regexp (make-regexp bos (* white) #\newline eos))
(define blank-line-regexp (make-regexp "[ \t]*\n"))
(define (regexp-search r l) (regexp-match? (regexp-exec r l)))
(define (read-paragraph . args)
  (let-optionals args ((port         (current-input-port))
		       (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
	(cond ((eof-object? line)
	       (if (eq? handle-delim 'split) (values line line) line))

	      ((regexp-match? blank-line-regexp line) (lp))

	      ;; Then, read in non-blank lines.
	      (else
	       (let lp ((lines (list line)))
		 (let ((line (read-line port 'concat)))
		   (if (and (string? line)
			    (not (regexp-search? blank-line-regexp line)))

		       (lp (cons line lines))

		       ;; Return the paragraph
		       (let ((->str (lambda (lns) (apply string-append (reverse lns)))))
			 (case handle-delim
			   ((trim) (->str lines))

			   ((concat)
			    (->str (if (eof-object? line) lines (cons line lines))))

			   ((split)
			    (values (->str lines) line))

			   (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))
