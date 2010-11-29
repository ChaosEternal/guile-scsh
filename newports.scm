;;; A Unix file port system to completely replace S48 file ports.
;;; We use S48 extensible ports.
;;; Copyright (c) 1993 by Olin Shivers.

;;; Guile version omits the Unix file port system and has other
;;; modifications.

(define-module (scsh newports)
  :use-module (ice-9 receive)
  :use-module (ice-9 optargs)
  :use-module (scsh optional)
  :use-module (scsh ssyntax)
  :use-module (scsh utilities)
  :use-module (scsh bitwise)
  :use-module (scsh syscalls)

  :export (fdport? set-port-buffering 
                   bufpol/block bufpol/line bufpol/none
                   open-file open-input-file open-output-file
                   call/fdes close-after
                   with-current-input-port* with-current-output-port*
                   with-current-error-port*
                   set-current-input-port! set-current-output-port!
                   set-current-error-port!
                   with-error-output-port* with-error-output-port)

  :export-syntax (with-current-input-port with-current-output-port
                                          with-current-error-port))


(define (fdport? x)
  (and (or (input-port? x)
	   (output-port? x))
       (false-if-exception (fileno x))))

(define set-port-buffering setvbuf)
(define bufpol/block _IOFBF)
(define bufpol/line _IOLBF)
(define bufpol/none _IONBF)

;;(define (%fdport-set-buffering/errno port policy size)
;; (%fdport*-set-buffering/errno (fdport-data port) policy size))

;;(define (set-port-buffering port policy . maybe-size)
;;  (let* ((size (if (pair? maybe-size)
;;		   (if (pair? (cdr maybe-size))
;;		       (error "Too many arguments." set-port-buffering)
;;		       (check-arg (lambda (s) (and (integer? s)
;;						   (<= 0 s)))
;;				  (car maybe-size)
;;				  set-port-buffering))
;;		   -1))
;;	 (policy (if (zero? size) bufpol/none policy))
;;	 (err (%fdport-set-buffering/errno port policy size)))
;;    (if err (errno-error err set-port-buffering port policy size))))

;(define (open-file fname flags . maybe-mode)
;  (let ((fd (apply open-fdes fname flags maybe-mode))
;	(access (bitwise-and flags open/access-mask)))
;    ((if (or (= access open/read) (= access open/read+write))
;	 make-input-fdport
;	 make-output-fdport)
;     fd 0)))

(define open-file open)

(define (open-input-file fname . maybe-flags)
  (let ((flags (:optional maybe-flags 0)))
    (open-file fname (deposit-bit-field flags open/access-mask open/read))))

(define (open-output-file fname . rest)
  (let* ((flags (if (pair? rest) (car rest)
		    (bitwise-ior open/create open/truncate))) ; default
	 (maybe-mode (if (null? rest) '() (cdr rest)))
	 (flags (deposit-bit-field flags open/access-mask open/write)))
    (apply open-file fname flags maybe-mode)))


(define (call/fdes fd/port proc)
  (cond ((integer? fd/port)
	 (proc fd/port))

	((fdport? fd/port)
	 (let ((port fd/port))
	   (dynamic-wind
	    (lambda ()
	      (if (not port) (error "Can't throw back into call/fdes.")))
	    (lambda () (proc (port->fdes port)))
	    (lambda ()
	      (release-port-handle port)
	      (set! port #f)))))

	(else (error "Not a file descriptor or fdport." fd/port))))

;;; Don't mess with the revealed count in the port case
;;; -- just sneakily grab the fdes and run.

(define (sleazy-call/fdes fd/port proc)
  (proc (cond ((integer? fd/port) fd/port)
	      ((fdport? fd/port)
	       (fileno fd/port))
	      (else (error "Not a file descriptor or fdport." fd/port)))))


;;; (close-after port f)
;;; 	Apply F to PORT. When F returns, close PORT, then return F's result.
;;;     Does nothing special if you throw out or throw in.

(define (close-after port f)
  (receive vals (f port)
    (close port)
    (apply values vals)))

;;; with-current-foo-port procs
;;; ---------------------------

(define (with-current-input-port* port thunk)
;; current-input-port etc., are not (yet?) fluids in Guile.
;;  (let-fluid $current-input-port port thunk))
  (let ((old-port #f))
    (dynamic-wind (lambda ()
		    (set! old-port (current-input-port))
		    (set-current-input-port port))
		  thunk
		  (lambda () (set-current-input-port old-port)))))

(define (with-current-output-port* port thunk)
;;  (let-fluid $current-output-port port thunk))
  (let ((old-port #f))
    (dynamic-wind (lambda ()
		    (set! old-port (current-output-port))
		    (set-current-output-port port))
		  thunk
		  (lambda () (set-current-output-port old-port)))))

;; this doesn't work, since any error will throw out of the dynamic
;; wind and restore the old error port.  on the other hand it doesn't
;; seem to work in the original scsh either.
(define (with-error-output-port* port thunk)
;;  (let-fluid $error-output-port port thunk))
  (let ((old-port #f))
    (dynamic-wind (lambda ()
		    (set! old-port (current-error-port))
		    (set-current-error-port port))
		  thunk
		  (lambda () (set-current-error-port old-port)))))


(define-simple-syntax (with-current-input-port port body ...)
  (with-current-input-port* port (lambda () body ...)))

(define-simple-syntax (with-current-output-port port body ...)
  (with-current-output-port* port (lambda () body ...)))

(define-simple-syntax (with-error-output-port port body ...)
  (with-error-output-port* port (lambda () body ...)))


;;; set-foo-port! procs
;;; -------------------
;;; Side-effecting variants of with-current-input-port* and friends.

(define set-current-input-port! set-current-input-port)
(define set-current-output-port! set-current-output-port)
(define set-error-output-port! set-current-error-port)
