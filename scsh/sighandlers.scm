;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.
;;; Signal handler system

;;; mostly rewritten for Guile.

;;; The principal trickiness here is that we have to interface to Unix signals
;;; *through* an intermediate interface, the S48 vm's idea of interrupts.
;;; So there is a difference between delivering a signal to the underlying
;;; Unix process and delivering it to the program that runs on the VM.
;;;
;;; One effect is that we have two separate codes for the same thing -- the
;;; Unix signal code, and the S48 interrupt value. E.g., SIGNAL/TSTP and
;;; INTERRUPT/TSTP.

;;; These system calls can return EINTR or restart. In order for the S48 vm's
;;; interrupt system to detect a signal and invoke the handler, they *must*
;;; return EINTR, and this must cause a return from C to Scheme.
;;;
;;; open close dup2 accept connect
;;; read recv recvfrom recvmsg
;;; write send sendto sendmsg
;;; select
;;; wait
;;; fcntl* ioctl
;;; sigsuspend
;;; HP-UX, but I don't use: poll lockf msem_lock msgsnd msgrcv semop
;;; 
;;; * Only during a F_SETLKW
;;;
;;; From rts/interrupt.scm (package interrupts, interface interrupts-interface)
;;;     WITH-INTERRUPTS INTERRUPT-HANDLERS SET-ENABLED-INTERRUPTS !
;;;	ENABLED-INTERRUPTS
;;; Must define WITH-INTERRUPTS* and WITH-INTERRUPTS.

(define-module (scsh sighandlers)
  :use-module (scsh define-foreign-syntax)
  ;; additional exports are generated below.
  :export (signal->interrupt
           ;;  interrupt-set
           with-enabled-interrupts
           ;; with-enabled-interrupts*
           enabled-interrupts
           set-enabled-interrupts

           set-interrupt-handler
           interrupt-handler

           ;;	%set-unix-signal-handler
           ;;	%unix-signal-handler

           ))

(foreign-source
 "#include <errno.h>"
  ""
  "/* Make sure foreign-function stubs interface to the C funs correctly: */"
  "#include \"sighandlers1.h\""
  "" "")

;;; Map a Unix async signal to its S48 interrupt value.
;;; -1 => Not defined.
(define-foreign %signal->interrupt (sig2interrupt (integer sig))
  integer)

(define (signal->interrupt sig)
  sig)

;; Guile doesn't have an interrupt enabling mechanism.

;(define (interrupt-set . interrupts)
;  (let lp ((ints interrupts) (ans 0))
;    (if (pair? ints)
;	(lp (cdr ints) (bitwise-ior ans (arithmetic-shift 1 (- (car ints) 1))))
;	ans)))

;;; I'm trying to be consistent about the ! suffix -- I don't use it
;;; when frobbing process state. This is not a great rule; perhaps I
;;; should change it.
;(define set-enabled-interrupts set-enabled-interrupts!)

;(define-simple-syntax (with-enabled-interrupts mask body ...)
;  (with-interrupts mask (lambda () body ...)))

;; since scsh only seems to use with-enabled-interrupts to disable
;; interrupts, try this bogus definition.


;;This seams to be weak part of the program (Stis)
;;let's try the call-with-blocked ....
(define-syntax with-enabled-interrupts 
  (syntax-rules ()
    ((mask . body)
     (call-with-blocked-asyncs (lambda () . body)))))
#;
(define-syntax with-enabled-interrupts 
  (syntax-rules ()
    ((mask . body)
     (dynamic-wind
         (lambda () (mask-signals))
         (lambda () , body)
         (lambda () (unmask-signals))))))

;(define with-enabled-interrupts* with-interrupts)

(define-syntax maybe-define-signal 
  (lambda (x)
    (syntax-case x ()
        ((y name)
         (let ((build-name 
                (lambda (prefix signame)
                  (string->symbol
                   (string-append prefix
                                  (string-downcase
                                   (substring (symbol->string signame)
                                              3))))))
               (nm (syntax->datum (syntax name))))
           (with-syntax 
            ((interrupt-name (datum->syntax (syntax y)
                                            (build-name "interrupt/" nm)))
             (signal-name    (datum->syntax (syntax y)
                                            (build-name "signal/" nm))))
            (syntax (begin
                      (if (not (defined? 'name)) 
                          (error (format #f "~a is not defined" 'name)))
                      (begin
                        (define interrupt-name name)
                        (define signal-name name)
                        (export interrupt-name)
                        (export signal-name))))))))))
                          
                        

(maybe-define-signal SIGABRT)
(maybe-define-signal SIGALRM)
(maybe-define-signal SIGBUS)
(maybe-define-signal SIGCHLD)
(maybe-define-signal SIGCLD)
(maybe-define-signal SIGCONT)
(maybe-define-signal SIGFPE)
(maybe-define-signal SIGHUP)
(maybe-define-signal SIGILL)
(maybe-define-signal SIGINT)
(maybe-define-signal SIGIO)
(maybe-define-signal SIGIOT)
(maybe-define-signal SIGKILL)
(maybe-define-signal SIGPIPE)
(maybe-define-signal SIGPOLL)
(maybe-define-signal SIGPROF)
(maybe-define-signal SIGPWR)
(maybe-define-signal SIGQUIT)
(maybe-define-signal SIGSEGV)
(maybe-define-signal SIGSTKFLT)
(maybe-define-signal SIGSTOP)
(maybe-define-signal SIGTERM)
(maybe-define-signal SIGTRAP)
(maybe-define-signal SIGTSTP)
(maybe-define-signal SIGTTIN)
(maybe-define-signal SIGTTOU)
(maybe-define-signal SIGURG)
(maybe-define-signal SIGUSR1)
(maybe-define-signal SIGUSR2)
(maybe-define-signal SIGVTALRM)
(maybe-define-signal SIGWINCH)
(maybe-define-signal SIGXCPU)
(maybe-define-signal SIGXFSZ)

;;; HANDLER is #f (ignore), #t (default), or a procedure taking an integer
;;; argument. The interrupt is delivered to a procedure by (1) setting the
;;; ENABLED-INTERRUPTS register to 0 (i.e., blocking all interrupts), and (2)
;;; applying the procedure to the previous value of the ENABLED-INTERRUPTS
;;; register. If the procedure returns normally, the ENABLED-INTERRUPTS 
;;; register will be restored to its previous value.

(define (chandler->scsh-handler handler)
  (cond ((eq? handler SIG_DFL)
	 #t)
	((eq? handler SIG_IGN)
	 #f)
	(handler handler)
	(#t (error "non-scheme handler"))))
  
(define (set-interrupt-handler int handler)
  (chandler->scsh-handler (car (sigaction int
					  (cond ((eq? handler #t)
						 SIG_DFL)
						((eq? handler #f)
						 SIG_IGN)
						(#t
						 (lambda (signum) 
						   (handler 0))))))))

(define (interrupt-handler int)
  (let ((handler (car (sigaction int))))
    (chandler->scsh-handler handler)))
