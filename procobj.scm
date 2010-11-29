;;; Unix wait & process objects for scsh
;;; Copyright (c) 1993, 1994, 1995 by Olin Shivers. See file COPYING.

;;; This is a GC'd abstraction for Unix process id's.
;;; The problem with Unix pids is (a) they clutter up the kernel
;;; process table until you wait(2) them, and (b) you can only
;;; wait(2) them once. Scsh's process objects are similar, but
;;; allow the storage to be allocated in the scsh address space,
;;; and out of the kernel process table, and they can be waited on
;;; multiple times.

(define-module (scsh procobj)
  :use-module (ice-9 receive)
  :use-module (ice-9 optargs)
  :use-module (scsh optional)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (scsh population)
  :use-module (scsh utilities)
  :use-module (scsh sighandlers)
  :use-module (scsh weak)
  :use-module (scsh errno)
  :export (proc:pid proc? pid->proc autoreap-policy reap-zombies
                    new-child-proc wait/poll wait/stopped-children wait 
                    wait-any wait-process-group reaped-procs))

;;; Process objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type proc-type  ; A process object
  (make-proc0 pid %status)
  proc?
  (pid	   proc:pid     set-proc:pid    )   ; Proc's pid.
  (%status proc:%status set-proc:%status))  ; The cached exit status of the process; 
                           ; #f if we haven't wait(2)'d the process yet.

;; Make proc objects print like #{proc 2318}.


(set-record-type-printer! proc-type
    (lambda (proc port)
      (format port "#{proc ~a}" (proc:pid proc))))

(define make-proc
  (case-lambda
   ((proc)         (make-proc0 proc #f    ))
   ((proc status)  (make-proc0 proc status))))

;;; Indexing this table by pid requires a linear scan. 
;;; Probably not an important op, tho.

(define process-table (make-population))

(define (maybe-pid->proc pid)
  (call/cc (lambda (quit)
	     ;; Search the table.
	     (walk-population (lambda (p)
				(if (= (proc:pid p) pid) (quit p)))
			      process-table)
	     #f)))

(define (pid->proc pid . maybe-probe?)
  (let ((probe? (:optional maybe-probe? #f)))
    (or (maybe-pid->proc pid)
	(case probe?
	  ((#f)     (error "Pid has no corresponding process object" pid))
	  ((create) (let ((p (make-proc pid))) 	; Install a new one.
		      (add-to-population! p  process-table)
		      p))
	  (else     #f)))))
	     
;;; Coerce pids and procs to procs.

(define (->proc proc/pid)
  (cond ((proc? proc/pid) proc/pid)
	((and (integer? proc/pid) (>= proc/pid 0))
	 (pid->proc proc/pid))
	(else (error "Illegal parameter" ->proc proc/pid))))


;;; Is X a pid or a proc?

(define (pid/proc? x) (or (proc? x) (and (integer? x) (>= x 0))))


;;; Process reaping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Reaping" a process means using wait(2) to move its exit status from the
;;; kernel's process table into scsh, thus cleaning up the kernel's process
;;; table and saving the value in a gc'd data structure, where it can be
;;; referenced multiple times.
;;;
;;; - Stopped processes are never reaped, only dead ones.
;;; 
;;; - Stopped process status codes are never cached in proc objects, 
;;;   only status codes for dead processes. So you can wait for a
;;;   dead process multiple times, but only once per process-stop.
;;; 
;;; - Unfortunately, reaping a process loses the information specifying its
;;;   process group, so if a process is reaped into scsh, it cannot be
;;;   waited for by WAIT-PROCESS-GROUP. Notice that only dead processes are
;;;   reaped, not suspended ones. Programs almost never use WAIT-PROCESS-GROUP
;;;   to wait for dead processes, so this is not likely to be a problem. If
;;;   it is, turn autoreaping off with (autoreap-policy #f).
;;; 
;;; - Reaping can be encouraged by calling (REAP-ZOMBIES).

;;; (autoreap-policy [new-policy])

(define *autoreap-policy* #f) ; Not exported from this module.

(define (autoreap-policy . maybe-policy)
  (let ((old-policy *autoreap-policy*))
    (if (pair? maybe-policy)
	(let ((new-policy (car maybe-policy)))
	  (cond ((pair? (cdr maybe-policy))
		 (error "Too many args to autoreap-policy" maybe-policy))
		((not (memq new-policy '(early #f)))
		 (error "Illegal autoreap policy." new-policy))
		(else (set! *autoreap-policy* new-policy)
		      (if (eq? new-policy 'early)
			  (set-interrupt-handler interrupt/chld
			    (lambda (enabled-ints) (reap-zombies))))))))
    old-policy))

;;; (reap-zombies)  => bool
;;;   Move any zombies from the kernel process table into scsh.
;;;   Return true if no more outstanding children; #f if some still live.

(define (reap-zombies)
  (let lp ()
    (receive (pid status) (%wait-any wait/poll)
      (if pid
	  (begin (add-reaped-proc! pid status)
;		 (format (error-output-port)
;			 "Reaping ~d[~d]~%" pid status)
		 (lp))
	  status))))

;;; This list contains procs that haven't exited yet. FORK adds new
;;; procs to the list. When a proc exits, it is removed from the list.
;;; Being on this list keeps live children's proc objects from being gc'd.

(define unexited-procs '())

(define (new-child-proc pid)
  (let ((proc (make-proc pid)))
    (add-to-population! proc process-table)
    (set! unexited-procs (cons proc unexited-procs))
    proc))

(define (mark-proc-exited proc)
  (set! unexited-procs (del proc unexited-procs)))

(define wait/poll WNOHANG)
(define wait/stopped-children WUNTRACED)

;;; (WAIT proc/pid [flags])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (wait proc/pid [flags]) => status or #f
;;;
;;; FLAGS (default 0) is the exclusive or of the following:
;;;     wait/poll	
;;;		Return #f immediately if there are no 
;;;		unwaited children available. 
;;; 	wait/stopped-children
;;; 		Report on suspended children as well.
;;;
;;;     If the process hasn't terminated (or suspended, if wait/stopped 
;;; 	is set) and wait/poll is set, return #f.

;;; WAIT waits for a specific process. Before performing a waitpid(2)
;;; systcall, wait first consults the proc object to see if the process has
;;; been reaped already. If so, its saved status is returned immediately.
;;;

(define (wait pid/proc . maybe-flags)
  (let* ((flags (:optional maybe-flags 0))
	 (proc (->proc pid/proc))
	 (win (lambda (status)
		(mark-proc-waited! proc) ; Not eligible for a WAIT-ANY
		status)))
    ;; First, see if we've already waited or reaped the process.
    (cond ((proc:%status proc) => win)

	  (else ; Really wait.
	   (let ((pid+status (catch 'system-error
				    (lambda ()
				      (waitpid (proc:pid proc) flags))
				    (lambda args (cons #f args)))))
	     (cond ((car pid+status)
		    (and (not (zero? (car pid+status))) ; pid=0 => none ready.
			 (win (cache-wait-status proc (cdr pid+status)))))

		   ;; We got an error -- before reporting it, check
		   ;; the proc record one last time.
		   ((proc:%status proc) => win)

		   (else 
		    (apply throw (cdr pid+status)))))))))

;;; Another way to do it:
;;; Every time we reap a process, we pop out of our SIGCHLD
;;; block so that we can service an interrupt if the system
;;; so wishes.
;(define (wait/pid pid)
;  ((let lp ()
;     (blocking signal/chld
;       (or (waited pid)	; Previously waited or reaped
;	    (receive (next-dead status) (reap-a-pid)
;	      (if (= pid next-dead) (lambda () status)
;		  lp)))))))


(define (cache-wait-status proc status)
  (cond ((and (integer? status)
	      (not (status:stop-sig status)))	; He's dead, Jim.
	 (set-proc:%status proc status)	; Cache exit status.
	 (mark-proc-exited proc)))	; We're now gc'able.
  status)


;;; (wait-any [flags]) => [proc status]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     [#f #f] => non-blocking, none ready.
;;;     [#f #t] => no more.

(define (wait-any . maybe-flags)
  (let ((table-hit (lambda (proc) (values proc (proc:%status proc))))	; Hit.
	(flags (:optional maybe-flags 0)))
    (cond ((get-reaped-proc!) => table-hit)	; Check internal table.

	  (else ; Really wait.
	   (let ((pid+status (catch 'system-error
				    (lambda ()
				      (waitpid WAIT_ANY flags))
				    (lambda args (cons #f args)))))
	     (cond ((car pid+status)
		    (if (zero? (car pid+status))
			;; None ready. Check the reaped-proc table once more
			;; before reporting this.
			(cond ((get-reaped-proc!) => table-hit)
			      (else (values #f #f))) ; None ready.

			;; Win.
			(let ((proc (pid->proc (car pid+status))))
			  (cache-wait-status proc (cdr pid+status))
			  (values proc (cdr pid+status)))))

		   ;; We got an error of some sort. Check the reaped table
		   ;; one last time before really deciding there was an error.
		   (else
		    (let ((errno (car (list-ref pid+status 5))))
		      (cond ((get-reaped-proc!) => table-hit)
			    ((= errno errno/child) (values #f #t)) ; No more.
			    (else
			     (apply throw (cdr pid+status))))))))))))

;;; (wait-process-group [proc-group flags]) => [proc status]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     [#f #f] => non-blocking, none ready.
;;;     [#f #t] => no more.
;;;
;;; 
;;; If you are doing process-group waits, you do *not* want to use 
;;; early autoreaping, since the reaper loses process-group information.

(define (wait-process-group . args)
  (let-optionals args ((proc-group 0) (flags 0))
    (let ((proc-group (cond ((integer? proc-group) proc-group)
			     ((proc? proc-group)    (proc:pid proc-group))
			     (else (error "Illegal argument" wait-process-group
					  proc-group)))))
      (receive (pid status) (%wait-process-group proc-group flags)
	(if pid
	    (let ((proc (pid->proc pid)))
	      (cache-wait-status proc status)
	      (values proc status))
	    (values pid status))))))		; pid = #f -- Empty poll.



;;; (%wait-any flags) (%wait-pid pid flags) (%wait-process-group pgrp flags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Direct interfaces to waitpid(2) call.
;;; [#f #f] means no processes ready on a non-blocking wait.
;;; [#f #t] means no waitable process on wait-any.

(define (%wait-pid pid flags)
  (let* ((pid+status (waitpid pid flags)))
    (and (not (zero? (car pid+status)))
	 (cdr pid+status))))	; pid=0 => none ready.

(define (%wait-any flags)
  (let ((pid+status (catch 'system-error
			   (lambda ()
			     (waitpid WAIT_ANY flags))
			   (lambda args (cons #f args)))))
    (cond ((car pid+status)
	   (if (zero? (car pid+status))
	       (values #f #f)		; None ready.
	       (values (car pid+status (cdr pid+status)))))
	  (else
	   (let ((errno (car (list-ref pid+status 5))))
	     (if (= errno errno/child)
		 (values #f #t)		; No more.
		 (apply throw (cdr pid+status))))))))

(define (%wait-process-group pgrp flags)
  (let ((pid+status (catch 'system-error
			   (lambda ()
			     (waitpid (- pgrp) flags))
			   (lambda args (cons #f args)))))
    (cond ((car pid+status)
	   (if (zero? (car pid+status))
	       (values #f #f)		; None ready.
	       (values (car pid+status) (cdr (pid+status)))))
	  (else
	   (let ((errno (car (list-ref pid+status 5))))
	     (if (= errno errno/child)
		 (values #f #t)		; No more.
		 (apply throw (cdr pid+status))))))))


;;; Reaped process table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We keep track of procs that have been reaped but not yet waited on by
;;; the user's code. These proces are eligible for return by WAIT-ANY.
;;; We keep track of these so that WAIT-ANY will hand them out exactly once.
;;; Whenever WAIT, WAIT-ANY, WAIT-PROCESS-GROUP waits on a process to exit,
;;; it removes the process from this table if it's in it.
;;; This code is bogus -- we use weak pointers. We need populations that
;;; support deletion or filtering.

(define reaped-procs '())	; Reaped, but not yet waited. 

(define (filter-weak-ptr-list pred lis)
  (fold-right (lambda (wptr result) (let ((val (weak-pointer-ref wptr)))
				      (if (and val (pred val))
					  (cons wptr result)
					  result)))
	      '()
	      lis))

;;; Add a newly-reaped proc to the list.
(define (add-reaped-proc! pid status)
  ((with-enabled-interrupts 0
     (cond ((maybe-pid->proc pid) =>
	    (lambda (proc)
	      (set-proc:%status proc status)
	      (set! reaped-procs (cons (make-weak-pointer proc)
				       reaped-procs))
	      (lambda () #f)))
	   (else (lambda ()	; Do this w/interrupts enabled.
		   (warn "Exiting child pid has no proc object." pid status)))))))
  
;;; Pop one off the list.
(define (get-reaped-proc!)
  (with-enabled-interrupts 0
    (let grp! ()
      (and (pair? reaped-procs)
	   (let ((proc (weak-pointer-ref (car reaped-procs))))
	     (set! reaped-procs (cdr reaped-procs))
	     (or proc (grp!)))))))

;;; PROC no longer eligible to be in the list. Delete it.
(define (mark-proc-waited! proc)
  (with-enabled-interrupts 0
    (set! reaped-procs
	  (filter-weak-ptr-list (lambda (elt) (not (eq? proc elt)))
				reaped-procs))))

;;; The mark-proc-waited! machinery above is a crock. It is inefficient --
;;; we should have a flag in the proc saying if it's eligible for a WAIT-ANY.
;;; Starts off #t, changes to #f after a wait. On a #t->#f transition, we
;;; delete it from the WAIT-ANY population. Right now, every time the user
;;; waits on the proc, we re-delete it from the population -- which is
;;; a no-op after the first time.
