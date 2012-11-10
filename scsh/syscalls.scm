;;; POSIX system-call Scheme binding.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

;;; Scheme48 implementation.

;;; Need to rationalise names here. getgid. get-gid. "effective" as morpheme?

(define-module (scsh syscalls)
  :use-module (scsh define-foreign-syntax)
  :use-module (ice-9 receive)
  :use-module (ice-9 optargs)
  :use-module (scsh optional)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (scsh utilities)
  :use-module (scsh fname)
  :use-module (scsh procobj)
  :use-module (scsh errno)
  :export (export %exec %%fork cwd user-gid user-effective-gid set-gid
	user-supplementary-gids user-uid user-effective-uid set-uid
	user-login-name pid parent-pid set-process-group
	become-session-leader set-umask process-times cpu-ticks/sec
	set-file-mode set-file-owner set-file-group read-symlink
	delete-directory set-file-times
	file-info file-info:type file-info:gid file-info:inode
	file-info:atime file-info:mtime file-info:ctime file-info:mode
	file-info:nlinks file-info:uid file-info:size
	sync-file sync-file-system
	seek/set seek/delta seek/end tell vpipe
	signal-process signal-process-group pause-until-interrupt itimer
	user-info user-info:name user-info:uid user-info:gid
	user-info:home-dir user-info:shell
	name->user-info uid->user-info ->uid ->username %homedir
	group-info group-info:name group-info:gid group-info:members
	->gid ->groupname
	directory-files
	env->alist alist->env
	fdes-flags set-fdes-flags fdes-status set-fdes-status
	open/read open/write open/read+write open/non-blocking
	open/append open/exclusive open/create open/truncate
	open/no-control-tty open/access-mask
	fdflags/close-on-exec
	sleep sleep-until
	system-name))

(foreign-source
  "#include <sys/signal.h>"
  "#include <sys/types.h>"
  "#include <sys/times.h>"
  "#include <sys/time.h>"
  "#include <fcntl.h>		/* for O_RDWR */" ; ???
  "#include <sys/stat.h>"
  "#include <errno.h>"
  "#include <netdb.h>"
  "#include <pwd.h>"
  "#include <unistd.h>"
  ""
  "/* Make sure foreign-function stubs interface to the C funs correctly: */"
  "#include \"dirstuff1.h\""
  "#include \"fdports1.h\""
  "#include \"select1.h\""
  "#include \"syscalls1.h\""
  "#include \"userinfo1.h\""
  ""
  "#define errno_on_zero_or_false(x) ((x) ? SCHFALSE : ENTER_FIXNUM(errno))"
  "#define errno_or_false(x) (((x) == -1) ? ENTER_FIXNUM(errno) : SCHFALSE)"
  "#define False_on_zero(x) ((x) ? ENTER_FIXNUM(x) : SCHFALSE)" ; Not a function.
  "" "")

;;; Macro for converting syscalls that return error codes to ones that
;;; raise exceptions on errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE-ERRNO-SYSCALL defines an error-signalling syscall procedure from 
;;; one that returns an error code as its first return value -- #f for win,
;;; errno for lose. If the error code is ERRNO/INTR (interrupted syscall),
;;; we try again.
;;;
;;; (define-errno-syscall (SYSCALL ARGS) SYSCALL/ERRNO . RET-VALS) ==>
;;;
;;; (define (SYSCALL . ARGS)
;;;   (receive (err . RET-VALS) (SYSCALL/ERRNO . ARGS)
;;;     (cond ((not err) (values . RET-VALS))		; Win
;;;           ((= err errno/intr) (SYSCALL . ARGS))	; Retry
;;;           (else (errno-error err SYSCALL . ARGS))))); Lose

;; noop for Guile.
(defmacro define-errno-syscall args #f)

;;; Process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign %%exec/errno
  (scheme_exec (string prog)
	       (vector-desc argv)
	       (desc env)) ; string vector or #t.
  integer)

;(define (%%exec prog argv env)
;  (errno-error (%%exec/errno prog argv env) %exec prog argv env)) ; cute.

(define (%exec prog arg-list env)
  (if (eq? env #t)
      (apply execl prog arg-list)
      (apply execle prog (alist->env-list env) arg-list)))


(define-foreign exit/errno ; errno -- misnomer.
  (exit (integer status))
  ignore)

(define-foreign %exit/errno ; errno -- misnomer
  (_exit (integer status))
  ignore)

;; (define (%exit . maybe-status)
;;  (%exit/errno (:optional maybe-status 0))
;;  (error "Yikes! %exit returned."))


(define-foreign %%fork/errno (fork)
  (multi-rep (to-scheme pid_t errno_or_false)
             pid_t))

;;; If the fork fails, and we are doing early zombie reaping, then reap
;;; some zombies to try and free up a some space in the process table,
;;; and try again.
;;;
;;; This ugly little hack will have to stay in until I do early
;;; zombie reaping with SIGCHLD interrupts.

;(define (%%fork-with-retry/errno)
;  (receive (err pid) (%%fork/errno)
;    (cond ((and err (eq? 'early (autoreap-policy)))
;	   (reap-zombies)
;	   (%%fork/errno))
;	  (else (values err pid)))))

(define (%%fork)
  (let ((pid (false-if-exception (primitive-fork))))
    (cond ((and (not pid)
		(eq? 'early (autoreap-policy)))
	   (reap-zombies)
	   (primitive-fork))
	  (else pid))))

(define-errno-syscall (%%fork) %%fork-with-retry/errno
  pid)

;;; Posix waitpid(2) call.
(define-foreign %wait-pid/errno (wait_pid (integer pid) (integer options))
  desc		; errno or #f
  integer  ; process' id
  integer) ; process' status


;;; Miscellaneous process state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Working directory

(define-foreign %chdir/errno
  (chdir (string directory))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%chdir dir) %chdir/errno)

;; primitive in Guile.
;;(define (chdir . maybe-dir)
;;  (let ((dir (:optional maybe-dir (home-dir))))
;;    (%chdir (ensure-file-name-is-nondirectory dir))))


(define-foreign cwd/errno (scheme_cwd)
  (to-scheme integer "False_on_zero") ; errno or #f
  string) ; directory (or #f on error)

(define-errno-syscall (cwd) cwd/errno
  dir)

(define cwd getcwd)


;;; GID

(define-foreign user-gid  (getgid) gid_t)
(define user-gid getgid)
(define-foreign user-effective-gid (getegid) gid_t)
(define user-effective-gid getegid)

(define-foreign set-gid/errno (setgid (gid_t id)) no-declare ; for SunOS 4.x 
  (to-scheme integer errno_or_false))

(define-errno-syscall (set-gid gid) set-gid/errno)
(define set-gid setgid)

(define-foreign %num-supplementary-gids/errno (num_supp_groups)
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-foreign load-groups/errno (get_groups (vector-desc group-vec))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define (user-supplementary-gids) (vector->list (getgroups)))


;;; UID

(define-foreign user-uid		(getuid)  uid_t)
(define user-uid getuid)
(define-foreign user-effective-uid 	(geteuid) uid_t)
(define user-effective-uid geteuid)

(define-foreign set-uid/errno (setuid (uid_t id)) no-declare ; for SunOS 4.x
  (to-scheme integer errno_or_false))

(define-errno-syscall (set-uid uid_t) set-uid/errno)
(define set-uid setuid)

(define-foreign %user-login-name (my_username)
  static-string)
  
(define (user-login-name)
  (vector-ref (getpwuid (getuid)) 0))

;;; PID

(define-foreign pid (getpid) pid_t)
(define pid getpid)
(define-foreign parent-pid (getppid) pid_t)
(define parent-pid getppid)


;;; Process groups and session ids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign process-group (getpgrp) pid_t)
(define process-group getpgrp)
(define-foreign %set-process-group/errno
  (setpgid (pid_t pid) (pid_t groupid))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%set-process-group pid pgrp)
  %set-process-group/errno)

(define (set-process-group arg1 . maybe-arg2)
  (receive (pid pgrp) (if (null? maybe-arg2)
			  (values (pid) arg1)
			  (values arg1 (car maybe-arg2)))
	   (setpgid pid pgrp)))


(define-foreign become-session-leader/errno (setsid)
  (multi-rep (to-scheme pid_t errno_or_false)
	     pid_t))

(define-errno-syscall (become-session-leader) become-session-leader/errno
  sid)
(define become-session-leader setsid)


;;; UMASK

(define-foreign set-umask (umask (mode_t mask)) no-declare ; integer on SunOS
  mode_t)

;; primitive in Guile.
;;(define (umask)
;;  (let ((m (set-umask 0)))
;;    (set-umask m)
;;    m))

(define (set-umask newmask) (umask newmask))


;;; PROCESS TIMES

;;; OOPS: The POSIX times() has a mildly useful ret value we are throwing away.
;;; OOPS: The ret values should be clock_t, not int, but cig can't handle it.

(define-foreign process-times/errno (process_times)
  (to-scheme integer errno_or_false)
  integer	; user   cpu time
  integer	; system cpu time
  integer	; user   cpu time for me and all my descendants.
  integer)	; system cpu time for me and all my descendants.

(define-errno-syscall (process-times) process-times/errno
  utime stime cutime cstime)
(define (process-times) (let ((obj (times)))
			  (values (tms:utime obj)
				  (tms:stime obj)
				  (tms:cutime obj)
				  (tms:cstime obj))))

(define-foreign cpu-ticks/sec (cpu_clock_ticks_per_sec) integer)
(define cpu-ticks/sec internal-time-units-per-second)

;;; File system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useful little utility for generic ops that work on filenames, fd's or
;;; ports.

;(define (generic-file-op thing fd-op fname-op)
;  (if (string? thing) (fname-op thing)
;      (call/fdes thing fd-op)))


(define-foreign set-file-mode/errno
  (chmod (string path) (mode_t mode)) no-declare ; integer on SunOS
  (to-scheme integer errno_or_false))

; IBM's AIX include files declare fchmod(char*, mode_t).
; Amazing, but true. So we must prevent this def-foreign from issuing
; the conflicting, correct declaration. Hence the NO-DECLARE.

(define-foreign set-fdes-mode/errno
  (fchmod (integer fd) (mode_t mode)) ; integer on SunOS
  no-declare ; Workaround for AIX bug.
  (to-scheme integer errno_or_false))

(define-errno-syscall (set-file-mode thing mode)
  (lambda (thing mode)
    (generic-file-op thing
		     (lambda (fd)    (set-fdes-mode/errno fd    mode))
		     (lambda (fname) (set-file-mode/errno fname mode)))))

(define set-file-mode chmod)

;;; NO-DECLARE: gcc unistd.h bogusness.
(define-foreign set-file-uid&gid/errno
  (chown (string path) (uid_t uid) (gid_t gid)) no-declare
  (to-scheme integer errno_or_false))

(define-foreign set-fdes-uid&gid/errno
  (fchown (integer fd) (uid_t uid) (gid_t gid)) no-declare ; for NT Cygwin
  (to-scheme integer errno_or_false))

(define-errno-syscall (set-file-owner thing uid)
  (lambda (thing uid)
    (generic-file-op thing
		     (lambda (fd)    (set-fdes-uid&gid/errno fd    uid -1))
		     (lambda (fname) (set-file-uid&gid/errno fname uid -1)))))

(define (set-file-owner thing uid) (chown thing uid -1))

(define-errno-syscall (set-file-group thing gid)
  (lambda (thing gid)
    (generic-file-op thing
		     (lambda (fd)    (set-fdes-uid&gid/errno fd    -1 gid))
		     (lambda (fname) (set-file-uid&gid/errno fname -1 gid)))))

(define (set-file-group thing gid) (chown thing -1 gid))

;;; Uses real uid and gid, not effective. I don't use this anywhere.

(define-foreign %file-ruid-access-not?
  (access (string path)
	  (integer perms))
  bool)

;(define (file-access? path perms)
;  (not (%file-access-not? path perms)))
;
;(define (file-executable? fname)
;  (file-access? fname 1))
;
;(define (file-writable? fname)
;  (file-access? fname 2))
;
;(define (file-readable? fname)
;  (file-access? fname 4))


;; defined in filesys.scm.
(define-foreign create-hard-link/errno
  (link (string original-name) (string new-name))
  (to-scheme integer errno_or_false))

(define-errno-syscall (create-hard-link original-name new-name)
  create-hard-link/errno)

(define-foreign create-fifo/errno (mkfifo (string path) (mode_t mode))
  no-declare ; integer on SunOS
  (to-scheme integer errno_or_false))

(define-errno-syscall (create-fifo path mode) create-fifo/errno)

(define-foreign create-directory/errno
  (mkdir (string path) (mode_t mode)) no-declare ; integer on SunOS.
  (to-scheme integer errno_or_false))

;;(define (create-directory path . maybe-mode)
;;  (let ((mode (:optional maybe-mode #o777))
;;	(fname (ensure-file-name-is-nondirectory path)))
;;    (cond ((create-directory/errno fname mode) =>
;;           (lambda (err)
;;	     (if err (errno-error err create-directory path mode)))))))

(define-foreign read-symlink/errno (scm_readlink (string path))
  (multi-rep (to-scheme string errno_on_zero_or_false) ; NULL => errno, otw #f
	     static-string))
             
(define-errno-syscall (read-symlink path) read-symlink/errno
  new-path)
(define read-symlink readlink)

(define-foreign %rename-file/errno
  (rename (string old-name) (string new-name))
  (to-scheme integer errno_or_false))
  
(define-errno-syscall (%rename-file old-name new-name)
  %rename-file/errno)


(define-foreign delete-directory/errno
  (rmdir (string path))
  (to-scheme integer errno_or_false))

(define-errno-syscall (delete-directory path) delete-directory/errno)
(define delete-directory rmdir)

(define-foreign %utime/errno (scm_utime (string path)
					(integer ac_hi) (integer ac_lo)
					(integer m_hi)  (integer m_lo))
  (to-scheme integer errno_or_false))

(define-foreign %utime-now/errno (scm_utime_now (string path))
  (to-scheme integer errno_or_false))
					

;;; (SET-FILE-TIMES/ERRNO path [access-time mod-time])

(define (set-file-times path . maybe-times)
  (if (pair? maybe-times)
      (let* ((access-time (real->exact-integer (car maybe-times)))
	     (mod-time (if (pair? (cddr maybe-times))
			   (error "Too many arguments to set-file-times/errno"
				  (cons path maybe-times))
			   (real->exact-integer (cadr maybe-times)))))
	(utime path access-time mod-time))
      (utime path)))

(define-errno-syscall (set-file-times . args) set-file-times/errno)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STAT

(define-foreign stat-file/errno
  (scheme_stat (string path) (vector-desc data) (bool chase?))
  (to-scheme integer "False_on_zero")) ; errno or #f

;(define-errno-syscall (stat-file fd data chase?) stat-file/errno)

(define-foreign stat-fdes/errno
  (scheme_fstat (integer fd) (vector-desc data))
  (to-scheme integer "False_on_zero")) ; errno or #f

;(define-errno-syscall (stat-fdes fd data) stat-fdes/errno)

(define-record-type file-info-type
  (make-file-info type
                  device
                  inode
                  mode
                  nlinks
                  uid
                  gid
                  size
                  atime
                  mtime
                  ctime
                  )
  file-info?
  (type   file-info:type)
  (device file-info:device)
  (inode  file-info:inode)
  (mode   file-info:mode)
  (nlinks file-info:nlinks)
  (uid    file-info:uid)
  (gid    file-info:gid)
  (size   file-info:size)
  (atime  file-info:atime)
  (mtime  file-info:mtime)
  (ctime  file-info:ctime))


(define (file-info fd/port/fname . maybe-chase?)
  (let ((chase? (:optional maybe-chase? #t)))
    (let ((info (if (or chase?
			(not (string? fd/port/fname)))
		    (stat fd/port/fname)
		    (lstat fd/port/fname))))
      (make-file-info (stat:type info)
		      (stat:dev info)
		      (stat:ino info)
		      (stat:mode info)
		      (stat:nlink info)
		      (stat:uid info)
		      (stat:gid info)
		      (stat:size info)
		      (stat:atime info)
		      (stat:mtime info)
		      (stat:ctime info)))))

;;; "no-declare" as there is no agreement among the OS's as to whether or not
;;; the OLD-NAME arg is "const". It *should* be const.

(define-foreign create-symlink/errno
  (symlink (string old-name) (string new-name))	no-declare
  (to-scheme integer errno_or_false))
  
;(define-errno-syscall (create-symlink old-name new-name)
;  create-symlink/errno)


;;; "no-declare" as there is no agreement among the OS's as to whether or not
;;; the PATH arg is "const". It *should* be const.

(define-foreign truncate-file/errno
  (truncate (string path) (off_t length)) 	no-declare
  (to-scheme integer errno_or_false))

(define-foreign truncate-fdes/errno
  (ftruncate (integer fd) (off_t length))   no-declare ; Indigo bogosity.
  (to-scheme integer errno_or_false))

(define-errno-syscall (truncate-file path length)
  (lambda (thing length)
    (generic-file-op thing
		     (lambda (fd)    (truncate-fdes/errno fd    length))
		     (lambda (fname) (truncate-file/errno fname length)))))


(define-foreign delete-file/errno
  (unlink (string path))
  (to-scheme integer errno_or_false))

(define-errno-syscall (delete-file path) delete-file/errno)


(define-foreign sync-file/errno (fsync (integer fd))
  (to-scheme integer errno_or_false))

(define sync-file fsync)

;;; Amazingly bogus syscall -- doesn't *actually* sync the filesys.
(define-foreign sync-file-system (sync) no-declare ; Linux sux - says int
  ignore)

(define sync-file-system sync)


;;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign %close-fdes/errno (close (integer fd))
  (to-scheme integer "errno_or_false"))

;;(define (%close-fdes fd)
;;  (let lp ()
;;    (let ((errno (%close-fdes/errno fd)))
;;      (cond ((not errno) 	  #t)	; Successful close.
;;	    ((= errno errno/badf) #f)	; File descriptor already closed.
;;	    ((= errno errno/intr) (lp))	; Retry.
;;	    (else
;;	     (errno-error errno %close-fdes fd))))))	; You lose.

(define-foreign %dup/errno
  (dup (integer fd))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-errno-syscall (%dup fd) %dup/errno
   new-fd)

(define-foreign %dup2/errno
  (dup2 (integer fd-from) (integer fd-to))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-errno-syscall (%dup2 fd-from fd-to) %dup2/errno 
   new-fd)


(define-foreign %fd-seek/errno
  (lseek (integer fd) (off_t offset) (integer whence))
  (multi-rep (to-scheme off_t errno_or_false)
	     off_t))



(define seek/set SEEK_SET)			;Unix codes for "whence"
(define seek/delta SEEK_CUR)
(define seek/end SEEK_END)

;(define (seek fd/port offset . maybe-whence)
;  (let ((whence (:optional maybe-whence seek/set)))
;    (receive (err cursor)  
;	((if (integer? fd/port) %fd-seek/errno %fdport-seek/errno)
;	 fd/port
;	 offset
;	 whence)
;      (if err (errno-error err seek fd/port offset whence) cursor))))

(define tell ftell)

(define-foreign %char-ready-fdes?/errno
  (char_ready_fdes (integer fd))
  desc) ; errno, #t, or #f

;;(define (%char-ready-fdes? fd)
;;  (let ((retval (%char-ready-fdes?/errno fd)))
;;    (if (integer? retval) (errno-error retval %char-ready-fdes? fd)
;;	retval)))


(define-foreign %open/errno
  (open (string path)
	(integer flags)
	(mode_t mode))	; integer on SunOS
  no-declare ; NOTE
  (multi-rep (to-scheme integer errno_or_false)
             integer))

(define-errno-syscall (%open path flags mode) %open/errno
   fd)

;;(define (open-fdes path flags . maybe-mode) ; mode defaults to 0666
;;  (%open path flags (:optional maybe-mode #o666)))


(define-foreign pipe-fdes/errno (scheme_pipe)
  (to-scheme integer "False_on_zero")	; Win: #f, lose: errno
  integer	; r
  integer)	; w

(define-errno-syscall (pipe-fdes) pipe-fdes/errno
  r w)


(define vpipe (lambda ()
	       (let ((rv (pipe)))
		 (values (car rv) (cdr rv)))))

(define-foreign %read-fdes-char
  (read_fdes_char (integer fd))
  desc) ; Char or errno or #f (eof).

;;(define (read-fdes-char fd)
;;  (let ((c (%read-fdes-char fd)))
;;    (if (integer? c) (errno-error c read-fdes-char fd) c)))


(define-foreign write-fdes-char/errno (write_fdes_char (char char) (integer fd))
  (to-scheme integer errno_or_false))

(define-errno-syscall (write-fdes-char char fd) write-fdes-char/errno)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read and write

(define-foreign read-fdes-substring!/errno
  (read_fdes_substring (string-desc buf)
		       (integer start)
		       (integer end)
		       (integer fd))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-foreign write-fdes-substring/errno
  (write_fdes_substring (string-desc buf)
			(integer start)
			(integer end)
			(integer fd))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))


;;; Signals (rather incomplete)
;;; ---------------------------

(define-foreign signal-pid/errno
  (kill (pid_t pid) (integer signal))
  (to-scheme integer errno_or_false))

(define-errno-syscall (signal-pid pid signal) signal-pid/errno)

(define (signal-process proc signal)
  (kill (cond ((proc? proc)    (proc:pid proc))
	      ((integer? proc) proc)
	      (else (error "Illegal proc passed to signal-process" proc)))
	signal))

(define (signal-process-group proc-group signal)
  (kill (- (cond ((proc? proc-group)    (proc:pid proc-group))
		 ((integer? proc-group) proc-group)
		 (else (error "Illegal proc passed to signal-process-group"
			      proc-group))))
	signal))

;;; SunOS, not POSIX:
;;; (define-foreign signal-process-group/errno
;;;   (killpg (integer proc-group) (integer signal))
;;;   (to-scheme integer errno_or_false))
;;; 
;;; (define-errno-syscall (signal-process-group proc-group signal)
;;;   signal-process-group/errno)

(define-foreign pause-until-interrupt (pause) no-declare ignore)

(define pause-until-interrupt pause)

(define-foreign itimer (alarm (uint_t secs)) uint_t)
(define itimer alarm)

;;; User info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type user-info-type
  (make-user-info name uid gid home-dir shell)
  user-info?
  (name     user-info:name)
  (uid      user-info:uid)
  (gid      user-info:gid)
  (home-dir user-info:home-dir)
  (shell    user-info:shell))


(set-record-type-printer! user-info-type
   (lambda (ui port)
     (format port "#{user-info ~a}" (user-info:name ui))))

(define-foreign %uid->user-info (user_info_uid (uid_t uid))
  bool		; win?
  static-string	; name
  gid_t		; gid
  static-string	; home-dir
  static-string); shell

(define-foreign %name->user-info (user_info_name (string name))
  bool		; win?
  uid_t		; uid
  gid_t		; gid
  static-string	; home-dir
  static-string); shell

(define (user-info uid/name)
  (let ((info (getpw uid/name)))
    (make-user-info (passwd:name info)
		    (passwd:uid info)
		    (passwd:gid info)
		    (passwd:dir info)
		    (passwd:shell info))))
(define name->user-info user-info)
(define uid->user-info user-info)
  
;;; Derived functions

(define (->uid uid/name)
  (user-info:uid (user-info uid/name)))

(define (->username uid/name)
  (user-info:name (user-info uid/name)))

(define (%homedir uid/name)
  (user-info:home-dir (user-info uid/name)))


;;; Group info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type group-info-type
  (make-group-info name gid members)
  group-info?
  (name user-info:name)
  (gid user-info:gid)
  (members user-info:members))


(set-record-type-printer! user-info-type
   (lambda (gi port)
     (format port "#{group-info ~a}" (user-info:name gi))))

;;; These guys return static structs, so they aren't reentrant.
;;; Must be fixed for threaded version.

(define-foreign %gid->group-info
  (group_info_gid (integer gid))
  bool		; win?
  static-string	; name
  (C char**)	; members
  integer)	; num members

(define-foreign %name->group-info
  (group_info_name (string name))
  bool		; win?
  integer	; gid
  (C char**)	; members
  integer)	; num members

(define (group-info gid/name)
  (let ((info (getgr gid/name)))
    (make-group-info (group:name info)
		     (group:gid info)
		     (group:mem info))))

;;; Derived functions

(define (->gid name)
  (group-info:gid (group-info name)))

(define (->groupname gid)
  (group-info:name (group-info gid)))

;;; Directory stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign %open-dir (open_dir (string dir-name))
  (to-scheme integer "False_on_zero")	; Win: #f, lose: errno
  (C char**)				; Vector of strings
  integer)				; Length of strings

;;; Takes a null-terminated C vector of strings -- filenames.
;;; Sorts them in place by the Unix filename order: ., .., dotfiles, others.

(define-foreign %sort-file-vector
  (scm_sort_filevec ((C "const char** ~a") cvec)
		    (integer    veclen))
  ignore)

(define (directory-files . args)
  (let-optionals args ((dir ".")
		       (dotfiles? #f))
		 (check-arg string? dir directory-files)
		 (let ((dport (opendir
			       (ensure-file-name-is-nondirectory dir))))
		   (let loop ((result '()))
		     (let ((entry (readdir dport)))
		       (cond ((eof-object? entry)
			      (closedir dport)
			      (sort! result string<?))
			     ((or (and (not dotfiles?)
				       (char=? #\. (string-ref entry 0)))
				  (string=? "." entry)
				  (string=? ".." entry))
			      (loop result))
			     (else
			      (loop (cons entry result)))))))))

;;; I do this one in C, I'm not sure why:
;;; It is used by MATCH-FILES.

;;; 99/7: No one is using this function, so I'm commenting it out.
;;; Later, we could tune up the globber or regexp file-matcher to use
;;; it (but should shift it into the rx directory). But I should also go
;;; to a file-at-a-time cursor model for directory fetching. -Olin

;(define-foreign %filter-C-strings!
;  (filter_stringvec (string-desc pattern) ((C "char const ** ~a") cvec))
;  integer)	; number of files that pass the filter.

; guile version.
; (define (%filter-C-strings! pattern vec)
;   (let ((rx (make-regexp pattern))
; 	(len (vector-length vec)))
;     (let loop ((i 0) (j 0))
;       (if (= i len)
; 	  (values #f j)
; 	  (loop (+ i 1)
; 		(if (regexp-exec rx (vector-ref vec i))
; 		    (begin
; 		      (vector-set! vec j (vector-ref vec i))
; 		      (+ j 1))
; 		    j))))))


;(define (match-files regexp . maybe-dir)
;  (let ((dir (:optional maybe-dir ".")))
;    (check-arg string? dir match-files)
;    (receive (err cvec numfiles)
;	     (%open-dir (ensure-file-name-is-nondirectory dir))
;      (if err (errno-error err match-files regexp dir))
;      (receive (numfiles) (%filter-C-strings! regexp cvec)
;	;(if err (error err match-files))
;	(%sort-file-vector cvec numfiles)
;	(let ((files (C-string-vec->Scheme&free cvec numfiles)))
;	  (vector->list files))))))


;;; Environment manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (var . val) / "var=val" rep conversion:

(define (split-env-string var=val)
  (let ((i (string-index var=val #\=)))
    (if i (values (substring var=val 0 i)
		  (substring var=val (+ i 1) (string-length var=val)))
	(error "No \"=\" in environment string" var=val))))

(define (env-list->alist env-list)
  (map (lambda (var=val)
	 (call-with-values (lambda () (split-env-string var=val))
			   cons))
       env-list))

;; guile version uses lists instead of vectors.
; (define (alist->env-vec alist)
(define (alist->env-list alist)
  (map (lambda (var.val)
	 (string-append (car var.val) "=" (cdr var.val)))
       alist))

;;; ENV->ALIST

(define-foreign %load-env (scm_envvec)
  (C char**)	; char **environ
  fixnum)	; & its length.

;(define (env->list)
;  (receive (C-env nelts) (%load-env)
;    (vector->list (C-string-vec->Scheme C-env nelts))))

(define (env->alist) (env-list->alist (environ)))

;;; ALIST->ENV

(define-foreign %install-env/errno
  (install_env (vector-desc env-vec))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%install-env env-vec) %install-env/errno)

(define (alist->env alist)
  (environ (alist->env-list alist)))

;;; GETENV, PUTENV, SETENV

(define-foreign getenv (getenv (string var))
  static-string)

(foreign-source
 "#define errno_on_nonzero_or_false(x) ((x) ? ENTER_FIXNUM(errno) : SCHFALSE)"
 "" "")

;(define-foreign putenv/errno
;  (put_env (string var=val))
;  desc) ; #f or errno


;;; putenv takes a constant: const char *, cig can't figure that out..
(define-foreign putenv/errno
  (putenv (string-copy var=val))  no-declare
  (to-scheme integer errno_on_nonzero_or_false)) ; #f or errno

(define-foreign delete-env (delete_env (string var))
  ignore)

;; primitive in Guile.
;; (define (putenv var=val)
;;  (if (putenv/errno var=val)
;;      (error "malloc failure in putenv" var=val)))
;;
;; in Guile's boot-9.scm.
;; (define (setenv var val)
;;  (if val
;;      (putenv (string-append var "=" val))
;;      (delete-env var)))


;;; Fd-ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign close-fdport*/errno (close_fdport (desc data))
  (to-scheme integer "False_on_zero")) 	; Win: #f, lose: errno

;;(define (close-fdport* data) 
;;  (let lp ()
;;    (let ((errno (close-fdport*/errno data)))
;;      (cond ((not errno) 	  #t)	; Successful close.
;;	    ((= errno errno/badf) #f)	; File descriptor already closed.
;;	    ((= errno errno/intr) (lp))	; Retry.
;;	    (else
;;	     (errno-error errno close-fdport* data))))))    ; You lose.



(define-foreign %fdport*-read-char/errno (fdport_getchar (desc data))
  desc) ; char, errno, or #f for end-of-file.

;;(define (%fdport*-read-char data)
;;  (let ((c (%fdport*-read-char/errno data)))
;;    (if (integer? c)
;;	(if (= c errno/intr)
;;	    (%fdport*-read-char data)			; Retry
;;	    (errno-error c %fdport*-read-char data))	; Lose
;;	(or c eof-object))))				; Win


(define-foreign %fdport*-char-ready?/errno
  (fdport_char_readyp (desc data))
  desc)

;;(define (%fdport*-char-ready? data)
;;  (let ((val (%fdport*-char-ready?/errno data)))
;;    (if (integer? val) (errno-error val %fdport*-char-ready? data)
;;	val)))

(define-foreign %fdport*-write-char/errno
  (fdport_putchar (desc data) (char c))
  (to-scheme integer "False_on_zero")) 	; Win: #f, lose: errno

(define-errno-syscall (%fdport*-write-char desc c)
  %fdport*-write-char/errno)

(define-foreign flush-fdport*/errno (flush_fdport (desc data))
  (to-scheme integer "False_on_zero")) 	; Win: #f, lose: errno

;;; FLUSH-FDPORT* isn't defined with DEFINE-ERRNO-SYSCALL because that would
;;; return 0 values, which blows up S48's extended-port machinery. This
;;; version returns #f.
;;; ???

;;(define (flush-fdport* data)
;;  (cond ((flush-fdport*/errno data) =>
;;         (lambda (err) (if (= err errno/intr)
;;			   (flush-fdport* data)
;;			   (errno-error err flush-fdport* data))))
;;	(else #f)))

(define-foreign flush-all-ports/errno (flush_all_ports)
  (to-scheme integer errno_or_false))

(define-errno-syscall (flush-all-ports)
  flush-all-ports/errno)

(define-foreign %fdport*-seek/errno
  (seek_fdport (desc data) (off_t offset) (integer whence))
  (to-scheme integer "False_on_zero")	; errno
  integer)				; new position

(define-foreign %fdport*-tell/errno
  (tell_fdport (desc data))
  (to-scheme integer "False_on_zero")	; errno
  integer)

(define-foreign %fdport*-set-buffering/errno
  (set_fdbuf (desc data) (integer policy) (integer size))
  (to-scheme integer "False_on_zero"))	; errno

(define-foreign %set-cloexec (set_cloexec (integer fd) (bool val))
  (to-scheme integer "errno_or_false"))

(define-foreign %init-fdports! (init_fdports) ignore)

(define-foreign %install-port/errno
  (install_port (integer fd) (desc port) (integer revealed))
  (to-scheme integer "False_on_zero")) ; Win: #f, lose: errno
  
(define-errno-syscall (%install-port fd port revealed) %install-port/errno)


(define-foreign %maybe-fdes->port (maybe_fdes2port (integer fd))
  desc) ; fd or #f


;;; Doesn't signal on error. Clients must check return value.

(define-foreign %move-fdport
  (move_fdport (integer fd) (desc port) (integer new-revealed-count))
  bool) ; Win: #f, lose: #t


(define-foreign read-fdport*-substring!/errno
  (read_fdport_substring (string-desc buf)
			 (integer start)
			 (integer end)
			 (desc data))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-foreign write-fdport*-substring/errno
  (write_fdport_substring (string-desc buf)
			  (integer start)
			  (integer end)
			  (desc fdport))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))


;;; Some of fcntl()
;;;;;;;;;;;;;;;;;;;

(define-foreign %fcntl-read/errno (fcntl_read (fixnum fd) (fixnum command))
  (multi-rep (to-scheme integer errno_or_false)
	     integer))

(define-foreign %fcntl-write/errno
  (fcntl_write (fixnum fd) (fixnum command) (fixnum val))
  (to-scheme integer errno_or_false))

(define-errno-syscall (%fcntl-read fd command) %fcntl-read/errno value)
(define-errno-syscall (%fcntl-write fd command val) %fcntl-write/errno)

;;; fcntl()'s F_GETFD and F_SETFD.  Note that the SLEAZY- prefix on the
;;; CALL/FDES isn't an optimisation; it's *required* for the correct behaviour
;;; of these procedures. Straight CALL/FDES modifies unrevealed file
;;; descriptors by clearing their CLOEXEC bit when it reveals them -- so it
;;; would interfere with the reading and writing of that bit!

(define (fdes-flags fd/port)
  (fcntl fd/port F_GETFD))

(define (set-fdes-flags fd/port flags)
  (fcntl fd/port F_SETFD flags))

;;; fcntl()'s F_GETFL and F_SETFL.
;;; Get: Returns open flags + get-status flags (below)
;;; Set: append, sync, async, nbio, nonblocking, no-delay

(define (fdes-status fd/port)
  (fcntl fd/port F_GETFL))

(define (set-fdes-status fd/port flags)
  (fcntl fd/port F_SETFL flags))

(define open/read O_RDONLY)
(define open/write O_WRONLY)
(define open/read+write O_RDWR)
(define open/non-blocking O_NONBLOCK)
(define open/append O_APPEND)
(define open/exclusive O_EXCL)
(define open/create O_CREAT)
(define open/truncate O_TRUNC)
(define open/no-control-tty O_NOCTTY)

(define open/access-mask (logior open/read open/write open/read+write))

(define fdflags/close-on-exec FD_CLOEXEC)

;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; usleep(3): Try to sleep for USECS microseconds.
;;; sleep(3):  Try to sleep for SECS seconds.

; De-released -- not POSIX and not on SGI systems.
; (define-foreign usleep (usleep (integer usecs)) integer)

;; Guile's sleep can be interrupted so define using sleep-until.
(define (sleep secs) (sleep-until (+ secs (current-time))))

(define guile-sleep
  (module-ref (resolve-module '(guile)) 'sleep))

(define (sleep-until when)
  (let ((now (current-time))
	(iwhen (inexact->exact when)))
    (if (> iwhen now)
	(if (> (guile-sleep (- iwhen now)) 0)
	    (sleep-until when)))))

(define-foreign %sleep-until (sleep_until (fixnum hi)
					  (fixnum lo))
  desc)

(define-foreign %gethostname (scm_gethostname)
  static-string)

(define (system-name) (utsname:nodename (uname)))

(define-foreign errno-msg (errno_msg (integer i))
  static-string)
(define errno-msg strerror)
