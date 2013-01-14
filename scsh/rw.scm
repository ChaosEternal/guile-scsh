;;; Basic read and write
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

;;; Note: read ops should check to see if their string args are mutable.

(define-module (scsh rw)
  :use-module (ice-9 rw)
  :use-module (scsh errno)
  :use-module (scsh optional)
  :use-module (ice-9 optargs)
  :use-module (scsh let-optionals-aster)
  :re-export (read-string!/partial write-string/partial) ;; from (ice-9 rw)
  :export (bogus-substring-spec? read-string/partial
                                 read-string! read-string write-string))




(define (bogus-substring-spec? s start end)
  (or (< start 0)
      (< (string-length s) end)
      (< end start)))


;;; Best-effort/forward-progress reading 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-string/partial len . maybe-fd/port) 
  (let* ((s (make-string len))
	 (fd/port (:optional maybe-fd/port (current-input-port)))
	 (nread (read-string!/partial s fd/port 0 len)))
    (cond ((not nread) #f) ; EOF
	  ((= nread len) s)
	  (else (substring s 0 nread)))))


;;; Persistent reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-read-string! s start end reader source)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" reader source s start end))

  (let loop ((i start))
    (if (>= i end) (- i start)
	(catch 'system-error
	       (lambda ()
		 (let ((nread (reader s source i end)))
		   (if (not nread) ; EOF
		       (let ((result (- i start)))
			 (and (not (zero? result)) result))
		       (loop (+ i nread)))))
	       (lambda args
		 ;; Give info on partially-read data in error packet.
		 (set-cdr! (list-ref args 4) s)
		 (apply scm-error args))))))

(define (read-string! s . args)
  (let-optionals* args ((fd/port (current-input-port))
		       (start   0)
		       (end     (string-length s)))
		 (generic-read-string! s start end
				       read-string!/partial
				       fd/port)))

(define (read-string len . maybe-fd/port) 
  (let* ((s (make-string len))
	 (fd/port (:optional maybe-fd/port (current-input-port)))
	 (nread (read-string! s fd/port 0 len)))
    (cond ((not nread) #f) ; EOF
	  ((= nread len) s)
	  (else (substring s 0 nread)))))

;;; Persistent writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-write-string s start end writer target)
  (if (bogus-substring-spec? s start end)
      (error "Bad substring indices" writer s start end target))

  (let loop ((i start))
    (if (< i end)
	(catch 'system-error
	       (lambda ()
		 (let ((nwritten (writer s target i end)))
		   (loop (+ i nwritten))))
	       (lambda args
		 (apply scm-error args))))))

(define (write-string s . args)
  (let-optionals* args ((fd/port (current-output-port))
		       (start   0)
		       (end     (string-length s)))
		 (generic-write-string s start end
				       uniform-array-write fd/port)))
