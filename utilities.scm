;;; Random useful utilities.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

(define-module (scsh utilities)
  :use-module (srfi srfi-1)
  :use-module (scsh loophole)
  :use-module (scsh bitwise)
  :export (del first? nth
               mapv mapv! vector-every? copy-vector initialize-vector 
               vector-append vfold vfold-right
               check-arg conjoin disjoin negate compose 
               deprecated-proc
               deposit-bit-field
               real->exact-integer
               call/cc
               ;; reverse! omitted.
               ;; delete, filter, first, fold, fold-right, any, 
               ;; every: use srfi-1.
               ;; first: incompatible with srfi-1.
               ))

(define call/cc call-with-current-continuation)

(define (del elt lis)
  (letrec ((del (lambda (lis)
		  (if (pair? lis)
		      (let* ((head (car lis))
			     (tail (cdr lis))
			     (new-tail (del tail)))
			(if (equal? head elt) new-tail
			    (if (eq? tail new-tail) lis
				(cons head new-tail))))
		      '()))))
    (del lis)))

;(define (delete pred lis)
;  (filter (lambda (x) (not (pred x))) lis))

;(define (fold kons knil lis)
;   (let lp ((lis lis) (ans knil))
;     (if (pair? lis)
; 	(lp (cdr lis) (kons (car lis) ans))
; 	ans)))

; (define (fold-right kons knil lis)
;   (let recur ((lis lis))
;     (if (pair? lis)
; 	(let ((head (car lis)))		; Won't need LIS after RECUR call. 
; 	  (kons head (recur (cdr lis))))
; 	knil)))

; (define (filter pred list)
;   (letrec ((filter (lambda (list)
; 		     (if (pair? list)
; 			 (let* ((head (car list))
; 				(tail (cdr list))
; 				(new-tail (filter tail)))
; 			   (if (pred head)
; 			       (if (eq? tail new-tail) list
; 				   (cons head new-tail))
; 			       new-tail))
; 			 '()))))
;     (filter list)))

; (define (first pred list)
;   (letrec ((lp (lambda (list)
; 		 (and (pair? list)
; 		      (let ((head (car list)))
; 			(if (pred head) head
; 			    (lp (cdr list))))))))
;    (lp list)))

;;; Returns the first true value produced by PRED, not the list element
;;; that satisfied PRED.

(define (first? pred list)
  (letrec ((lp (lambda (list)
		 (and (pair? list)
		      (or (pred (car list))
			  (lp (cdr list)))))))
    (lp list)))

;(define any first?)

; (define (every pred list)
;   (or (not (pair? list))	
;       (let lp ((head (car list))  (tail (cdr list)))
; 	(if (pair? tail)
; 	    (and (pred head) (lp (car tail) (cdr tail)))
; 	    (pred head)))))		; Tail-call the last PRED call.


(define (mapv f v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i 0 (+ i 1)))
	((>= i len) ans)
      (vector-set! ans i (f (vector-ref v i))))))

(define (mapv! f v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
	((>= i len) v)
      (vector-set! v i (f (vector-ref v i))))))

(define (vector-every? pred v)
  (let lp ((i (- (vector-length v) 1)))
    (or (< i 0)
	(and (pred (vector-ref v i))
	     (lp (- i 1))))))

(define (copy-vector v)
  (let* ((len (vector-length v))
	 (ans (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) ans)
      (vector-set! ans i (vector-ref v i)))))

(define (initialize-vector len init)
  (let ((v (make-vector len)))
    (do ((i (- len 1) (- i 1)))
	((< i 0) v)
      (vector-set! v i (init i)))))

(define (vector-append . vecs)
  (let* ((vlen (fold (lambda (v len) (+ (vector-length v) len)) 0 vecs))
	 (ans (make-vector vlen)))
    (let lp1 ((vecs vecs) (to 0))
      (if (pair? vecs)
	  (let* ((vec (car vecs))
		 (len (vector-length vec)))
	    (let lp2 ((from 0) (to to))
	      (cond ((< from len)
		     (vector-set! ans to (vector-ref vec from))
		     (lp2 (+ from 1) (+ to 1)))
		    (else (lp1 (cdr vecs) to)))))))
    ans))
      

(define (vfold kons knil v)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1))
	 (ans knil (kons (vector-ref v i) ans)))
	((>= i len) ans))))

(define (vfold-right kons knil v)
  (do ((i (- (vector-length v) 1) (- i 1))
       (ans knil (kons (vector-ref v i) ans)))
      ((< i 0) ans)))


;;; We loophole the call to ERROR -- the point is that perhaps the
;;; user will interact with a breakpoint, and proceed with a new
;;; value, which we will then pass to a new invocation of CHECK-ARG
;;; for approval.
(define (check-arg pred val caller)
  (if (pred val) val
      (check-arg pred
		 (loophole :value (error "Bad argument" val pred caller))
		 caller)))

(define (conjoin f g)
  (lambda args (and (apply f args) (apply g args))))

(define (disjoin f g)
  (lambda args (or (apply f args) (apply g args))))

(define (negate f) (lambda args (not (apply f args))))

(define (compose f g)
  (lambda args (call-with-values (lambda () (apply g args)) f)))

;; guile primitive is compatible.
;;(define (reverse! lis)
;;  (let lp ((lis lis) (prev '()))
;;    (if (not (pair? lis)) prev
;;	(let ((tail (cdr lis)))
;;	  (set-cdr! lis prev)
;;	  (lp tail lis)))))

(define (deposit-bit-field bits mask field)
  (bitwise-ior (bitwise-and field mask)
	       (bitwise-and bits  (bitwise-not mask))))


(define (nth lis i)
  (if (< i 0) (error "nth: illegal list index" i)
      (let lp ((l lis) (i i))
	(if (pair? l)
	    (if (zero? i) (car l)
		(lp (cdr l) (- i 1)))
	    (error "nth: index too large" lis i)))))


(define (deprecated-proc proc name . maybe-preferred-msg)
  (begin-deprecated
    (lambda args
      (issue-deprecation-warning
       (string-append
	(symbol->string name)
	" is deprecated. "
	(if (pair? maybe-preferred-msg)
	    (car maybe-preferred-msg)
	    "")))
      (apply proc args))))


(define (real->exact-integer x)
  (let ((f (round x)))
    (if (inexact? f) (inexact->exact f) f)))

