; Copyright (c) 1993, 1994 Richard Kelsey and Jonathan Rees.  See file COPYING.

; modified for Guile.

;; original file: alt/bitwise.scm.

; Bitwise operators written in vanilla Scheme.
; Written for clarity and simplicity, not for speed.

; No need to use these in Scheme 48 since Scheme 48's virtual machine
; provides fast machine-level implementations.

(define-module (scsh bitwise)
  :export (arithmetic-shift bitwise-not bitwise-and bitwise-ior bitwise-xor))

;; use Guile primitives.
(define (bitwise-not a)   (lognot a))
(define (bitwise-and a b) (logand a b))
(define (bitwise-ior a b) (logior a b))
(define (bitwise-xor a b) (logxor a b))

;;(define (bitwise-not i)
;;  (- -1 i))

;;(define (bitwise-and x y)
;;  (cond ((= x 0) 0)
;;	((= x -1) y)
;;	(else
;;	 (+ (* (bitwise-and (arithmetic-shift x -1)
;;			    (arithmetic-shift y -1))
;;	       2)
;;	    (* (modulo x 2) (modulo y 2))))))

;;(define (bitwise-ior x y)
;;  (bitwise-not (bitwise-and (bitwise-not x)
;;			    (bitwise-not y))))

;;(define (bitwise-xor x y)
;;  (bitwise-and (bitwise-not (bitwise-and x y))
;;	       (bitwise-ior x y)))

(define (bitwise-eqv x y)
  (bitwise-not (bitwise-xor x y)))


(define (arithmetic-shift n m)
  (inexact->exact (floor (* n (expt 2 m)))))


;;(define (count-bits x)		; Count 1's in the positive 2's comp rep
;;  (let ((x (if (< x 0) (bitwise-not x) x)))
;;    (do ((x x (arithmetic-shift x -1))
;;	 (result 0 (+ result (modulo x 2))))
;;	((= x 0) result))))

;(define (integer-length integer) ...) ;?
