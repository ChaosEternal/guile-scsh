;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.
(define-module (scsh fdports)
  :use-module (scsh bitwise)
  :use-module (scsh syscalls)
  :use-module (scsh scsh)
  :export (open/create+trunc open/write+append+create shell-open))

;; move->fdes, dup, dup->fdes, dup->inport, dup->outport are defined by
;; libguile.


(define open/create+trunc
  (bitwise-ior open/write (bitwise-ior open/create open/truncate)))

(define open/write+append+create
  (bitwise-ior open/write 
	       (bitwise-ior  open/append open/create)))

(define (shell-open path flags fdes)
  (move->fdes (open-fdes (stringify path) flags #o666) fdes))
