;;; Python style here-string support for guile.
;;; by Mark H Weaver
;;; modified by Chaos Eternal to be integrated in guile-scsh
;;; Licensed under GPLv3 or later.

;;; Usage: just put (use-module (scsh here-strings)) in the beginning of your code.
;;; Block starts with #""", ends with """ and no escape sequences apply.

;;; for example:
;; 
;; (run (cat ) (<< #"""
;; here strings
;; with " " and 
;; """))
;; 



(define-module (scsh here-strings)
  #:use-module (ice-9 match))

(define (read-here-string c port)
  (unless (and (char=? #\" (read-char port))
               (char=? #\" (read-char port)))
    (error "Invalid here-string syntax"))
  (let loop ((chars '()))
    (match chars
      ((#\" #\" #\" . chars)
       (list->string (reverse! chars)))
      (_ (let ((c (read-char port)))
           (if (eof-object? c)
               (error "EOF within here-string")
               (loop (cons c chars))))))))

(read-hash-extend #\" read-here-string)

;; (define (read-here-string c port)
;;   (unless (and (char=? #\' (read-char port))
;;                (char=? #\' (read-char port)))
;;     (error "Invalid here-string syntax"))
;;   (let loop ((chars '()))
;;     (match chars
;;       ((#\' #\' #\' . chars)
;;        (list->string (reverse! chars)))
;;       ((#\n #\\ . chars)
;;        (loop (cons #\newline chars)))
;;       ((#\r #\\ . chars)
;;        (loop (cons #\cr chars)))
;;       ((#\t #\\ . chars)
;;        (loop (cons #\tab chars)))
;;       ((#\0 #\\ . chars)
;;        (loop (cons #\null chars)))
;;       (_ (let ((c (read-char port)))
;;            (if (eof-object? c)
;;                (error "EOF within here-string")
;;                (loop (cons c chars))))))))

;; (read-hash-extend #\" read-here-string)