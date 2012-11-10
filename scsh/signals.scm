(define-module (scsh signals))
(export syntax-error)

(define syntax-error error)

;;; scsh interface:
;;  (export error warn syntax-error call-error
;;	  signal signal-condition
;;	  make-condition))
