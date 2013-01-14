;;; Macro expanding procs for scsh.
;;; Written for Clinger/Rees explicit renaming macros.
;;; Needs name-export and receive-syntax S48 packages.
;;; Also needs scsh's utilities package (for CHECK-ARG).
;;; Must be loaded into for-syntax package.
;;; Copyright (c) 1993 by Olin Shivers. See file COPYING.

(define-module (scsh syntax-helpers)
  :use-module (scsh  signals)
  :use-module (scsh  fdports)
  :use-module (scsh scsh)
  :use-module (ice-9 receive)
  ;:use-module (scsh  scsh)
  :export (transcribe-process-form transcribe-extended-process-form))

(define-syntax peeker
  (lambda (x)
    (syntax-case x ()
      ((_ x) 
       ;(pk `(res: ,(syntax->datum (syntax x))))
       (syntax x)))))

(define-syntax transcribe-process-form
  (lambda (x)
    (syntax-case x ()
      ((_ . x) 
       ;(pk `(tr-pr-form ,@(syntax->datum (syntax x))))
       (syntax (transcribe-process-form* x peeker))))))

(define-syntax transcribe-process-form*
  (syntax-rules (begin | |+ epf pipe pipe+ )
    ((_ (begin . rest) . l)
     (transcribe-begin-process-form  rest . l))
    ((_ (epf   . rest) . l)
     (transcribe-extended-process-form* rest . l))
    ((_ (pipe  . rest) . l)
     (transcribe-simple-pipeline rest . l))
    ((_ (|  . rest) . l)
     (transcribe-simple-pipeline rest . l))
    ((_ (pipe+ . rest) . l)
     (transcribe-complex-pipeline rest . l))
    ((_ (|+ . rest) . l)
     (transcribe-complex-pipeline rest . l))

    ((_ (rest ...))
     (apply exec-path `(rest ...)))
    ((_ (rest ...) f l ...)
     (f (apply exec-path `(rest ...)) l ...))))

;; DEBLOCK maps an expression to a list of expressions, flattening BEGINS.
;; (deblock '(begin (begin 3 4) 5 6 (begin 7 8))) => (3 4 5 6 7 8)
;; initiated wit cont and ends with (cont 3 4 5 6 7 8)


(define-syntax deblock
 (syntax-rules (begin)
   ((_ (( (begin)       y ...)  z ...) . l)
    (deblock ((y ...) z ...) . l))
   ((_ (( (begin l ...) y ...) z ...) . u)
    (deblock ((l ... y ...) z ...) . u))
   ((_ (( x             y ...) z ...) . l)
    (deblock ((y ...)          z ... x) . l))
   ((_  (() z ...))  (z ...))
   ((_  (() z ...) f l ...)  (f (z ...) l ...))))

;; BLOCKIFY maps an expression list to a BEGIN form, flattening nested BEGINS.
;; (blockify '( (begin 3 4) 5 (begin 6) )) => (begin 3 4 5 6)

(define-syntax blockify
  (syntax-rules ()
    ((_ (x ...) . l)  (deblock ((x ...) begin) . l))))


(define-syntax thunkate
  (syntax-rules ()
    ((_ code) (lambda () (deblock (code))))))

;;; Process forms are rewritten into code that causes them to execute
;;; in the current process.
;;; (BEGIN . scheme-code)	=> (STDIO->STDPORTS (LAMBDA () . scheme-code))
;;; (| pf1 pf2)			=> (BEGIN (FORK/PIPE (LAMBDA () pf1-code))
;;;                                       pf2-code)
;;; (|+ conns pf1 pf2)		=> (BEGIN
;;;                                  (FORK/PIPE+ `conns (LAMBDA () pf1-code))
;;;                                  pf2-code)
;;; (epf . epf)			=> epf-code
;;; (prog arg1 ... argn)	=> (APPLY EXEC-PATH `(prog arg1 ... argn))
;;; [note the implicit backquoting of PROG, ARG1, ...]

;;; NOTE: | and |+ won't read into many Scheme's as a symbol. If your
;;; Scheme doesn't handle it, kill them, and just use the PIPE, PIPE+
;;; aliases.


(define-syntax syntax-pre-last
  (syntax-rules ()
    ((_ y (x . u)        0 f . l)
     (f x syntax-pre-last (y u) 1 f . l))
    ((_ fx (y ()) 1 f g . l)
     (g y fx . l))
    ((_ fx ((y ...) u) 1 . l)
     (syntax-pre-last (y ... fx) u 0 . l))))

(define-syntax syntax-map
  (syntax-rules ()
    ((_ () ()            0 f g . l)
     (g () . l))
    ((_ y (x . u)        0 f . l)
     (f x syntax-map (y u) 1 f . l))
    ((_ fx ((y ...) ()) 1 f g . l)
     (g (y ... fx) . l))
    ((_ fx ((y ...) u) 1  . l)
     (syntax-map (y ... fx) u 0 . l)))) 
    
  
(define-syntax transcribe-begin-process-form
  (syntax-rules ()
    ((_ (body ...))
     (with-stdio-ports* (lambda () (begin body ...))))

    ((_ (body ...) f l ...)
     (f (with-stdio-ports* (lambda () (begin body ...))) l ...))))


(define-syntax transcribe-simple-pipeline 
  (syntax-rules ()
    ((_)  (error "Empty pipline"))
    ((_ (p ...) . l)
     (syntax-pre-last () (p ...) 0  
                      transcribe-process-form* tr-s-p . l))))

(define-syntax transcribe-complex-pipeline 
  (syntax-rules ()
    ((_ (conns pf ...) . l)
     (syntax-pre-last () (pf ...) 0 
                      transcribe-process-form*  tr-s-q conns . l))))

(define-syntax tr-s-p
  (syntax-rules ()
    ((_ (rev ...) y . l)
     (blockify ((fork/pipe (thunkate rev)) ... y) . l))))

(define-syntax tr-s-q
  (syntax-rules ()
    ((_ (rev ...) y conns)
     (let ((w `conns))
     (blockify ((fork/pipe+ w (thunkate rev)) ... y))))

    ((_ (rev ...) y conns f . l)
     (f (let ((w `conns))
          (blockify ((fork/pipe+ w (thunkate rev)) ... y))) . l))))

(define-syntax twix
  (syntax-rules ()
    ((_ rs r . l)
     (transcribe-process-form* r t-e-p-f rs . l))))

(define-syntax transcribe-extended-process-form
  (lambda (x)
    (syntax-case x ()
      ((_ . y) 
       ;(pk `(tr-e-pr-form ,(syntax->datum (syntax y))))
       (syntax (transcribe-extended-process-form* y peeker))))))

(define-syntax transcribe-extended-process-form*
  (syntax-rules ()
    ((_ (pf redirs ...) . l)
     (syntax-map () (redirs ...) 0 transcribe-redirection twix pf . l))))


(define-syntax t-e-p-f
  (syntax-rules ()
    ((_  r (rs ...) . l)
     (blockify (rs ... r) . l))))

(define-syntax parse-spec
  (syntax-rules ()
    ((_ ((a) def do-tr x) l ...)
     (do-tr (def `a x) l ...))
    ((_ ((a b) def do-tr x) l ...)
     (do-tr (`a `b x) l ...))
    ((_ ((a b _) def do-tr x) l ...)
     (do-tr (`a `b x) l ...))))

(define-syntax do-tr
  (syntax-rules ()
    ((_ (fdes fname 0))
     (shell-open fname 0 fdes))
    ((_ (fdes fname 0) f l ...)
     (f (shell-open fname 0 fdes) l ...))
    ((_ (fdes fname 1))
     (shell-open fname open/create+trunc fdes))
    ((_ (fdes fname 1) f l ...)
     (f (shell-open fname open/create+trunc fdes) l ...))
    ((_ (fdes exp 2))
     (shell-open exp (open-string-source exp) fdes))
    ((_ (fdes exp 2) f l ...)
     (f (shell-open exp (open-string-source exp) fdes) l ...))
    ((_ (fdes fname 3))
     (shell-open fname (open/write+append+create fdes)))
    ((_ (fdes fname 3) f l ...)
     (f (shell-open fname (open/write+append+create fdes)) l ...))))

(define-syntax transcribe-redirection 
  (syntax-rules (< > << >> = - stdports)
    ((_ (< . args) . l)
     (parse-spec (args 0 do-tr 0) . l))

    ((_ (> . args) . l)
     (parse-spec (args 1 do-tr 1) . l))
    
    ((_ (<< . args) . l)
     (parse-spec (args 0 do-tr 2) . l))
    
    ((_ (>> . args) . l)
     (parse-spec (args 1 do-tr 3) . l))

    ((_ (= x y))
     (dup->fdes y x))

    ((_ (= x y) f l ...)
     (f (dup->fdes y x) l ...))

    ((_ (- x))
     (close x))

    ((_ (- x) f l ...)
     (f (close x) l ...))

    ((_ stdports) (stdports->stdio))
    ((_ stdports f l ...) (f (stdports->stdio) l ...))

    ((_ _) (oops))
    ((_ _ f l ...) (f (oops) l ...))))
