(define-module (scsh run-extras)
  :use-module (scsh syntax)
  :export (run/file* run/collecting))

(define (run/file* thunk)
  (let ((fname (create-temp-file)))
    (run (begin (thunk)) (> ,fname))
    fname))

(define (run/collecting* fds thunk)
  ;; First, generate a pair of ports for each communications channel.
  ;; Each channel buffers through a temp file.
  (let* ((channels (map (lambda (ignore)
			  (call-with-values temp-file-channel cons))
		       fds))
	 (read-ports (map car channels))
	 (write-ports (map cdr channels))

	 ;; In a subprocess, close the read ports, redirect input from
	 ;; the write ports, and run THUNK.
	 (status (run (begin (for-each close-input-port read-ports)
			     (for-each move->fdes write-ports fds)
			     (thunk)))))

    ;; In this process, close the write ports and return the exit status
    ;; and all the the read ports.
    (for-each close-output-port write-ports)
    (apply values status read-ports)))
