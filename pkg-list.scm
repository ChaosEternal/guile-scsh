;; Copyright (C) 2013 Chaos Eternal <chaoseternal@gmail.com>
;; See file COPYING. 

(package (guile-scsh (0 1))
	 (provides scsh)
	 (depends (srfi-8) (srfi-9) (srfi-13) (srfi-14)
		  (guile (>= (2 0))))
	 (synopsis "scheme shell ported to guile-2.x")
	 (description
	  "A port of scsh (scheme shell) to guile."
	  )
	 (homepage "http://gitorious.org/guile-scsh")
	 (libraries "scsh")
  (documentation "docs" "README" "COPYING" "USAGE"))