(load "lisp-unit")
(use-package :lisp-unit)

; définition des tests
(define-test test1
  (assert-equal 2 (+ 1 1))
  (assert-true (member 2 '(1 3 4 2 5)))
  )

; on lance le tout
(run-tests)