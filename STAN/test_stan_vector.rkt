#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")

;; test vector->init
(test-equal (term (vector->init 0)) (term ()))
(test-equal (term (vector->init 1)) (term (0.0)))
(test-equal (term (vector->init 5)) (term (0.0 0.0 0.0 0.0 0.0)))

;; test vector->size
(test-equal (term (vector->size (vector->init 0))) 0)
(test-equal (term (vector->size (vector->init 1))) 1)
(test-equal (term (vector->size (vector->init 42))) 42)

;; test vector->get
;; the two commented out cases will show that an exception is thrown.
(test-equal (term (vector->get (1 2 3 4) 1)) 1)
(test-equal (term (vector->get (1 2 3 4) 2)) 2)
;(test-equal (term (vector->get (1 2 3 4) -1)) -1)
;(test-equal (term (vector->get (1 2 3 4) 5)) -1)

;; test vector->add-const
(test-equal (term (vector->add-const (0) 1)) (term (1)))
(test-equal (term (vector->add-const (0 1 2 3) 2)) (term (2 3 4 5)))
(test-equal (term (vector->add-const (0 1 2 3) -1)) (term (-1 0 1 2)))

;; test vector->subtract-const
(test-equal (term (vector->subtract-const (0) 1)) (term (-1)))
(test-equal (term (vector->subtract-const (0 1 2 3) 2)) (term (-2 -1 0 1)))
(test-equal (term (vector->subtract-const (0 1 2 3) -1)) (term (1 2 3 4)))

;; test vector->multiply-const
(test-equal (term (vector->multiply-const (0) 1)) (term (0)))
(test-equal (term (vector->multiply-const (0 1 2 3) 2)) (term (0 2 4 6)))
(test-equal (term (vector->multiply-const (0 1 2 3) -1)) (term (0 -1 -2 -3)))

;; test vector->divide-const
(test-equal (term (vector->divide-const (0.0) 1)) (term (0.0)))
(test-equal (term (vector->divide-const (0.0 1.0 2.0 3.0) 2)) (term (0.0 0.5 1.0 1.5)))
(test-equal (term (vector->divide-const (0.0 1.0 2.0 3.0) -1)) (term (-0.0 -1.0 -2.0 -3.0)))

;; test vector->add-vectors. Size constraints are not tested because errors are
;; thrown instead.
(test-equal (term (vector->add-vectors () ())) (term ()))
(test-equal (term (vector->add-vectors (1) (1))) (term (2)))
(test-equal (term (vector->add-vectors (1 2 3 4) (1 1 1 1))) (term (2 3 4 5)))

;; test vector->subtract-vectors. Size constraints are not tested because errors are
;; thrown instead.
(test-equal (term (vector->subtract-vectors () ())) (term ()))
(test-equal (term (vector->subtract-vectors (1) (1))) (term (0)))
(test-equal (term (vector->subtract-vectors (1 2 3 4) (1 1 1 2))) (term (0 1 2 2)))

;; test vector->multiply-vectors. Size constraints are not tested because errors are
;; thrown instead.
(test-equal (term (vector->multiply-vectors () ())) (term ()))
(test-equal (term (vector->multiply-vectors (1) (1))) (term (1)))
(test-equal (term (vector->multiply-vectors (1 2 3 4) (1 1 1 2))) (term (1 2 3 8)))

;; test vector->divide-vectors. Size constraints are not tested because errors are
;; thrown instead.
(test-equal (term (vector->divide-vectors () ())) (term ()))
(test-equal (term (vector->divide-vectors (1) (1))) (term (1)))
(test-equal (term (vector->divide-vectors (1.0 2.0 3.0 4.0) (1.0 1.0 2.0 2.0))) (term (1.0 2.0 1.5 2.0)))

(test-results)