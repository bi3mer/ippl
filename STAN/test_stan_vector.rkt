#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")

;; test vector->init
(test-equal (term (vector->init 0)) (term ()))
(test-equal (term (vector->init 1)) (term (0.0)))
(test-equal (term (vector->init 5)) (term (0.0 0.0 0.0 0.0 0.0)))

;; test vector->size
(test-equal (term (vector->size (vector->init 0))) 1)
(test-equal (term (vector->size (vector->init 1))) 2)
(test-equal (term (vector->size (vector->init 42))) 43)

;; test vector->get
;; the two commented out cases will show that an exception is thrown.
(test-equal (term (vector->get (1 2 3 4) 1)) 1)
(test-equal (term (vector->get (1 2 3 4) 2)) 2)
(test-equal (term (vector->get (1 2 3 4) -1)) "index out of bounds")
(test-equal (term (vector->get (1 2 3 4) 5)) "index out of bounds")

;; test vector->outOfBounds
(test-equal (term (vector->outOfBounds (1 2 3 4) 4)) #f)
(test-equal (term (vector->outOfBounds (1 2 3 4) 5)) #t)
(test-equal (term (vector->outOfBounds (1 2 3 4) 0)) #t)

;; test vector->set
(define vs1 (term (vector->set (1 2 3 4) 1 3)))
(test-equal vs1 (term (3 2 3 4)))

(define vs2 (term (vector->set (1 2 3 4) 4 10)))
(test-equal vs2 (term (1 2 3 10)))

(define vs3 (term (vector->set (1 2 3 4) 5 3)))
(test-equal vs3 "index out of bounds")

(define vs4 (term (vector->set (1 2 3 4) 0 3)))
(test-equal vs4 "index out of bounds")

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

;; test vector->add-vectors.
(test-equal (term (vector->add-vectors () ())) (term ()))
(test-equal (term (vector->add-vectors (1) (1))) (term (2)))
(test-equal (term (vector->add-vectors (1 2 3 4) (1 1 1 1))) (term (2 3 4 5)))
(test-equal (term (vector->add-vectors (1) (1 2))) "vectors must be of the same size")

;; test vector->subtract-vectors.
(test-equal (term (vector->subtract-vectors () ())) (term ()))
(test-equal (term (vector->subtract-vectors (1) (1))) (term (0)))
(test-equal (term (vector->subtract-vectors (1 2 3 4) (1 1 1 2))) (term (0 1 2 2)))
(test-equal (term (vector->subtract-vectors (1) (1 2))) "vectors must be of the same size")

;; test vector->multiply-vectors.
(test-equal (term (vector->multiply-vectors () ())) (term ()))
(test-equal (term (vector->multiply-vectors (1) (1))) (term (1)))
(test-equal (term (vector->multiply-vectors (1 2 3 4) (1 1 1 2))) (term (1 2 3 8)))
(test-equal (term (vector->multiply-vectors (1) (1 2))) "vectors must be of the same size")

;; test vector->divide-vectors. 
(test-equal (term (vector->divide-vectors () ())) (term ()))
(test-equal (term (vector->divide-vectors (1) (1))) (term (1)))
(test-equal (term (vector->divide-vectors (1.0 2.0 3.0 4.0) (1.0 1.0 2.0 2.0))) (term (1.0 2.0 1.5 2.0)))
(test-equal (term (vector->divide-vectors (1) (1 2))) "vectors must be of the same size")

;; test vector->ordered
(test-equal (term (vector->ordered ())) "vector is ordered")
(test-equal (term (vector->ordered (3))) "vector is ordered")
(test-equal (term (vector->ordered (3 2))) "vector is not ordered")
(test-equal (term (vector->ordered (1 2 3 4 5))) "vector is ordered")

;; test vector->positiveOrdered
(test-equal (term (vector->positiveOrdered ())) "vector is positive ordered")
(test-equal (term (vector->positiveOrdered (1))) "vector is positive ordered")
(test-equal (term (vector->positiveOrdered (-1))) "vector is not positive ordered")
(test-equal (term (vector->positiveOrdered (-1 2 34 55))) "vector is not positive ordered")
(test-equal (term (vector->positiveOrdered (3 1 2))) "vector is not positive ordered")
(test-equal (term (vector->positiveOrdered (1 2 3))) "vector is positive ordered")

(test-results)