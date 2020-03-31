#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_matrix.rkt")
(require "stan_vector.rkt")

;; test matrix->init
(test-equal (term (matrix->init 1 1)) (term ((0.0))))
(test-equal (term (matrix->init 1 3)) (term ((0.0) (0.0) (0.0))))
(test-equal (term (matrix->init 3 1)) (term ((0.0 0.0 0.0))))
(test-equal (term (matrix->init 2 2)) (term ((0.0 0.0) (0.0 0.0))))

;; test matrix->numRows
(test-equal (term (matrix->numRows (matrix->init 1 1))) 1)
(test-equal (term (matrix->numRows (matrix->init 2 5))) 5)
(test-equal (term (matrix->numRows (matrix->init 5 3))) 3)

;; test matrix->numRows
(test-equal (term (matrix->numRows (matrix->init 1 1))) 1)
(test-equal (term (matrix->numCols (matrix->init 2 5))) 2)
(test-equal (term (matrix->numCols (matrix->init 5 3))) 5)

;; test matrix->getVector
(test-equal
 (term (matrix->getVector (matrix->init 1 5) 0))
 "index out of bounds")
(test-equal
 (term (matrix->getVector (matrix->init 1 5) 6))
 "index out of bounds")
(test-equal
 (term (matrix->getVector (matrix->init 1 5) 1))
 (term (0.0)))
(test-equal
 (term (matrix->getVector (matrix->init 2 5) 5))
 (term (0.0 0.0)))

;; test matrix->inBounds
(test-equal (term (matrix->inBounds ((0.0 0.0)) 0)) #f)
(test-equal (term (matrix->inBounds ((0.0 0.0)) 1)) #t)
(test-equal (term (matrix->inBounds ((0.0 0.0)) 2)) #f)
(test-equal (term (matrix->inBounds (matrix->init 2 3) 0)) #f)
(test-equal (term (matrix->inBounds (matrix->init 2 3) 1)) #t)
(test-equal (term (matrix->inBounds (matrix->init 2 3) 2)) #t)
(test-equal (term (matrix->inBounds (matrix->init 2 3) 3)) #t)
(test-equal (term (matrix->inBounds (matrix->init 2 3) 4)) #f)
(test-equal (term (matrix->inBounds (matrix->init 2 2) 0)) #f)

;; test matrix->getValue
(test-equal
 (term (matrix->getValue (matrix->init 3 3) 0 1))
 "index out of bounds")

(test-equal
 (term (matrix->getValue (matrix->init 3 3) 1 0))
 "index out of bounds")

(test-equal
 (term (matrix->getValue (matrix->init 3 3) 1 1))
 0.0)

(test-equal
 (term (matrix->getValue (matrix->init 3 3) 3 3))
 0.0)

;; test matrix->setVector
(test-equal
 (term (matrix->setVector (matrix->init 2 2) 0 (1.0 1.0)))
 "index out of bounds")

(test-equal
 (term (matrix->setVector (matrix->init 2 2) 3 (1.0 1.0)))
 "index out of bounds")

(test-equal
 (term (matrix->setVector (matrix->init 2 2) 2 (1.0 1.0 1.0)))
 "mismatched vector sizes")

(test-equal
 (term (matrix->setVector (matrix->init 2 2) 1 (1.0)))
 "mismatched vector sizes")

(test-equal
 (term (matrix->setVector (matrix->init 2 2) 1 (1.0 1.0)))
 (term ((1.0 1.0) (0.0 0.0))))

(test-equal
 (term (matrix->setVector (matrix->init 2 2) 2 (1.0 1.0)))
 (term ((0.0 0.0) (1.0 1.0))))

;; matrix->setValue
(test-equal
 (term (matrix->setValue (matrix->init 3 1) 1 0 3.3))
 "index out of bounds")

(test-equal
 (term (matrix->setValue (matrix->init 3 1) 1 4 3.3))
 "index out of bounds")

(test-equal
 (term (matrix->setValue (matrix->init 1 3) 0 1 3.3))
 "index out of bounds")

(test-equal
 (term (matrix->setValue (matrix->init 3 3) 4 1 3.3))
 "index out of bounds")

(test-equal
 (term (matrix->setValue (matrix->init 3 1) 1 1 3.3))
 (term ((3.3 0.0 0.0))))

(test-equal
 (term (matrix->setValue (matrix->init 3 1) 3 1 3.3))
 (term ((0.0 0.0 3.3))))

;; test matrix->onlyOnes
(test-equal
 (term (matrix->onlyOnes ((1.0 1.0) (1.0 1.0))))
 "contains all ones")

(test-equal
 (term (matrix->onlyOnes ((1.0 0.0) (1.0 1.0))))
 "does not contain all ones")

(test-equal
 (term (matrix->onlyOnes ((1.0 1.0) (0.0 1.0))))
 "does not contain all ones")

(test-equal
 (term (matrix->onlyOnes ((1.0 1.0) (1.0 0.0))))
 "does not contain all ones")

(test-equal
 (term (matrix->onlyOnes ((0.0 0.0) (0.0 0.0))))
 "does not contain all ones")

(test-results)