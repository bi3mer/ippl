#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")
(require "stan_matrix.rkt")

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

(test-results)