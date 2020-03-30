#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_constraints.rkt")

;; integer constraint tests
(test-equal (term (validateConstraint x 3 ((none)) i)) (term (((x "no error")))))

(test-equal
 (term (validateConstraint x 3 ((upper = 2)) i))
 (term (((x "upper constraint not met")))))

(test-equal
 (term (validateConstraint x 3 ((lower = 4)) i))
 (term (((x "lower constraint not met")))))

(test-equal
 (term (validateConstraint x 3 ((lower = 2) (upper = 4)) i))
 (term (((x "no error") (x "no error")))))

;; real constraint tests
(test-equal (term (validateConstraint x 3.0 ((none)) r)) (term (((x "no error")))))

(test-equal
 (term (validateConstraint x 2.01 ((upper = 2)) r))
 (term (((x "upper constraint not met")))))

(test-equal
 (term (validateConstraint x 3.99 ((lower = 4)) r))
 (term (((x "lower constraint not met")))))

(test-equal
 (term (validateConstraint x 2.41 ((lower = 2.4) (upper = 2.42)) r))
 (term (((x "no error") (x "no error")))))

;; v and row-vector constraint tests
(test-equal
 (term (validateConstraint x (3 1 2) ((none)) v))
 (term (((x "no error")) ((x "no error")) ((x "no error")))))

(test-equal
 (term (validateConstraint x (3 1 2) ((upper = 3)) v))
 (term (((x "upper constraint not met")) ((x "no error")) ((x "no error")))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1)) v))
 (term (((x "no error")) ((x "lower constraint not met")) ((x "no error")))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1) (upper = 3)) v))
 (term (
        ((x "no error") (x "upper constraint not met")) ; 3
        ((x "lower constraint not met") (x "no error")) ; 1
        ((x "no error") (x "no error")))))              ; 2

;; simplex constraint tests

(test-results)