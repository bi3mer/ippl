#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_metafunctions.rkt")
(require "stan_reduction_relation.rkt")

;; create variable tests
(define ct1 (term (stan->run (i none x))))
(test-equal (term (meta->getEnvironment ,ct1)) (term ((x 0 none i))))

(define ct2 (term (stan->run (r (upper = 0) x))))
(test-equal (term (meta->getEnvironment ,ct2)) (term ((x 0.0 (upper = 0) r))))

(define ct3 (term (stan->run (v 3 none x))))
(test-equal (term (meta->getEnvironment ,ct3)) (term ((x (0.0 0.0 0.0) none v))))

(define ct4 (term (stan->run (simplex 3 none x))))
(test-equal (term (meta->getEnvironment ,ct4)) (term ((x (0.0 0.0 0.0) none simplex))))

;; update variable tests


(test-results)

