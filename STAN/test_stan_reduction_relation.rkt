#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_metafunctions.rkt")
(require "stan_reduction_relation.rkt")

;; create variable tests
(define ct1 (term (stan->run (i none x))))
(test-equal
 (term (meta->getEnvironment ,ct1))
 (term ((x 0 none i))))

(define ct2 (term (stan->run (r (upper = 0) x))))
(test-equal
 (term (meta->getEnvironment ,ct2))
 (term ((x 0.0 (upper = 0) r))))

(define ct3 (term (stan->run (v 3 none x))))
(test-equal
 (term (meta->getEnvironment ,ct3))
 (term ((x (0.0 0.0 0.0) none v))))

(define ct4 (term (stan->run (simplex 3 none x))))
(test-equal
 (term (meta->getEnvironment ,ct4))
 (term ((x (0.0 0.0 0.0) none simplex))))

(define ct5 (apply-reduction-relation* stan_r (term (((i none x) (r none x)) ()))))
(test-equal
 ct5
 (term (((skip skip) "cannot create variable that already exists"))))

;; update variable tests
(define gt1 (term (stan->run ((i none x) (x = 4)))))
(test-equal
 (term (meta->getEnvironment ,gt1))
 (term ((x 4 none i))))

(define gt2 (term (stan->run ((r none x) (x = 4.0)))))
(test-equal
 (term (meta->getEnvironment ,gt2))
 (term ((x 4.0 none r))))

; this needs to be an error!
(define gt3 (term (stan->run ((i none x) (x = 4.12)))))
(test-equal
 (term (meta->getEnvironment ,gt3))
 (term ((x 4.12 none i))))

(test-results)

