#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")
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

(define gt3 (apply-reduction-relation* stan_r (term (((i none x) (x = 4.12)) ()))))
(test-equal
 gt3
 (term (((skip skip) "real number cannot be assigned to an integer"))))

(define gt4 (apply-reduction-relation* stan_r (term (((i none x) (y = 4.12)) ()))))
(test-equal
 gt4
 (term (((skip skip) "cannot update variable that does not exist"))))

;; update vector value tests
(define ucv (term (stan->run ((v 4 none y) (y [3] = 10.0)))))
(test-equal
 (term (meta->getEnvironment ,ucv))
 (term ((y (0.0 0.0 10.0 0.0) none v))))

(define ucv1 (apply-reduction-relation* stan_r (term (((v 4 none y) (y [-1] = 10.0)) ()))))
(test-equal
 ucv1
 (term (((skip skip) "index out of bounds"))))

(define ucv2 (apply-reduction-relation* stan_r (term (((v 4 none y) (y [5] = 10.0)) ()))))
(test-equal
 ucv2
 (term (((skip skip) "index out of bounds"))))

;; get value tests
(define gvt (term (stan->run ((r none x) (x = 3.0) (r none y) (y = x)))))
(test-equal
 (term (meta->getEnvironment ,gvt))
 (term ((y 3.0 none r) (x 3.0 none r))))

(define gvt2 (term (stan->run ((v 4 none x) (x [1] = 3.0) (r none y) (y = (x[1]))))))
(test-equal
 (term (meta->getEnvironment ,gvt2))
 (term ((y 3.0 none r) (x (3.0 0.0 0.0 0.0) none v))))

(test-equal
 (apply-reduction-relation* stan_r (term (((v 4 none x) (x [1] = (x [5]))) () )))
 (term (((skip (x (1) = "index out of bounds")) ((x (0.0 0.0 0.0 0.0) none v))))))

(test-equal
 (apply-reduction-relation* stan_r (term (((v 4 none x) (x [1] = (x [0]))) () )))
 (term (((skip (x (1) = "index out of bounds")) ((x (0.0 0.0 0.0 0.0) none v))))))

;; math tests
(define mt (term (stan->run ((r none x) (x = (3.0 + 3.1)))) ))
(test-equal (term (meta->getEnvironment ,mt)) (term ((x 6.1 none r))))

(define mt1 (term (stan->run ((r none x) (x = (3.1 + x)))) ))
(test-equal (term (meta->getEnvironment ,mt1)) (term ((x 3.1 none r))))

(define mt2 (term (stan->run ((r none x) (x = (x - 3.1)))) ))
(test-equal (term (meta->getEnvironment ,mt2)) (term ((x -3.1 none r))))

(define mt3 (term (stan->run ((r none x) (x = (x / 3)))) ))
(test-equal (term (meta->getEnvironment ,mt3)) (term ((x 0.0 none r))))

(define mt4 (term (stan->run ((r none x) (x = (x * 3)))) ))
(test-equal (term (meta->getEnvironment ,mt4)) (term ((x 0.0 none r))))

(define mt5 (term (stan->run ((r none x) (x = 3.0) (x = (x % 2.0)))) ))
(test-equal (term (meta->getEnvironment ,mt5)) (term ((x 1.0 none r))))

(define mt6 (term (stan->run ((r none x) (x = (2.0 ^ 3.0)))) ))
(test-equal (term (meta->getEnvironment ,mt6)) (term ((x 8.0 none r))))

(define mt7 (term (stan->run ((r none x) (x = (2.0 ^ (3.0 - 1.0))))) ))
(test-equal (term (meta->getEnvironment ,mt7)) (term ((x 4.0 none r))))

;; vector math tests
(define vmt
  (term (stan->run ((v 4 none x)
                    (x [2] = 3.0)
                    (v 4 none y)
                    (y [2] = 10.0)
                    (y = (x .* y))))))
(define vmt_env (term (meta->getEnvironment ,vmt)))
(test-equal (term (env->getValue ,vmt_env y)) (term (0.0 30.0 0.0 0.0)))
(test-equal (term (env->getValue ,vmt_env x)) (term (0.0 3.0 0.0 0.0)))

(define vmt2
  (term (stan->run ((v 4 none x)
                    (x [2] = 10.0)
                    (v 4 none y)
                    (y [1] = 1.0)
                    (y [2] = 5.0)
                    (y [3] = 1.0)
                    (y [4] = 1.0)
                    (y = (x ./ y))))))
(define vmt2_env (term (meta->getEnvironment ,vmt2)))
(test-equal (term (env->getValue ,vmt2_env y)) (term (0.0 2.0 0.0 0.0)))
(test-equal (term (env->getValue ,vmt2_env x)) (term (0.0 10.0 0.0 0.0)))

(define vmt_3_program (term ((v 4 none x)
                             (x [2] = 3.0)
                             (v 5 none y)
                             (y [2] = 10.0)
                             (y = (x .* y)))))
(define vmt3 (apply-reduction-relation* stan_r (term (,vmt_3_program ()))))
(test-equal
 vmt3
 (term
  (((skip skip skip skip (y = "vectors must be of the same size"))
    ((y (0.0 10.0 0.0 0.0 0.0) none v) (x (0.0 3.0 0.0 0.0) none v))))))

(test-results)

