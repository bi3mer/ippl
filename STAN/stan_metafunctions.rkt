#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")
(require "stan_vector.rkt")

;; get the environment from the result of an apply reduction relation
(define-metafunction STAN_E
  meta->getEnvironment : (s σ) -> σ
  [(meta->getEnvironment (s σ)) σ])

;; math operations for stan with reals and integers. Stan beahvior for divide by
;; 0 is undefined.
(define-metafunction STAN
  meta->mathOperation : pv MO pv -> pv or "cannot divide by zero"
  [(meta->mathOperation pv_1 + pv_2) ,(+ (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 - pv_2) ,(- (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 * pv_2) ,(* (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 / pv_2)
   "cannot divide by zero"
   (side-condition (or (eqv? (term pv_2) 0) (eqv? (term pv_2) 0.0)))]
  [(meta->mathOperation pv_1 / pv_2) ,(/ (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 ^ pv_2) ,(expt (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 % pv_2) ,(modulo (term pv_1) (term pv_2))])

;; math operations for stan with vectors
(define-metafunction STAN
  meta->vectorMathOperation : vec AMO vec -> vec or "vectors must be of the same size"
  [(meta->vectorMathOperation vec_1 ./ vec_2) (vector->divide-vectors vec_1 vec_2)]
  [(meta->vectorMathOperation vec_1 .* vec_2) (vector->multiply-vectors vec_1 vec_2)])

;; boolean functions. Vectors and matrices will not work with these.
(define-metafunction STAN
  meta->booleanOperators : pv MBO pv -> boolean
  [(meta->booleanOperators pv_1 > pv_2) ,(> (term pv_1) (term pv_2))]
  [(meta->booleanOperators pv_1 >= pv_2) ,(>= (term pv_1) (term pv_2))]
  [(meta->booleanOperators pv_1 < pv_2) ,(< (term pv_1) (term pv_2))]
  [(meta->booleanOperators pv_1 <= pv_2) ,(<= (term pv_1) (term pv_2))]
  [(meta->booleanOperators pv_1 == pv_2) ,(eqv? (term pv_1) (term pv_2))]
  [(meta->booleanOperators pv_1 != pv_2) ,(not (eqv? (term pv_1) (term pv_2)))])

;; exports
(provide meta->vectorMathOperation)
(provide meta->getEnvironment)
(provide meta->mathOperation)
(provide meta->booleanOperators)