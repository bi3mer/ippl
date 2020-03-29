#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")
(require "stan_vector.rkt")

;; get the environment from the result of an apply reduction relation
(define-metafunction STAN_E
  meta->getEnvironment : (s σ) -> σ
  [(meta->getEnvironment (s σ)) σ])

;; math operations for stan with reals and integers
(define-metafunction STAN
  meta->mathOperation : pv MO pv -> pv
  [(meta->mathOperation pv_1 + pv_2) ,(+ (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 - pv_2) ,(- (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 * pv_2) ,(* (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 / pv_2) ,(/ (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 ^ pv_2) ,(expt (term pv_1) (term pv_2))]
  [(meta->mathOperation pv_1 % pv_2) ,(modulo (term pv_1) (term pv_2))])

;; math operations for stan with vectors
(define-metafunction STAN
  meta->vectorMathOperation : vec AMO vec -> vec
  [(meta->vectorMathOperation vec_1 ./ vec_2) (vector->divide-vectors vec_1 vec_2)]
  [(meta->vectorMathOperation vec_1 .* vec_2) (vector->multiply-vectors vec_1 vec_2)])

;; exports
(provide meta->vectorMathOperation)
(provide meta->getEnvironment)
(provide meta->mathOperation)