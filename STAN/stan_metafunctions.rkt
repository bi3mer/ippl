#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")

;; get the environment from the result of an apply reduction relation
(define-metafunction STAN_E
  meta->getEnvironment : (s σ) -> σ
  [(meta->getEnvironment (s σ)) σ])

(provide meta->getEnvironment)