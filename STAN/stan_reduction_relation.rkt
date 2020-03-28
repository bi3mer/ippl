#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")
(require "stan_environment.rkt")

(define stan_r
  (reduction-relation
   STAN_E
   ;; variable operations
   (--> [(in-hole E x) σ]
       [(in-hole E (lookup σ x)) σ]
       findVar)

   (--> [(in-hole E (i C x)) σ]
        [(in-hole E skip) (createVar σ x 0)] ; (extend (extend σ x 0) current x) 
        assign-int)
   (--> [(in-hole E (r C x)) σ]
        [(in-hole E skip) (createVar σ x 0.0)] ; (extend (extend σ x 0.0) current x) 
        assign-real)

   ;; (x [e] = e case is not handled
   (--> [(in-hole E (x = pv)) σ]
        [(in-hole E skip) (setVar σ x pv)]
        update-val)

   ;; math operations
   (--> [(in-hole E (pv_1 MO pv_2)) σ]
        [(in-hole E (mathOperation pv_1 MO pv_2)) σ]
        math-operation)

   (--> [(in-hole E (- pv)) σ]
        [(in-hole E (mathOperation pv * -1)) σ]
        math-negative)

   ;; for

   ;; if
   ))