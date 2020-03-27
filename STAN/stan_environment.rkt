#lang racket
(require redex)
(require "stan_bnf.rkt")

;; extended Stan language to contain an environment for variables.
(define-extended-language STAN_E STAN
  (σ ::=
     ((x EV C t) ...)
     (variable-not-found-error x)
     (variable-already-initialized x)))

;; add to the environment or update a value
(define-metafunction STAN
  extend : ((x EV C t) ...) x EV C t -> ((x EV C t) ...)
  ;; empty list case
  [(extend () x EV C t ) ((x EV C t)) ]
  ;; overwritting current variable
  ;[(extend ((x EV C t) (x_rest EV_rest C_rest t_rest) ...) x_1 EV_1 C_1 t_1)
  ;  ((x EV_1 C_1 t_1) (x_rest EV_rest C_rest t_rest) ...)
  ;  (side-condition (eqv? (term x) (term x_1)))]
  ;; Moving over to next entry since current variable is not relevant
  [(extend ((x EV C t) (x_rest EV_rest C_rest t_rest) ...) x_1 EV_1 C_1 t_1)
    ,(cons (term (x EV C t)) (term (extend ((x_rest EV_rest) ...) x_1 EV_1 C_1 t_1)))])

;; from the environment, retrieve the value for the given variable name
(define-metafunction STAN
  getValue : ((x EV C t) ...) x -> EV
  [(getValue (_ ... (x EV C t) _ ...) x) EV])

;; from the environment, retrieve the constraint for the given variable name
(define-metafunction STAN
  getConstraints : ((x EV C t) ...) x -> C
  [(getConstraints (_ ... (x EV C t) _ ...) x) C])

;; from the environment, retrieve the type for the given variable name
(define-metafunction STAN
  getType : ((x EV C t) ...) x -> t
  [(getType (_ ... (x EV C t) _ ...) x) t])

;; returnt rue if the variable type is the same as the EV type
(define-metafunction STAN_E
  typeMatch : V t -> boolean
  [(typeMatch V i) ,(integer? (term V))]
  [(typeMatch V r) ,(number? (term V))]
  [(typeMatch V vec-type) ,(vector? (term V))])

;; set a variables value. Will return an error environment if the variable is
;; not found or the type is wrong. 
(define-metafunction STAN_E
  setVar : σ x EV -> σ
  [(setVar σ x EV) (extend σ x EV) (side-condition (term (exists σ x)))]
  [(setVar σ x EV) (variable-not-found-error x)])

(define one (term (extend () x 3 none r)))
one
;(define two (term (extend ,one x 3.0 (upper = 3) r)))
;two
(define three (term (extend ,one y 3.0 (upper = 3) r)))
three
(term (getValue ,three y))
(term (getConstraints ,three y))
(term (getType ,three y))
(test-equal (term (typeMatch 3 i)) #t)
(test-equal (term (typeMatch 3.01 i)) #f)
(test-equal (term (typeMatch 3 r)) #t)
(test-equal (term (typeMatch 3.01 r)) #t)
;(test-equal (term (typeMatch (vector 3 12) v)) #t)

"should be true"
(redex-match? STAN vec (term (3 1 2 150.0)))


(test-results)







(define-metafunction STAN_E
  createVar : σ x EV -> σ
  [(createVar σ x EV) (extend σ x EV) (side-condition (not (term (exists σ x))))]
  [(createVar σ x EV) (variable-already-initialized x)])