#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")

;; return list for each constraint and if the error occurred
(define-metafunction STAN_E
  validateNumberConstraints : x EV C -> ((x error) ...)
  [(validateNumberConstraints x EV ()) ()]
  ;; none constraint
  [(validateNumberConstraints x EV ((none) C_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (C_rest ...))))]
  ;; upper constraint when side condition for upper met
  [(validateNumberConstraints x EV ((upper = pv) C_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (C_rest ...))))
   (side-condition (< (term EV) (term pv)))]
  ;; upper constraint when side condition for upper is not met
  [(validateNumberConstraints x EV ((upper = pv) C_rest ...))
   ,(cons
     (term (x "upper constraint not met"))
     (term (validateNumberConstraints x EV (C_rest ...))))]
  ;; lower constraint when side condition for upper met
  [(validateNumberConstraints x EV ((lower = pv) C_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (C_rest ...))))
   (side-condition (> (term EV) (term pv)))]
  ;; lower constraint when side condition for upper is not met
  [(validateNumberConstraints x EV ((lower = pv) C_rest ...))
   ,(cons
     (term (x "lower constraint not met"))
     (term (validateNumberConstraints x EV (C_rest ...))))])

;; Same as above except it runs for each element of the vector
(define-metafunction STAN_E
  validateVectorConstraints : x vec C -> (((x error) ...) ...)
  [(validateVectorConstraints x () C) ()]
  [(validateVectorConstraints x (pv_h pv_t ...) C)
   ,(cons
     (term (validateNumberConstraints x pv_h C))
     (term (validateVectorConstraints x (pv_t ...) C)))])

;; just a pattern maching function to match to type
(define-metafunction STAN_E
  validateConstraint : x EV C t -> (((x error) ...) ...)
  [(validateConstraint x EV C i) ((validateNumberConstraints x EV C))]
  [(validateConstraint x EV C r) ((validateNumberConstraints x EV C))]
  [(validateConstraint x EV C v) (validateVectorConstraints x EV C)]
  [(validateConstraint x EV C row-vector) (validateVectorConstraints x EV C)]
  [(validateConstraint x EV C ordered)
   ,(cons (term (x (vector->ordered EV))) (term (validateVectorConstraints x EV C)))])

;; below v the rest are cons with an individual meta function besides row vector
;simplex ordered positive-ordered row-vector

(define-metafunction STAN_E
  constraints->validate : σ -> ((x error) ...)
  [(constraints->validate ()) ()]
  [(constraints->validate ((x_h EV_h C_h t_h) (x_t EV_t C_t t_t) ...))
   ,(cons
     (term (validateConstraint x_h EV_h C_h t_h))
     (term (constraints->validate ((x_t EV_t C_t t_t) ...))))])


(define-metafunction STAN_E
  constraints->update : σ -> σ
  [(constraints->update ()) ()])

;; exports
(provide validateConstraint) ; won't be used but nice for unit testing
(provide constraints->validate)
(provide constraints->update)