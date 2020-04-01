#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")
(require "stan_matrix.rkt")
(require "stan_environment.rkt")

;;;;;;;;;;;;;;;;;;;; constraint restrictions ;;;;;;;;;;;;;;;;;;;;
;; return list for each constraint and if the error occurred
(define-metafunction STAN_E
  validateNumberConstraints : x EV C -> ((x error) ...)
  [(validateNumberConstraints x EV ()) ()]
  ;; none constraint
  [(validateNumberConstraints x EV ((none) IC_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (IC_rest ...))))]
  ;; upper constraint when side condition for upper met
  [(validateNumberConstraints x EV ((upper = pv) IC_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (IC_rest ...))))
   (side-condition (< (term EV) (term pv)))]
  ;; upper constraint when side condition for upper is not met
  [(validateNumberConstraints x EV ((upper = pv) IC_rest ...))
   ,(cons
     (term (x "upper constraint not met"))
     (term (validateNumberConstraints x EV (IC_rest ...))))]
  ;; lower constraint when side condition for upper met
  [(validateNumberConstraints x EV ((lower = pv) IC_rest ...))
   ,(cons (term (x "no error")) (term (validateNumberConstraints x EV (IC_rest ...))))
   (side-condition (> (term EV) (term pv)))]
  ;; lower constraint when side condition for upper is not met
  [(validateNumberConstraints x EV ((lower = pv) IC_rest ...))
   ,(cons
     (term (x "lower constraint not met"))
     (term (validateNumberConstraints x EV (IC_rest ...))))])

;; Same as above except it runs for each element of the vector
(define-metafunction STAN_E
  validateVectorConstraints : x vec C -> (((x error) ...) ...)
  [(validateVectorConstraints x () C) ()]
  [(validateVectorConstraints x (pv_h pv_t ...) C)
   ,(cons
     (term (validateNumberConstraints x pv_h C))
     (term (validateVectorConstraints x (pv_t ...) C)))])

(define-metafunction STAN_E
  validateMatrixConstraints : x mat C -> ((((x error) ...) ...) ...)
  [(validateMatrixConstraints x () C) ()]
  [(validateMatrixConstraints x (vec_h vec_t ...) C)
   ,(cons
     (term (validateVectorConstraints x vec_h C))
     (term (validateMatrixConstraints x (vec_t ...) C)))])

;; just a pattern maching function to match to type and it's sepcifics
(define-metafunction STAN_E
  validateConstraint : x EV C t -> ((x error) ((((x error) ...) ...) ...))
  [(validateConstraint x EV C i)
   ((x "no type specific constraint") (((validateNumberConstraints x EV C))))]
  [(validateConstraint x EV C r)
   ((x "no type specific constraint") (((validateNumberConstraints x EV C))))]
  [(validateConstraint x EV C v)
   ((x "no type specific constraint") ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C row-vector)
   ((x "no type specific constraint") ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C ordered)
   ((x (vector->ordered EV)) ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C simplex)
   ((x (vector->simplex EV)) ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C positive-ordered)
   ((x (vector->positiveOrdered EV)) ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C unit-vector)
   ((x (vector->unitVector EV)) ((validateVectorConstraints x EV C)))]
  [(validateConstraint x EV C m)
   ((x "no type constraint") (validateMatrixConstraints x EV C))]
  [(validateConstraint x EV C alwaysone)
   ((x (matrix->onlyOnes EV)) (validateMatrixConstraints x EV C))])

;; validate every variable in the environment based on constraints
(define-metafunction STAN_E
  constraints->validate : σ -> (((x error) ((((x error) ...) ...) ...)) ...)
  [(constraints->validate ()) ()]
  [(constraints->validate ((x_h EV_h C_h t_h) (x_t EV_t C_t t_t) ...))
   ,(cons
     (term (validateConstraint x_h EV_h C_h t_h))
     (term (constraints->validate ((x_t EV_t C_t t_t) ...))))])

;;;;;;;;;;;;;;;;;;;; constraint updates ;;;;;;;;;;;;;;;;;;;;

;; update for real numbers
(define-metafunction STAN_E
  updateReal : EV C -> EV
  [(updateReal EV ()) EV]
  [(updateReal EV ((offset = number_o : multiplier = number_m) IC ...))
   ,(+ (term number_o) (* (term EV) (term number_m)))]
  [(updateReal EV (IC_h IC_t ...)) (updateReal EV (IC_t ...))])

;; update for vectors
(define-metafunction STAN_E
  updateVector : vec C -> vec
  [(updateVector () C) ()]
  [(updateVector (number) C) ((updateReal number C))]
  [(updateVector (number_h number_t ...) C)
   ,(cons
     (term (updateReal number_h C))
     (term (updateVector (number_t ...) C)))])

;; update for matrices
(define-metafunction STAN
  updateMatrix : (vec ...) C -> (vec ...)
  ; empty case
  [(updateMatrix () C) ()]
  ; list is still going
  [(updateMatrix (vec_h vec_t ...) C)
   ,(cons
     (term (updateVector vec_h C))
     (term (updateMatrix (vec_t ...) C)))])

;; update for types
(define-metafunction STAN_E
  update : x EV C t -> (x EV C t)
  [(update x EV C i) (x EV C i)]
  [(update x EV C r) (x (updateReal EV C) C r)]
  [(update x EV C v) (x (updateVector EV C) C v)]
  [(update x EV C simplex) (x (updateVector EV C) C simplex)]
  [(update x EV C ordered) (x (updateVector EV C) C ordered)]
  [(update x EV C positive-ordered) (x (updateVector EV C) C positive-ordered)]
  [(update x EV C row-vector) (x (updateVector EV C) C row-vector)]
  [(update x EV C unit-vector) (x (updateVector EV C) C unit-vector)]
  [(update x EV C m) (x (updateMatrix EV C) C m)]
  [(update x EV C alwaysone) (x (updateMatrix EV C) C alwaysone)])

;; update real numbers with offset and multiplier if they have it.
(define-metafunction STAN_E
  constraints->update : σ -> σ
  [(constraints->update ()) ()]
   [(constraints->update ((x_h EV_h C_h t_h) (x_t EV_t C_t t_t) ...))
   ,(cons
     (term (update x_h EV_h C_h t_h))
     (term (constraints->update ((x_t EV_t C_t t_t) ...))))])

;; exports
(provide validateConstraint) ; won't be used but nice for unit testing
(provide constraints->validate)
(provide constraints->update)