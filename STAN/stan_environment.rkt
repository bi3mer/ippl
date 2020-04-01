#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_matrix.rkt")
(require "stan_vector.rkt")

;; extended Stan language to contain an environment for variables. Made public.
(define-extended-language STAN_E STAN
  (σ ::= ((x EV C t) ...)))

;;;;;;;;;;;;;;;;; Private Functions ;;;;;;;;;;;;;;;;;
;; Add variable to the environment. If this is being called then the
;; variable has already been checked and been shown to not exist. 
(define-metafunction STAN
  addVariable : ((x EV C t) ...) x EV C t -> ((x EV C t) ...)
  [(addVariable ((x_1 EV_1 C_1 t_1) ...) x EV C t)
   ((x EV C t) (x_1 EV_1 C_1 t_1) ...)])

;; Update the value of a variable. If this is being called then
;; the variable has already been shown to exist.
(define-metafunction STAN
  updateVariable : ((x EV C t) ...) x EV -> ((x EV C t) ...)
  ; case where we are on the last variable so they must be equivalent
  ;[(updateVariable ((x_h EV_h C_h t_h)) x EV) ((x_h EV C_h t_h))]
  
  ; update variable since it was found
  [(updateVariable ((x_h EV_h C_h t_h) (x_r EV_r C_r t_r) ...) x EV)
   ((x_h EV C_h t_h) (x_r EV_r C_r t_r) ...)
   (side-condition (eqv? (term x_h) (term x)))]
  
  ; variable not found so keep looking
  [(updateVariable ((x_h EV_h C_h t_h) (x_r EV_r C_r t_r) ...) x EV)
   ,(cons (term (x_h EV_h C_h t_h)) (term (updateVariable ((x_r EV_r C_r t_r) ...) x EV)))])

;; Find if a variable exists.
(define-metafunction STAN
  variableExists : ((x EV C t) ...) x -> boolean
  [(variableExists () x) #f]
  [(variableExists (_ ... (x EV C t) _ ...) x) #t]
  [(variableExists (_ ... (x_any EV C t) _ ...) x) #f])

;; update a number only if the type is correct
(define-metafunction STAN_E
  updateExistingNumber : σ x pv -> σ or "real number cannot be assigned to an integer"
  [(updateExistingNumber σ x pv)
   (updateVariable σ x pv)
   (side-condition
    (and
     (eqv? (term (env->getType σ x)) (term i))
     (integer? (term pv))))]
  [(updateExistingNumber σ x pv)
   (updateVariable σ x pv)
   (side-condition (eqv? (term (env->getType σ x)) (term r)))]
  [(updateExistingNumber σ x pv)  "real number cannot be assigned to an integer"])

;;;;;;;;;;;;;;;;; Public Functions ;;;;;;;;;;;;;;;;;
;; from the environment, retrieve the value for the given variable name.
(define-metafunction STAN
  env->getValue : ((x EV C t) ...) x -> EV
  [(env->getValue (_ ... (x EV C t) _ ...) x) EV])

;; from the environment, retrieve the constraint for the given variable name.
(define-metafunction STAN
  env->getConstraints : ((x EV C t) ...) x -> C
  [(env->getConstraints (_ ... (x EV C t) _ ...) x) C])

;; from the environment, retrieve the type for the given variable name.
(define-metafunction STAN
  env->getType : ((x EV C t) ...) x -> t
  [(env->getType (_ ... (x EV C t) _ ...) x) t])

;; set a variable value if it already exists. If it does not then an error is
;; thrown
(define-metafunction STAN_E
  env->updateVar : σ x EV -> σ or "cannot update variable that does not exist"
  [(env->updateVar σ x EV)
   (updateVariable σ x EV)
   (side-condition (term (variableExists σ x)))]
  [(env->updateVar σ x EV) "cannot update variable that does not exist"])

;; create a variable if it does not already exists. If it does then no clause
;; matches and this is an error state.
(define-metafunction STAN_E
  env->createVar : σ x EV C t -> σ or "cannot create variable that already exists"
  [(env->createVar σ x_1 EV_1 C_1 t_1)
   (addVariable σ x_1 EV_1 C_1 t_1)
   (side-condition (not (term (variableExists σ x_1))))]
  [(env->createVar  σ x_1 EV_1 C_1 t_1) "cannot create variable that already exists"])

;; update a number if it already exists and the value is the correct type.
;; An integer can be assigned to a real but not vise versa.
(define-metafunction STAN_E
  env->updateNumber : σ x pv -> σ or
  "cannot update variable that does not exist" or
  "real number cannot be assigned to an integer"
  [(env->updateNumber σ x pv)
   (updateExistingNumber σ x pv)
   (side-condition (term (variableExists σ x)))]
  [(env->updateNumber σ x pv) "cannot update variable that does not exist"])

;; update a vector value given the variable name, index, and value. I'm not
;; happy with how I ended up handling checking for types. I'm also not happy
;; with how I handled propogating the error. I'm sure there is a better way
;; but I couldn't figure it out in a reasonable time.
(define-metafunction STAN_E
  env->updateVectorValue : σ x int pv -> σ
  or "variable not found"
  or "cannot index a number"
  or "index out of bounds"
  [(env->updateVectorValue σ x int pv)
   "variable not found"
   (side-condition (not (term (variableExists σ x))))]
  [(env->updateVectorValue σ x int pv)
   "cannot index a number"
   (side-condition
    (or
     (eqv? (term (env->getType σ x)) (term i))
     (eqv? (term (env->getType σ x)) (term r))))]
  [(env->updateVectorValue σ x int pv)
   "index out of bounds"
   (side-condition (term (vector->outOfBounds (env->getValue σ x) int)))]
  [(env->updateVectorValue σ x int pv)
   (env->updateVar σ x (vector->set (env->getValue σ x) int pv))])

;; update environment vector by new set. Will be called via .* or ./.
(define-metafunction STAN_E
  env->updateVector : σ x vec -> σ or
  "variable not found" or 
  "vector can only be assigned to variable of type vector" or
  "size cannot change on updating vector variable"
  [(env->updateVector σ x vec)
   "variable not found"
   (side-condition (not (term (variableExists σ x))))]
  [(env->updateVector σ x vec)
   "vector can only be assigned to variable of type vector"
   (side-condition
    (or
     (eqv? (term (env->getType σ x)) (term i))
     (eqv? (term (env->getType σ x)) (term r))))]
  [(env->updateVector σ x vec)
   "size cannot change on updating vector variable"
   (side-condition
    (not (eqv?
     (term (vector->size vec))
     (term (vector->size (env->getValue σ x))))))]
  [(env->updateVector σ x vec) (env->updateVar σ x vec)])

;; update matrix vector value
;; update matrix vector
;; update matrix
(define-metafunction STAN_E
  env->updateMatrix : σ x mat -> σ or
  "variable not found" or 
  "matrix can only be assigned to variable of type matrix" or
  "size cannot change on updating matrix variable"
  ; size check
  [(env->updateMatrix σ x mat)
   "variable not found"
   (side-condition (not (term (variableExists σ x))))]
  ; type check
  [(env->updateMatrix σ x mat)
   "matrix can only be assigned to variable of type matrix"
   (side-condition
    (not
     (or
      (eqv? (term (env->getType σ x)) (term m))
      (eqv? (term (env->getType σ x)) (term onlyones)))))]
  ; size check and run on success
  [(env->updateMatrix σ x mat)
   (env->updateVar σ x mat)
   (side-condition
    (and
     (eqv?
      (term (matrix->numRows mat))
      (term (matrix->numRows (env->getValue σ x))))
     (eqv?
      (term (matrix->numCols mat))
      (term (matrix->numCols (env->getValue σ x))))))]
  ; else return error
  [(env->updateMatrix σ x mat)
   "size cannot change on updating matrix variable"])

;; exports
(provide STAN_E)
(provide env->getValue)
(provide env->getConstraints)
(provide env->getType)
(provide env->updateVar)
(provide env->createVar)
(provide env->updateNumber)
(provide env->updateVectorValue)
(provide env->updateVector)
(provide env->updateMatrix)