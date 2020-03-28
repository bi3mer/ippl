#lang racket
(require redex)
(require "stan_bnf.rkt")

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
;; the variable ahs already been checked and shown to exist.
(define-metafunction STAN
  updateVariable : ((x EV C t) ...) x EV -> ((x EV C t) ...)
  ; update variable since it was found
  [(updateVariable ((x_h EV_h C_h t_h) (x_r EV_r C_r t_r) ...) x EV)
   ((x_h EV C_h t_h) (x_r EV_r C_r t_r) ...)
   (side-condition (eqv? (term x_h) (term x)))]
  
  ; variable not found so keep looking
  [(updateVariable ((x_h E_h C_h t_h) (x_r EV_r C_r t_r) ...) x EV)
   ,(cons (term (x_h EV_h C_h t_h)) (term (updateVariable ((x_r EV_r C_r t_r) ...) x EV)))])

;; Find if a variable exists.
(define-metafunction STAN
  exists : ((x EV C t) ...) x -> boolean
  [(exists () x) #f]
  [(exists (_ ... (x EV C t) _ ...) x) #t]
  [(exists (_ ... (x_any EV C t) _ ...) x) #f])

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
   (side-condition (term (exists σ x)))]
  [(env->updateVar σ x EV) "cannot update variable that does not exist"])

;; create a variable if it does not already exists. If it does then no clause
;; matches and this is an error state.
(define-metafunction STAN_E
  env->createVar : σ x EV C t -> σ or "cannot create variable that already exists"
  [(env->createVar σ x_1 EV_1 C_1 t_1)
   (addVariable σ x_1 EV_1 C_1 t_1)
   (side-condition (not (term (exists σ x_1))))]
  [(env->createVar  σ x_1 EV_1 C_1 t_1) "cannot create variable that already exists"])

;; exports
(provide STAN_E)
(provide env->getValue)
(provide env->getConstraints)
(provide env->getType)
(provide env->updateVar)
(provide env->createVar)