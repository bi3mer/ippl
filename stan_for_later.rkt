#lang racket


;;;; Type Checking
;; Stan Γ
(define-extended-language STAN_TC STAN
  (Γ ::= ((x t) ...)))

(define-metafunction STAN
  extendΓ : ((x t) ...) x t -> ((x t) ...)
  [(extendΓ () x t) ((x V))]
  [(extendΓ ((x t) (x_rest t_rest) ...) x_1 t_1)
    ((x t_1) (x_rest t_REST) ...)
    (side-condition (eqv? (term x) (term x_1)))]
  [(extendΓ ((x t) (x_rest t_REST) ...) x_1 t_1)
    ,(cons (term (x t)) (term (extendΓ ((x_rest t_rest) ...) x_1 t_1)))])

(define-metafunction STAN
  lookupΓ : ((x t) ...) x -> t
  [(lookupΓ (_ ... (x t) _ ...) x) t])


(define-judgment-form STAN_TC
  #:mode (⊢ I I O)
  #:contract (⊢ Γ s t)
  
  [----------------------- "int"
   (⊢ Γ n i)]

   [----------------------- "real"
   (⊢ Γ n r)]

   [----------------------- "vector"
   (⊢ Γ n v)]

   [----------------------- "matrix"
   (⊢ Γ n m)]

   [----------------------- "array"
   (⊢ Γ n a)]
 
  [----------------------- "math operator"
   (⊢ Γ AO (int int -> int))]
 
  [----------------------- "variable"
   (⊢ Γ x (lookupΓ Γ x))]
 
  [(⊢ (extendΓ Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
 
  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])