#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")
(require "stan_environment.rkt")
(require "stan_metafunctions.rkt")

(define stan_r
  (reduction-relation
   STAN_E
   ;; variable operations
   ; create int
   (--> [(in-hole E (i C x)) σ]
        [(in-hole E skip) (env->createVar σ x 0 C i)] 
        assign-int)
   ; create real
   (--> [(in-hole E (r C x)) σ]
        [(in-hole E skip) (env->createVar σ x 0.0 C r)]
        assign-real)
   ; create vector
   (--> [(in-hole E (vec-type int C x)) σ]
        [(in-hole E skip) (env->createVar σ x (vector->init int) C vec-type)]
        assign-vec)

   ; update variable value for a number
   (--> [(in-hole E (x = pv)) σ]
        [(in-hole E skip) (env->updateNumber σ x pv)]
        update-variable-int-or-num)

   ; update variable value for a vector
   (--> [(in-hole E (x = vec)) σ]
        [(in-hole E skip) (env->updateVector σ x vec)]
        update-variable-vector)

   ; update vector value
   (--> [(in-hole E (x [int] = pv)) σ]
        [(in-hole E skip) (env->updateVectorValue σ x int pv)] 
        update-vector-variable-value)

   ; get a variable
   (--> [(in-hole E x) σ]
       [(in-hole E (env->getValue σ x)) σ]
       get-value)

   ;; index a vector
   (--> [(in-hole E (vec (int))) σ]
        [(in-hole E (vector->get vec int)) σ]
        index-vector)

   ;; math operations
   ; real and ints
   (--> [(in-hole E (pv_1 MO pv_2)) σ]
        [(in-hole E (meta->mathOperation pv_1 MO pv_2)) σ]
        math-operation)

   (--> [(in-hole E (- pv)) σ]
        [(in-hole E (meta->mathOperation pv * -1)) σ]
        math-negative-operation)

   ; vectors
   (--> [(in-hole E (vec_1 AMO vec_2)) σ]
        [(in-hole E (meta->vectorMathOperation vec_1 AMO vec_2)) σ]
        vector-math-operation)

   ;; for

   ;; if
   ))

(define-metafunction STAN_E
  stan->simplifyOut : ((s σ)) -> (s σ)
  [(stan->simplifyOut ((s σ))) (s σ)])

(define-metafunction STAN_E
  stan->run : s -> (s σ)
  [(stan->run s)
   (stan->simplifyOut ,(apply-reduction-relation* stan_r (term (s ()) )))])

(provide stan_r)
(provide stan->run)