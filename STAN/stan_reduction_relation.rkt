#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_matrix.rkt")
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

   ; create a matrix
   (--> [(in-hole E (mat-type int_1 int_2 C x)) σ]
        [(in-hole E skip) (env->createVar σ x (matrix->init int_1 int_2) C mat-type)]
        assign-matrix)

   ; update variable value for a number
   (--> [(in-hole E (x = pv)) σ]
        [(in-hole E skip) (env->updateNumber σ x pv)]
        update-variable-int-or-num)

   ; update variable value for a vector
   (--> [(in-hole E (x = vec)) σ]
        [(in-hole E skip) (env->updateVector σ x vec)]
        update-variable-vector)

   ; update variable value for a matrix
   (--> [(in-hole E (x = mat)) σ]
        [(in-hole E skip) (env->updateMatrix σ x mat)]
        update-variable-matrix)

   ; update vector value
   (--> [(in-hole E (x < int > = pv)) σ]
        [(in-hole E skip) (env->updateVectorValue σ x int pv)] 
        update-vector-variable-value)

   ; update matrix vector
   (--> [(in-hole E (x < int > = vec)) σ]
        [(in-hole E skip) (env->updateMatrixVector σ x int vec)]
        update-matrix-vector)

   ; update matrix vector value
   (--> [(in-hole E (x < int_col int_row > = pv)) σ]
        [(in-hole E skip) (env->updateMatrixVectorValue σ x int_col int_row pv)]
        update-matrix-vector-value)
   
   ; get a variable
   (--> [(in-hole E x) σ]
       [(in-hole E (env->getValue σ x)) σ]
       get-value)

   ;; index a vector
   (--> [(in-hole E (vec < int >)) σ]
        [(in-hole E (vector->get vec int)) σ]
        index-vector)

   ;; index a matrix
   (--> [(in-hole E (mat < int >)) σ]
        [(in-hole E (matrix->getVector mat int)) σ]
        index-matrix)

   ;; get matrix value
   (--> [(in-hole E (mat < int_1 int_2 >)) σ]
        [(in-hole E (matrix->getValue mat int_1 int_2)) σ]
        double-index-matrix)

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

   ;; boolean math
   (--> [(in-hole E (pv_1 MBO pv_2)) σ]
        [(in-hole E (meta->booleanOperators pv_1 MBO pv_2)) σ]
        boolean-operator)

   ;; if
   (-->[(in-hole E (if #t then s_1 else s_2)) σ]
       [(in-hole E s_1) σ]
       if-true)
   
   (-->[(in-hole E (if #f then s_1 else s_2)) σ]
       [(in-hole E s_2) σ]
       if-false)

   ;; for
   (-->[(in-hole E (for x in int_1 : int_2 do s)) σ]
       [(in-hole E
                 (if (int_1 <= int_2)
                     then
                      (s (for x in ,(+ 1 (term int_1)) : int_2 do s))
                     else
                      skip))
        (env->updateNumber σ x int_1)]
       for-loop)
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