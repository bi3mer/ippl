#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_constraints.rkt")
(require "stan_environment.rkt")
(require "stan_metafunctions.rkt")
(require "stan_reduction_relation.rkt")

;; This sample shows vector updating and vector multiplication, not the dot
;; product. It also shows the constraints working where one value is set to
;; 30 in the multiplication. But the upper value is 20 and that means that
;; the constraints do not work which is y has one element where the upper
;; constraint is not met. 
(define sample1
  (term
   (stan->run
    ((v 4 ((none)) x)
     (x < 2 > = 3.0)
     (v 4 ((upper = 20.0)) y)
     (y < 2 > = 10.0)
     (y = (x .* y))))))

(test-equal
 (term (meta->getEnvironment ,sample1))
 (term ((y (0.0 30.0 0.0 0.0) ((upper = 20.0)) v) (x (0.0 3.0 0.0 0.0) ((none)) v))))

(test-equal
 (term (constraints->validate (meta->getEnvironment ,sample1)))
 (term
  (((y "no type specific constraint")
     ((((y "no error"))
       ((y "upper constraint not met"))
       ((y "no error"))
       ((y "no error")))))
    ((x "no type specific constraint")
     ((((x "no error"))
       ((x "no error"))
       ((x "no error"))
       ((x "no error"))))))))

;; This sample shows a for loop working. It also shows the order constraint
;; working in the constraints because it states that the y vector is ordered.
(define sample2
  (term
   (stan->run
    ((ordered 4 ((none)) y)
     (i ((none)) x)
     (for x in 1 : 4 do (y < x > = x))))))

(test-equal
 (term (meta->getEnvironment ,sample2))
 (term ((x 5 ((none)) i) (y (1 2 3 4) ((none)) ordered))))

(test-equal
 (term (constraints->validate (meta->getEnvironment ,sample2)))
 (term
  (((x "no type specific constraint") ((((x "no error")))))
    ((y "vector is ordered")
     ((((y "no error"))
       ((y "no error"))
       ((y "no error"))
       ((y "no error"))))))))

;; This sample shows an example of the ordered constraint not being met. Note
;; that a vector starts at initialized with all zeroes.
(define sample3
  (term
   (stan->run
    ((ordered 4 ((none)) y)
     (y < 2 > = -1)))))

(test-equal
 (term (meta->getEnvironment ,sample3))
 (term ((y (0.0 -1 0.0 0.0) ((none)) ordered))))

(test-equal
 (term (constraints->validate (meta->getEnvironment ,sample3)))
 (term
  (((y "vector is not ordered")
     ((((y "no error"))
       ((y "no error"))
       ((y "no error"))
       ((y "no error"))))))))

;; example fibonacci
(define fib
  (term
    ((i ((none)) n)
     (i ((none)) result)
     (n = 9)
     (if (n <= 0)
         then
         (result = 0)
         else
         ((i ((none)) a)
          (i ((none)) index)
          (i ((none)) temp)
          (result = 1)
          (for index in 0 : (n - 2) do
            ((temp = a)
             (a = result)
             (result = (temp + result)))))))))

(define fib_env (term (meta->getEnvironment (stan->run ,fib))))
(test-equal
 (term (env->getValue ,fib_env result))
 34)


(test-results)