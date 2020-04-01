#lang racket
(require redex)
(require "stan_metafunctions.rkt")
(require "stan_environment.rkt")
(require "stan_bnf.rkt")

;; test env->createVar
(define cv1 (term (env->createVar () x 3.0 ((upper = 0)) r)))
(test-equal cv1 (term ((x 3.0 ((upper = 0)) r))))

(define cv2 (term (env->createVar ,cv1 y (3.0 2.0) ((none)) v)))
(test-equal cv2 (term ((y (3.0 2.0) ((none)) v) (x 3.0 ((upper = 0)) r))))

(define cv3 (term (env->createVar ,cv2 y 3 ((none)) i)))
(test-equal cv3 "cannot create variable that already exists")

;; test env->updateVar
(define uv1 (term (env->updateVar ((x 3.0 ((none)) r)) x -1.0)))
(test-equal uv1 (term ((x -1.0 ((none)) r))))

(define uv2 (term (env->updateVar ,cv1 x 1.0)))
(test-equal uv2 (term ((x 1.0 ((upper = 0)) r))))

(define uv3 (term (env->updateVar ,cv2 y (3.0 5.0))))
(test-equal uv3 (term ((y (3.0 5.0) ((none)) v) (x 3.0 ((upper = 0)) r))))

;; test env->updateNumber
(define uvn1 (term (env->updateNumber ((x 3 ((none)) i)) x 10.01)))
(test-equal uvn1 "real number cannot be assigned to an integer")

(define uvn2 (term (env->updateNumber ((x 3 ((none)) r)) y 10.01)))
(test-equal uvn2 "cannot update variable that does not exist")

(define uvn3 (term (env->updateNumber ((x 3 ((none)) r)) x 10.01)))
(test-equal uvn3 (term ((x 10.01 ((none)) r))))

(define uvn4 (term (env->updateNumber ((x 3 ((none)) i)) x 10)))
(test-equal uvn4 (term ((x 10 ((none)) i))))

(define uvn5 (term (env->updateNumber ((x 3 ((none)) r)) x 10)))
(test-equal uvn5 (term ((x 10 ((none)) r))))

;; test env->getType
(test-equal (term (env->getType ,uv3 y)) (term v))
(test-equal (term (env->getType ,uv3 x)) (term r))

;; test env->getConstraints
(test-equal (term (env->getConstraints ,uv3 y)) (term ((none))))
(test-equal (term (env->getConstraints ,uv3 x)) (term ((upper = 0))))

;; test env->getValue
(test-equal (term (env->getValue ,uv3 y)) (term (3.0 5.0)))
(test-equal (term (env->getValue ,uv3 x)) (term 3.0))

;; test env->updateVectorValue
(test-equal
 (term (env->updateVectorValue () x 3 3))
 "variable not found")

(test-equal
 (term (env->updateVectorValue ((x 3 ((none)) r)) x 3 3))
 "cannot index a number")

(test-equal
 (term (env->updateVectorValue ((x (3 0 1 1) ((none)) v)) x 1 1.12))
 (term ((x (1.12 0 1 1) ((none)) v))))

(test-equal
 (term (env->updateVectorValue ((x (3 0 1 1) ((none)) v)) x 0 1.12))
 "index out of bounds")

(test-equal
 (term (env->updateVectorValue ((x (3 0 1 1) ((none)) v)) x 5 1.12))
 "index out of bounds")

;; test env->updateVector
(test-equal
 (term (env->updateVector () x (3 3)))
 "variable not found")

(test-equal
 (term (env->updateVector ((x 3 ((none)) i)) x (3 3)))
 "vector can only be assigned to variable of type vector")

(test-equal
 (term (env->updateVector ((x (3 3 3) ((none)) v)) x (3 3)))
 "size cannot change on updating vector variable")

(test-equal
 (term (env->updateVector ((x (3 3) ((none)) v)) x (1 1)))
 (term ((x (1 1) ((none)) v))))


(test-results)