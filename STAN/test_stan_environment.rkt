#lang racket
(require redex)
(require "stan_metafunctions.rkt")
(require "stan_environment.rkt")
(require "stan_bnf.rkt")

;; test env->createVar
(define cv1 (term (env->createVar () x 3.0 (upper = 0) r)))
(test-equal cv1 (term ((x 3.0 (upper = 0) r))))

(define cv2 (term (env->createVar ,cv1 y (3.0 2.0) none v)))
(test-equal cv2 (term ((y (3.0 2.0) none v) (x 3.0 (upper = 0) r))))

; will throw an error
;(term (env->createVar ,cv2 y 3 none i))

;; test env->updateVar
(define uv1 (term (env->updateVar ((x 3.0 none r)) x -1.0)))
(test-equal uv1 (term ((x -1.0 none r))))

(define uv2 (term (env->updateVar ,cv1 x 1.0)))
(test-equal uv2 (term ((x 1.0 (upper = 0) r))))

(define uv3 (term (env->updateVar ,cv2 y (3.0 5.0))))
(test-equal uv3 (term ((y (3.0 5.0) none v) (x 3.0 (upper = 0) r))))

; will throw an error
;(term (env->updateVar ,uv3 z 3.0))

;; test env->getType
(test-equal (term (env->getType ,uv3 y)) (term v))
(test-equal (term (env->getType ,uv3 x)) (term r))

;; test env->getConstraints
(test-equal (term (env->getConstraints ,uv3 y)) (term none))
(test-equal (term (env->getConstraints ,uv3 x)) (term (upper = 0)))

;; test env->getValue
(test-equal (term (env->getValue ,uv3 y)) (term (3.0 5.0)))
(test-equal (term (env->getValue ,uv3 x)) (term 3.0))

(test-results)