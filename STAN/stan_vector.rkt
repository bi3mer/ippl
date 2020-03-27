#lang racket
(require redex)
(require "stan_bnf.rkt")

;; initialize a vector of a given size
(define-metafunction STAN
  vector->init : int -> (number ...)
  [(vector->init int) () (side-condition (<= (term int) 0))]
  [(vector->init int)
   ,(cons 0.0 (term (vector->init ,(- (term int) 1))))])

;; get the size of a vector with the public function vector->size and the private
;; vector->size-private. Private function does all the work here since we
;; accumulate on the last argument which is an int and represents the size of
;; the array.
(define-metafunction STAN
  vector->size-private : (number ...) int -> int
  [(vector->size-private () int) int]
  [(vector->size-private (number_head number_rest ...) int)
   (vector->size-private (number_rest ...) ,(+ 1 (term int)))])

(define-metafunction STAN
  vector->size : (number ...) -> int
  [(vector->size (number ...)) (vector->size-private (number ...) 0)])

;; given an index, get the correct value. Note that Stan is 0 indexed so the
;; implementation of this is as well. It hurts me too.
;; As a note, this will throw an excepion for out of range which is expected
;; behavior. We want to throw an error when this happens.
(define-metafunction STAN
  vector->get : (number ...) int -> number
  [(vector->get (number number_rest ...) 1) number]
  [(vector->get (number number_rest ...) int)
   (vector->get (number_rest ...) ,(- (term int) 1))])

;; add a number to every member the vector
(define-metafunction STAN
  vector->add-const : (number ...) number -> (number ...)
  [(vector->add-const () number) ()]
  [(vector->add-const (number number_rest ...) number_const)
   ,(cons
     (+ (term number) (term number_const))
     (term (vector->add-const (number_rest ...) number_const)))])

;; subtract a number to every member the vector (v[i] - num) for all i in v.
(define-metafunction STAN
  vector->subtract-const : (number ...) number -> (number ...)
  [(vector->subtract-const () number) ()]
  [(vector->subtract-const (number number_rest ...) number_const)
   ,(cons
     (- (term number) (term number_const))
     (term (vector->subtract-const (number_rest ...) number_const)))])

;; multiple a number to every member of the vector
(define-metafunction STAN
  vector->multiply-const : (number ...) number -> (number ...)
  [(vector->multiply-const () number) ()]
  [(vector->multiply-const (number number_rest ...) number_const)
   ,(cons
     (* (term number) (term number_const))
     (term (vector->multiply-const (number_rest ...) number_const)))])

;; divide a number to every member of the vector
(define-metafunction STAN
  vector->divide-const : (number ...) number -> (number ...)
  [(vector->divide-const () number) ()]
  [(vector->divide-const (number number_rest ...) number_const)
   ,(cons
     (/ (term number) (term number_const))
     (term (vector->divide-const (number_rest ...) number_const)))])

;; add arrays to build a new one
(define-metafunction STAN
  vector->add-vectors-private : vec vec -> vec
  [(vector->add-vectors-private () ()) ()]
  [(vector->add-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (+ (term number_1) (term number_2))
     (term (vector->add-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->add-vectors : vec vec -> vec
  [(vector->add-vectors vec_1 vec_2)
   (vector->add-vectors-private vec_1 vec_2)
   (side-condition (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2))))])

;; subtract arrays to build a new one. 
(define-metafunction STAN
  vector->subtract-vectors-private : vec vec -> vec
  [(vector->subtract-vectors-private () ()) ()]
  [(vector->subtract-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (- (term number_1) (term number_2))
     (term (vector->subtract-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->subtract-vectors : vec vec -> vec
  [(vector->subtract-vectors vec_1 vec_2)
   (vector->subtract-vectors-private vec_1 vec_2)
   (side-condition (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2))))])

;; multiply arrays to build a new one
(define-metafunction STAN
  vector->multiply-vectors-private : vec vec -> vec
  [(vector->multiply-vectors-private () ()) ()]
  [(vector->multiply-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (* (term number_1) (term number_2))
     (term (vector->multiply-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->multiply-vectors : vec vec -> vec
  [(vector->multiply-vectors vec_1 vec_2)
   (vector->multiply-vectors-private vec_1 vec_2)
   (side-condition (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2))))])

;; divide arrays to build a new one
(define-metafunction STAN
  vector->divide-vectors-private : vec vec -> vec
  [(vector->divide-vectors-private () ()) ()]
  [(vector->divide-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (/ (term number_1) (term number_2))
     (term (vector->divide-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->divide-vectors : vec vec -> vec
  [(vector->divide-vectors vec_1 vec_2)
   (vector->divide-vectors-private vec_1 vec_2)
   (side-condition (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2))))])


(provide vector->init)
(provide vector->size)
(provide vector->get)
(provide vector->add-const)
(provide vector->subtract-const)
(provide vector->multiply-const)
(provide vector->divide-const)
(provide vector->add-vectors)
(provide vector->subtract-vectors)
(provide vector->multiply-vectors)
(provide vector->divide-vectors)