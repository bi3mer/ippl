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
  [(vector->size (number ...)) (vector->size-private (number ...) 1)])

;; given an index, get the correct value. Note that Stan is 0 indexed so the
;; implementation of this is as well. It hurts me too.
(define-metafunction STAN
  vector->get : (number ...) int -> number or "index out of bounds"
  [(vector->get () int) "index out of bounds"]
  [(vector->get (number number_rest ...) 1) number]
  [(vector->get (number number_rest ...) int)
   (vector->get (number_rest ...) ,(- (term int) 1))])

;; Returns true if the index is in the bounds of the vector else false.
(define-metafunction STAN
   vector->outOfBounds : (number ...) int -> boolean
  [(vector->outOfBounds (number ...) int)
   ,(or
     (< (term int) 1)
     (>= (term int) (term (vector->size (number ...)))))])

;; given an index, set the value to that index or return an error. It is again
;; 1 indexed and it still hurts me.
(define-metafunction STAN
  set : (number ...) int number -> (number ...)
  [(set (number_h number_rest ...) 1 number) (number number_rest ...)]
  [(set (number_h number_rest ...) int number)
   ,(cons
     (term number_h)
     (term (set (number_rest ...) ,(- (term int) 1) number)))])
 
(define-metafunction STAN
  vector->set : (number ...) int pv -> (number ...) or "index out of bounds"
  [(vector->set (number ...) int number_1)
   "index out of bounds"
   (side-condition (term (vector->outOfBounds (number ...) int)))]
  [(vector->set (number_h ...) int number) (set (number_h ...) int number)])

(define vs4 (term (vector->set (1 2 3 4) -1 3)))
  
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
  vector->add-vectors : vec vec -> vec or "vectors must be of the same size"
  [(vector->add-vectors vec_1 vec_2)
   "vectors must be of the same size"
   (side-condition (not (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2)))))]
  [(vector->add-vectors vec_1 vec_2) (vector->add-vectors-private vec_1 vec_2)])

;; subtract arrays to build a new one. 
(define-metafunction STAN
  vector->subtract-vectors-private : vec vec -> vec
  [(vector->subtract-vectors-private () ()) ()]
  [(vector->subtract-vectors-private 
   (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (- (term number_1) (term number_2))
     (term (vector->subtract-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->subtract-vectors : vec vec -> vec or "vectors must be of the same size"
  [(vector->subtract-vectors vec_1 vec_2)
   "vectors must be of the same size"
   (side-condition (not (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2)))))]
  [(vector->subtract-vectors vec_1 vec_2)
   (vector->subtract-vectors-private vec_1 vec_2)])

;; multiply arrays to build a new one
(define-metafunction STAN
  vector->multiply-vectors-private : vec vec -> vec  
  [(vector->multiply-vectors-private () ()) ()]
  [(vector->multiply-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (* (term number_1) (term number_2))
     (term (vector->multiply-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->multiply-vectors : vec vec -> vec or "vectors must be of the same size"
  [(vector->multiply-vectors vec_1 vec_2)
   "vectors must be of the same size"
   (side-condition (not (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2)))))]
  [(vector->multiply-vectors vec_1 vec_2)
   (vector->multiply-vectors-private vec_1 vec_2)])

;; divide arrays to build a new one
(define-metafunction STAN
  vector->divide-vectors-private : vec vec -> vec  
  [(vector->divide-vectors-private () ()) ()]
  [(vector->divide-vectors-private (number_1 number_1_rest ...) (number_2 number_2_rest ...))
   ,(cons
     (/ (term number_1) (term number_2))
     (term (vector->divide-vectors-private (number_1_rest ...) (number_2_rest ...))))])

(define-metafunction STAN
  vector->divide-vectors : vec vec -> vec or "vectors must be of the same size"
  [(vector->divide-vectors vec_1 vec_2)
    "vectors must be of the same size"
   (side-condition (not (equal?
                    (term (vector->size vec_1))
                    (term (vector->size vec_2)))))]
  [(vector->divide-vectors vec_1 vec_2)
   (vector->divide-vectors-private vec_1 vec_2)])

;; returns a string to indicate whether or not a vector is ordered
(define-metafunction STAN
  vector->ordered : vec -> "vector is ordered" or "vector is not ordered"
  [(vector->ordered ())  "vector is ordered"]
  [(vector->ordered (number))  "vector is ordered"]
  [(vector->ordered (number_h number_1 number_2 ...))
   (vector->ordered (number_1 number_2 ...))
   (side-condition (<= (term number_h) (term number_1)))]
  [(vector->ordered (number_h number_t ...)) "vector is not ordered"])

;; true if vector contains all positive values, else false
(define-metafunction STAN
  vector->onlyPositiveValues : vec -> boolean
  [(vector->onlyPositiveValues ()) #t]
  [(vector->onlyPositiveValues (number_h number_t ...))
   (vector->onlyPositiveValues (number_t ...))
   (side-condition (> (term number_h) 0))]
  [(vector->onlyPositiveValues (number_h number_t ...)) #f])

;; returns string to indicate whether or not a vector is positive ordered or
;; not.
(define-metafunction STAN
  vector->positiveOrdered : vec -> "vector is positive ordered" or 
  "vector is not positive ordered"
  [(vector->positiveOrdered vec) 
  "vector is positive ordered"
  (side-condition
    (and 
      (term (vector->onlyPositiveValues vec))
      (equal?
       (term (vector->ordered vec))
       "vector is ordered")))]
  [(vector->positiveOrdered vec) "vector is not positive ordered"])

;; gets the sum of the vector
(define-metafunction STAN
  vector->sum : vec -> number
  [(vector->sum ()) 0.0]
  [(vector->sum (number)) number]
  [(vector->sum (number_h number_t ...))
   ,(+
     (term number_h)
     (term (vector->sum (number_t ...))))])

;; gets the sum of the vector
(define-metafunction STAN
  vector->squareSum : vec -> number
  [(vector->squareSum ()) 0.0]
  [(vector->squareSum (number)) ,(expt (term number) 2)]
  [(vector->squareSum (number_h number_t ...))
   ,(+
     (expt (term number_h) 2)
     (term (vector->squareSum (number_t ...))))])

;; get the magnitude of a vector
(define-metafunction STAN
  vector->magnitude : vec -> number
  [(vector->magnitude vec) ,(sqrt (term (vector->squareSum vec)))])

;; sum of vector shoudl be one. Returns a string to indicate if this is valid
(define-metafunction STAN
  vector->unitVector : vec -> "vector is a unit vector" or
  "vector is not a unit vector"
  [(vector->unitVector vec)
   "vector is a unit vector"
   (side-condition
    (or
     (equal? (term (vector->magnitude vec)) 1.0)
     (equal? (term (vector->magnitude vec)) 1)))]
  [(vector->unitVector vec) "vector is not a unit vector"])

;; I can't find any clear documentation on how to implement this and am leaving
;; it blank. May change depending on meeting later in the week.
(define-metafunction STAN
  vector->simplex : vec -> "vector is simplex" or "vector is not simplex" or "not implemented"
  [(vector->simplex vec) "not implemented"])

;; exports
(provide vector->init)
(provide vector->size)
(provide vector->get)
(provide vector->outOfBounds)
(provide vector->set)
(provide vector->add-const)
(provide vector->subtract-const)
(provide vector->multiply-const)
(provide vector->divide-const)
(provide vector->add-vectors)
(provide vector->subtract-vectors)
(provide vector->multiply-vectors)
(provide vector->divide-vectors)
(provide vector->ordered)
(provide vector->positiveOrdered)
(provide vector->sum)
(provide vector->squareSum)
(provide vector->magnitude)
(provide vector->unitVector)
(provide vector->simplex)