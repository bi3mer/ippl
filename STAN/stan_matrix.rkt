#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_vector.rkt")

;; intialize a matrix where the first integer argument represents the length
;; of every row. The second integer represents how many rows there are. Returns
;; a matrix.
(define-metafunction STAN
  matrix->init : int int -> (vec ...) or "bounds error"
  [(matrix->init int_x int_y) () (side-condition (<= (term int_y) 0))]
  [(matrix->init int_x int_y)
   ,(cons
     (term (vector->init int_x))
     (term (matrix->init int_x ,(- (term int_y) 1))))])

;; get number of rows
(define-metafunction STAN
  numRows : (vec ...) int -> int
  [(numRows () int) int]
  [(numRows (vec_h vec_t ...) int)
   (numRows (vec_t ...) ,(+ 1 (term int)))])

(define-metafunction STAN
  matrix->numRows : (vec ...) -> int
  [(matrix->numRows (vec ...)) (numRows (vec ...) 0)])
  
;; get number of columns
(define-metafunction STAN
  matrix->numCols : (vec ...) -> int
  [(matrix->numCols (vec_h vec_t ...)) (vector->size vec_h)])

;; get vector
(define-metafunction STAN
  matrix->getVector : (vec ...) int -> vec or "index out of bounds"
  [(matrix->getVector () int) "index out of bounds"]
  [(matrix->getVector (vec_h vec_t ...) 1) vec_h]
  [(matrix->getVector (vec_h vec_t ...) int)
   (matrix->getVector (vec_t ...) ,(- (term int) 1))])

;; return true if an index can be used to index the matrix else false
(define-metafunction STAN
  matrix->inBounds : (vec ...) int -> boolean
  [(matrix->inBounds (vec ...) int)
   ,(and
     (<= (term int) (term (matrix->numRows (vec ...))))
     (>= (term int) 1))])

;; get value
(define-metafunction STAN
  matrix->getValue : (vec ...) int int -> number or "index out of bounds"
  ; second, y, is not in bounds of matrix
  [(matrix->getValue (vec ...) int_x int_y)
   "index out of bounds"
   (side-condition (not (term (matrix->inBounds (vec ...) int_y))))]
  ; get the vector and vecotr->get will handle future bounds issues
  [(matrix->getValue (vec ...) int_x int_y)
   (vector->get (matrix->getVector (vec ...) int_y) int_x)])

;; set vector where the public function does bounds checking first
(define-metafunction STAN
  setVector : (vec ...) int vec -> (vec ...)
  [(setVector (vec_h vec_t ...) 1 vec) (vec vec_t ...)]
  [(setVector (vec_h vec_t ...) int vec)
   ,(cons (term vec_h) (term (setVector (vec_t ...) ,(- (term int) 1) vec)))])
  
(define-metafunction STAN
  matrix->setVector : (vec ...) int vec -> (vec ...) or
  "index out of bounds" or
  "mismatched vector sizes"
  ; bad index
  [(matrix->setVector (vec ...) int vec_1)
   "index out of bounds"
   (side-condition (not (term (matrix->inBounds (vec ...) int))))]
  
  ; sizes do not match
  [(matrix->setVector (vec ...) int vec_1)
   "mismatched vector sizes"
   (side-condition
    (not (eqv?
          (term (vector->size vec_1))
          (term (matrix->numCols (vec ...))))))]
  
  ; update the matrix with the new vector
  [(matrix->setVector (vec ...) int vec_1)
   (setVector (vec ...) int vec_1)])

;; set value
(define-metafunction STAN
  setValue : (vec ...) int int number -> (vec ...)
  [(setValue (vec_h vec_t ...) int_col 1 number)
   ((vector->set vec_h int_col number) vec_t ...)]
  [(setValue (vec_h vec_t ...) int_col int_row number)
   ,(cons
     (term vec_h)
     (term (setValue (vec_t ...) ,(- (term int_col) 1) int_row number)))])
  
(define-metafunction STAN
  matrix->setValue : (vec ...) int int number -> (vec ...) or "index out of bounds"
  ; index checking for columns and rows
  [(matrix->setValue (vec ...) int_col int_row number)
   "index out of bounds"
   (side-condition
    (or
     (not (term (matrix->inBounds (vec ...) int_row)))
     (or
      (< (term int_col) 1)
      (> (term int_col) (term (matrix->numCols (vec ...)))))))]

  ; update the value
  [(matrix->setValue (vec ...) int_col int_row number)
   (setValue (vec ...) int_col int_row number)])
  

;; always 1 constraint



(provide matrix->init)
(provide matrix->inBounds)
(provide matrix->numRows)
(provide matrix->numCols)
(provide matrix->getVector)
(provide matrix->getValue)
(provide matrix->setVector)
(provide matrix->setValue)