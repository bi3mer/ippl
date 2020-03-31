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

(provide matrix->init)
(provide matrix->numRows)
(provide matrix->numCols)