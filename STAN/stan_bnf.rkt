#lang racket
(require redex)

;; TODO: vector
;; TODO: matrix
;; TODO: array
;; TODO: test vector bounding
;; TODO: test matrix bounding
;; TODO: test array bounding
;; TODO: arithmetic operations
;; TODO: int to r but not r to int
;; TODO: for loop
;; TODO: if test

;; NOTES:
;; - the current approach for restrictions does allow a user to create multiple
;;   restrictions of the same type for a variable. For example, there is
;;   nothing wrong with giving a variable 100 lower bounds. The implementation
;;   will only use the last one but this could be seen as a limitation to the
;;   implementation provided.
;; - syntax simplified so a variable has to be declared before its value can be
;;   assigned.
;; - syntax simplified for [][] case to be [ number ... ] to simplify the AST to
;;   be more manageable.
;; - for now, removing matrix
;; - for now, removing array

;;;; Stan language
(define-language STAN
  (e ::= pv x (e ...) (e [e]) (e MO e) (e AMO e) (- e)) ;; 
  (s ::=
     skip
     (i C x)
     (r C x)
     (vec-type integer C x) ;; type of vector, size of vector, constraints, and name.
     (x = e)
     (x [ e ... ] = e)
     (s ...)) ;; (for(x in e : e) s) ((for x in e) s) (if e s then s) 
  
  ;; primative values
  (int ::= integer)
  (pv ::= int number)

  ;; vector
  (vec-type ::= v simplex ordered positive-ordered row-vector)
  (vec ::= (number ...))

  ;; types
  (t ::= i r vec-type a) ; m,,, int real vector array matrix

  ;; matrix
  ;(matrix-types ::= matrix cov-matrix cholesky-factor-cov cholesky-factor-corr)

  ;; Values
  (V ::= pv vec)

  ;; Environment Values
  (EV ::= V x)
  
  ;; Constraints. Non-relevant constraints will not be used in the final checking
  (C ::=
      none
      (lower = number)
      (upper = number)
      (offset = number)
      (multiplier = number)
      (C ...))

  ;; Math Operators
  (MO ::= + - * / ^ %)

  ;; Array Math Operators
  (AMO ::= .* ./)

  ;; Math Boolean Operators
  (MBO ::= > >= <= <=)

  ;; Boolean Operators
  (BO ::= == !=)

  ;; Operation Semantics
  (E ::=
     hole
     (skip ... E s ...)
     (x = E)
     (E MO e)
     (pv MO E)
     (- E)
     (E AMO e)
     (vec AMO E))

  ;; variable
  (x ::= variable-not-otherwise-mentioned))

(provide STAN)