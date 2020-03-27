#lang racket
(require redex)
(require math/matrix) ;; may be able to do something with this.

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
  (pv ::= integer number)

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

;; extended Stan language to contain an environment for variables.
(define-extended-language STAN_E STAN
  (σ ::=
     ((x EV C t) ...)
     (variable-not-found-error x)
     (variable-already-initialized x)))

;; add to the environment or update a value
(define-metafunction STAN
  extend : ((x EV C t) ...) x EV C t -> ((x EV C t) ...)
  ;; empty list case
  [(extend () x EV C t ) ((x EV C t)) ]
  ;; overwritting current variable
  #;[(extend ((x EV C t) (x_rest EV_rest C_rest t_rest) ...) x_1 EV_1 C_1 t_1)
    ((x EV_1 C_1 t_1) (x_rest EV_rest C_rest t_rest) ...)
    (side-condition (eqv? (term x) (term x_1)))]
  ;; Moving over to next entry since current variable is not relevant
  [(extend ((x EV C t) (x_rest EV_rest C_rest t_rest) ...) x_1 EV_1 C_1 t_1)
    ,(cons (term (x EV C t)) (term (extend ((x_rest EV_rest) ...) x_1 EV_1 C_1 t_1)))])

;; from the environment, retrieve the value for the given variable name
(define-metafunction STAN
  getValue : ((x EV C t) ...) x -> EV
  [(getValue (_ ... (x EV C t) _ ...) x) EV])

;; from the environment, retrieve the constraint for the given variable name
(define-metafunction STAN
  getConstraints : ((x EV C t) ...) x -> C
  [(getConstraints (_ ... (x EV C t) _ ...) x) C])

;; from the environment, retrieve the type for the given variable name
(define-metafunction STAN
  getType : ((x EV C t) ...) x -> t
  [(getType (_ ... (x EV C t) _ ...) x) t])

;; returnt rue if the variable type is the same as the EV type
(define-metafunction STAN_E
  typeMatch : V t -> boolean
  [(typeMatch V i) ,(integer? (term V))]
  [(typeMatch V r) ,(number? (term V))]
  [(typeMatch V vec-type) ,(vector? (term V))])

;; set a variables value. Will return an error environment if the variable is
;; not found or the type is wrong. 
(define-metafunction STAN_E
  setVar : σ x EV -> σ
  [(setVar σ x EV) (extend σ x EV) (side-condition (term (exists σ x)))]
  [(setVar σ x EV) (variable-not-found-error x)])

(define one (term (extend () x 3 none r)))
one
;(define two (term (extend ,one x 3.0 (upper = 3) r)))
;two
(define three (term (extend ,one y 3.0 (upper = 3) r)))
three
(term (getValue ,three y))
(term (getConstraints ,three y))
(term (getType ,three y))
(test-equal (term (typeMatch 3 i)) #t)
(test-equal (term (typeMatch 3.01 i)) #f)
(test-equal (term (typeMatch 3 r)) #t)
(test-equal (term (typeMatch 3.01 r)) #t)
;(test-equal (term (typeMatch (vector 3 12) v)) #t)

"should be true"
(redex-match? STAN vec (term (3 1 2 150.0)))


(test-results)







(define-metafunction STAN_E
  createVar : σ x EV -> σ
  [(createVar σ x EV) (extend σ x EV) (side-condition (not (term (exists σ x))))]
  [(createVar σ x EV) (variable-already-initialized x)])