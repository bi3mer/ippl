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

;;;; Stan language
(define-language STAN
  (e ::= pv x (e ...) (e [e]) (e MO e)) ;; 
  (s ::=
     skip
     (i C x)
     (r C x)
     (x = e)
     (x [ e ... ] = e)
     (s ...)) ;; (for(x in e : e) s) (if e s then s)
  
  ;; primative values
  (pv ::= integer number)

  ;; Values
  (V ::= pv)

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
  (MO ::= + - * /)

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
     (pv MO E))

  ;; variable
  (x ::= variable-not-otherwise-mentioned))

;;;; stan environment
;; TODO: add type
(define-extended-language STAN_E STAN
  (σ ::= ((x EV) ...) (variable-not-found-error x)))

(define-metafunction STAN
  extend : ((x EV) ...) x EV -> ((x EV) ...)
  [(extend () x EV) ((x EV))]
  [(extend ((x EV) (x_rest EV_rest) ...) x_1 EV_1)
    ((x EV_1) (x_rest EV_rest) ...)
    (side-condition (eqv? (term x) (term x_1)))]
  [(extend ((x EV) (x_rest EV_rest) ...) x_1 EV_1)
    ,(cons (term (x EV)) (term (extend ((x_rest EV_rest) ...) x_1 EV_1)))])

(define-metafunction STAN
  lookup : ((x EV) ...) x -> EV
  [(lookup (_ ... (x EV) _ ...) x) EV])

(define-metafunction STAN
  exists : ((x EV) ...) x -> boolean
  [(exists (_ ... (x EV) _ ...) x) #t]
  [(exists ((x_1 EV) ...) x) #f])

(define-metafunction STAN_E
  setVar : σ x EV -> σ
  [(setVar σ x EV) (extend σ x EV) (side-condition (term (exists σ x)))]
  [(setVar σ x EV) (variable-not-found-error z)])

;;;; Required Metafunctions
;; math operations
(define-metafunction STAN
  mathOperation : pv MO pv -> pv
  [(mathOperation pv_1 + pv_2) ,(+ (term pv_1) (term pv_2))]
  [(mathOperation pv_1 - pv_2) ,(- (term pv_1) (term pv_2))]
  [(mathOperation pv_1 * pv_2) ,(* (term pv_1) (term pv_2))]
  [(mathOperation pv_1 / pv_2) ,(/ (term pv_1) (term pv_2))])

;; boolean

;; get the environment from the result of an apply reduction relation
(define-metafunction STAN_E
  getEnv : ((s σ)) -> σ
  [(getEnv  ((s σ))) σ])

;;;; Reduction Relation
;; probably need to use a meta function to pattern match on the constraints to
;; fill everything in for testing at the end.
(define stan_r
  (reduction-relation
   STAN_E
   ;; variable operations
   (--> [(in-hole E x) σ]
       [(in-hole E (lookup σ x)) σ]
       findVar)

   (--> [(in-hole E (i C x)) σ]
        [(in-hole E skip) (extend σ x 0)] ; (extend (extend σ x 0) current x) 
        assign-int)
   (--> [(in-hole E (r C x)) σ]
        [(in-hole E skip) (extend σ x 0.0)] ; (extend (extend σ x 0.0) current x) 
        assign-real)

   ;; (x [e] = e case is not handled
   (--> [(in-hole E (x = pv)) σ]
        [(in-hole E skip) (setVar σ x pv)]
        update-val)

   ;; math operations
   (--> [(in-hole E (pv_1 MO pv_2)) σ]
        [(in-hole E (mathOperation pv_1 MO pv_2)) σ]
        math-operation)

   ;; for

   ;; if
   ))

;;;;;;;;;;;;;;; Syntax Tests ;;;;;;;;;;;;;;;
;; variable definitions
(test-equal
 (redex-match? STAN s (term (r none x)))
 #t)

(test-equal
 (redex-match? STAN s (term (i none x)))
 #t)

(test-equal
 (redex-match? STAN s (term (r (upper = 0) x)))
 #t)

(test-equal
 (redex-match? STAN s (term (r ((lower = 1) (upper = 0)) x)))
 #t)

(test-equal
 (redex-match? STAN s (term (r ((lower = 1) (offset = 0) (multiplier = 3)) x)))
 #t)

(test-equal
 (redex-match? STAN s (term (r ((lower = 1) (offset = 0) (badtag = 3)) x)))
 #f)

;; setting a variable
(test-equal (redex-match? STAN s (term (x = 3)))
 #t)

(test-equal
 (redex-match? STAN s (term (x [3] = 3)))
 #t)

(test-equal
 (redex-match? STAN s (term (x [y] = 3)))
 #t)

(test-equal
 (redex-match? STAN s (term (x ([y 0 1]) = 3)))
 #t)

;;;;;;;;;;;;;;; Environment Tests ;;;;;;;;;;;;;;;
(test-equal (term (extend () x 3)) (term ((x 3))))
(test-equal (term (extend () x 3.12)) (term ((x 3.12))))
(test-equal (term (extend ((x 3)) y 7)) (term ((x 3) (y 7))))
(test-equal (term (extend ((x 3)) y 1.11)) (term ((x 3) (y 1.11))))

(test-equal (term (lookup ((x 3)) x)) (term 3))
(test-equal (term (lookup ((x 3) (y 7.01)) y)) (term 7.01))

(test-equal (term (exists ((x 3)) x)) #t)
(test-equal (term (exists ((x 3) (y 3)) y)) #t)
(test-equal (term (exists ((x 3) (y 3)) z)) #f)

(test-equal (term (setVar ((x 3)) x 10)) (term ((x 10))))
(test-equal (term (setVar ((x 3)) z 10)) (term (variable-not-found-error z)))

;;;;;;;;;;;;;;; Judgement Tests ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Reduction Relation Tests ;;;;;;;;;;;;;;;
;; variable creation
(define vc1 (apply-reduction-relation* stan_r (term ((r none x) ()))))
(test-equal (term (lookup (getEnv ,vc1) x)) 0.0)

(define vc2 (apply-reduction-relation* stan_r (term ((i none x) ()))))
(test-equal (term (lookup (getEnv ,vc2) x)) 0)

(define vc3 (apply-reduction-relation* stan_r (term (((r none x) (i none y)) ()))))
(test-equal (term (lookup (getEnv ,vc3) x)) 0.0)
(test-equal (term (lookup (getEnv ,vc3) x)) 0.0)

; note that arrays, vectors, and matrices are not handled yet

;; variable assignment
(define va1 (apply-reduction-relation* stan_r (term (((r none x) (x = 3.0)) ()))))
(test-equal (term (lookup (getEnv ,va1) x)) 3.0)

(define va2 (apply-reduction-relation* stan_r (term (((r none x) (z = 1)) ()))))
(test-equal (term (getEnv ,va2)) (term (variable-not-found-error z)))

(define va3 (apply-reduction-relation* stan_r (term (((r none z) (z = 1)) ()))))
(test-equal (term (getEnv ,va3)) (term ((z 1))))

; note that arrays, vectors, and matrices are not handled yet
; note that x [ y ... ] is not tested yet. 

;; math operations
(define mo1 (apply-reduction-relation* stan_r (term (((i none x) (x = (4 + 4))) ()))))
(test-equal (term (lookup (getEnv ,mo1) x)) 8)

(define mo2 (apply-reduction-relation* stan_r (term (((i none x) (x = (4 - 4))) ()))))
(test-equal (term (lookup (getEnv ,mo2) x)) 0)

(define mo3 (apply-reduction-relation* stan_r (term (((i none x) (x = (4 * 4))) ()))))
(test-equal (term (lookup (getEnv ,mo3) x)) 16)

(define mo4 (apply-reduction-relation* stan_r (term (((i none x) (x = (4 / 4))) ()))))
(test-equal (term (lookup (getEnv ,mo4) x)) 1)

(define mo5
  (apply-reduction-relation*
   stan_r (term (((i none x) (x = (4 + 4)) (i none y) (y = (x * x))) ()))))
(test-equal (term (lookup (getEnv ,mo5) x)) 8)
(test-equal (term (lookup (getEnv ,mo5) y)) 64)

;(test-equal (term (getEnv ,mo1)) (term ((x 8))))

(test-results)




