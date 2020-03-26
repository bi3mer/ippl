#lang racket
(require redex)


;; TODO: vector
;; TODO: matrix
;; TODO: array
;; TODO: test vector bounding
;; TODO: test matrix bounding
;; TODO: test array bounding

;; NOTES:
;; - the current approach for restrictions does allow a user to create multiple
;;   restrictions of the same type for a variable. For example, there is
;;   nothing wrong with giving a variable 100 lower bounds. The implementation
;;   will only use the last one but this could be seen as a limitation to the
;;   implementation provided.
(define-language STAN
  (e ::= pv x (e ...) (e [e]))
  (s ::= (T x = e)  skip) ;; (s ...) (for(e in e : e) s) (if e s then s)

  
  ;; primative values
  (pv ::= integer number)

  ;; values
  (V ::= pv)
  
  ;; integer restrictions
  (IR ::=
      none
      (lower = integer)
      (upper = integer)
      (IR IR))
  
  ;; real restrictions
  (RR ::=
      none
      (lower = number)
      (upper = number)
      (offset = number)
      (multiplier = number)
      (RR RR))
  
  ;; vector restrictions
  (VR ::= x)

  ;; matrix restrictions
  (MR :: x)

  ;; Types, if I name real real then I get an error that I can't handle. So it
  ;; is now called r. TO fit this, I changed int to i. Vector to v. Matrix to
  ;; m.
  (T ::= (i IR) (r RR)) ;; vector matrix array

  ;; Operators
  (O ::= + - * / .* ./)

  ;; boolean operators
  (BO ::= > >= <= <= == !=)

  ;; variable
  (x ::= variable-not-otherwise-mentioned))

(define-extended-language STAN_E STAN
  (σ ::= ((x V) ...)))

(define-metafunction STAN
  extend : ((x V) ...) x V -> ((x V) ...)
  [(extend () x V) ((x V))]
  [(extend ((x V) (x_rest V_rest) ...) x_1 V_1)
    ((x V_1) (x_rest V_REST) ...)
    (side-condition (eqv? (term x) (term x_1)))]
  [(extend ((x V) (x_rest V_REST) ...) x_1 V_1)
    ,(cons (term (x V)) (term (extend ((x_rest V_rest) ...) x_1 V_1)))])

(define-metafunction STAN
  lookup : ((x V) ...) x -> V
  [(lookup (_ ... (x V) _ ...) x) V])

;;;;;;;;;;;;;;; Grammar Tests ;;;;;;;;;;;;;;;
;; test expressions
(test-equal (redex-match? STAN e (term x)) #t)
(test-equal (redex-match? STAN e (term (x y z 100))) #t)
(test-equal (redex-match? STAN e (term (x[y]))) #t)
(test-equal (redex-match? STAN e (term (x[1]))) #t)

;; test statements
(test-equal
 (redex-match? STAN s (term skip))
 #t)

;; test int bounding
(test-equal
 (redex-match? STAN s (term ((i none) x = 3)))
 #t)

(test-equal
 (redex-match? STAN s (term ((i (lower = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((i (upper = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((i ((lower = 1) (upper = 3))) x = 1)))
 #t)

;; test real bounding
(test-equal
 (redex-match? STAN s (term ((r none) x = 3)))
 #t)

(test-equal
 (redex-match? STAN s (term ((r (lower = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((r (upper = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((r (offset = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((r (multiplier = 3)) x = 1)))
 #t)

(test-equal
 (redex-match? STAN s (term ((r ((lower = 1) (upper = 3))) x = 1)))
 #t)

(test-equal
 (redex-match?
  STAN s (term ((r ((lower = 1) ((upper = 3) (offset = 10)))) x = 1)))
 #t)

(test-equal
 (redex-match?
  STAN s (term ((r ((lower = 1) ((upper = 3) ((offset = 10) (multiplier = 2))))) x = 1)))
 #t)

;;;;;;;;;;;;;;; Environment Tests ;;;;;;;;;;;;;;;
(test-equal (term (extend () x 3)) (term ((x 3))))
(test-equal (term (extend () x 3.12)) (term ((x 3.12))))
(test-equal (term (extend ((x 3)) y 7)) (term ((x 3) (y 7))))
(test-equal (term (extend ((x 3)) y 1.11)) (term ((x 3) (y 1.11))))

(test-equal (term (lookup ((x 3)) x)) (term 3))
(test-equal (term (lookup ((x 3) (y 7.01)) y)) (term 7.01))

;;;;;;;;;;;;;;; Meta Function Tests ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Judgement Tests ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Reduction Relation Tests ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; Full Program Tests ;;;;;;;;;;;;;;;


(test-results)




