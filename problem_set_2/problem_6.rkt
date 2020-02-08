#lang racket
(require redex)


;; Part A: Lam with call-by-value operational semantics
;;
;; Looking through the redex documentation I saw that they had this alternative
;; whre they used v to describe lambda and I think that makes a lot more sense and
;; will likely lead to a cleaner reduction relation later on.
(define-language Lam
  (e ::= v (e e) x)
  (v ::= (lambda (x) e))
  (x ::= variable-not-otherwise-mentioned)
  (C ::= (V (C e) (v C) (x C) hole))
  (V ::= (lambda (x) C)))

(test-equal (redex-match? Lam x (term a)) #t)
(test-equal (redex-match? Lam e (term a)) #t)
(test-equal (redex-match? Lam v (term (lambda (x) x))) #t)
(test-equal (redex-match? Lam e (term (lambda (x) x))) #t)
(test-equal (redex-match? Lam (e_1 e_2) (term ((lambda (x) x) z))) #t)

;; Part B: LamBool with call-by-value reduction relation


(test-results)