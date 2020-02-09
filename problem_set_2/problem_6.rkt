#lang racket
(require redex)


;; Part A: Lam with call-by-value operational semantics
;; used https://summer-school.racket-lang.org/2017/notes/mon-mor.html as a resource
;; to figure out how to get lambda (x) e to have some kind of scope for x.
(define-language Lam
  (e ::= v (e e) (lambda (x) e) x)
  (x ::= variable-not-otherwise-mentioned)
  (C ::= (C e) (v C) (x C) (lambda (x) C) hole)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language Lam)

(test-equal (redex-match? Lam x (term a)) #t)
(test-equal (redex-match? Lam e (term a)) #t)
(test-equal (redex-match? Lam e (term (lambda (x) x))) #t)
(test-equal (redex-match? Lam (e_1 e_2) (term ((lambda (x) x) z))) #t)

(term (substitute (term (lambda (x) y)) y x))
(term (substitute (term (lambda (z) y)) y x))

;; Part B: LamBool with call-by-value reduction relation
(define-language LamBool
  (e ::= v (e e) (lambda (x) e) x true false (if e_1 then e_2 else e_3))
  (x ::= variable-not-otherwise-mentioned)
  (C ::=
     (C e)
     (x C)
     (lambda (x) C)
     (if C then e_1 else e_2)
     (if true then C else e_2)
     (if false then e_1 else C)
     hole)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language LamBool)

(test-equal (redex-match? LamBool x (term a)) #t)
(test-equal (redex-match? LamBool e (term a)) #t)
(test-equal (redex-match? LamBool e (term (lambda (x) x))) #t)
(test-equal (redex-match? LamBool (e_1 e_2) (term ((lambda (x) x) z))) #t)
(test-equal (redex-match? LamBool e (term (if true then (lambda (x) x) else (lambda (y) x)))) #t)


(define LamBool-Reduction
  (reduction-relation
   LamBool
   (--> (in-hole C ((lambda (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        substitue-lambda-beta-reduction)
   (--> (in-hole C (if true then e_2 else e_3))
        (in-hole C e_2)
        if-true-substitution)
   (--> (in-hole C (if false then e_2 else e_3))
        (in-hole C e_3))))

 ;; test if sbustitution works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term ((lambda (x) y) (lambda (z) y))))
 (term (y)))

;; test if true works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term (if true then true else false)))
 (term (true)))

;; test if false false works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term (if false then true else false)))
 (term (false)))

;; test if reduction in if clause works
(test-equal
 (apply-reduction-relation* LamBool-Reduction (term (if ((lambda (x) x) false) then true else false)))
 (term (false))) 

;; TODO: relation for if else

#;(define bool-red     ;; alternative definition, using E
  (reduction-relation
   bool-or
   (--> (in-hole E (f or e))
        (in-hole E e)
        or-false)
   (--> (in-hole E (t or e))
        (in-hole E t)
        or-true)))


(test-results)