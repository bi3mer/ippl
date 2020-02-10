#lang racket
(require redex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part A ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lam with call-by-value operational semantics
;; used https://summer-school.racket-lang.org/2017/notes/mon-mor.html as a resource
;; to figure out how to get lambda (x) e to have some kind of scope for x.
(define-language Lam
  (e ::= v (e e) (lambda (x) e) x)
  (x ::= variable-not-otherwise-mentioned)
  (C ::= (C e) (x C) (lambda (x) C) hole)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language Lam)

(test-equal (redex-match? Lam x (term a)) #t)
(test-equal (redex-match? Lam e (term a)) #t)
(test-equal (redex-match? Lam e (term (lambda (x) x))) #t)
(test-equal (redex-match? Lam (e_1 e_2) (term ((lambda (x) x) z))) #t)

(define Lam-Reduction
  (reduction-relation
   Lam
   (--> (in-hole C ((lambda (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        substitue-lambda-beta-reduction)
   (--> (in-hole C ((e)))
        (in-hole C e))))

;; test if sbustitution works
(test-equal
 (apply-reduction-relation Lam-Reduction (term ((lambda (x) y) (lambda (z) y))))
 (term (y)))

(test-results)

;(term (substitute (term (lambda (x) y)) y x))
;(term (substitute (term (lambda (z) y)) y x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part B ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LamBool with call-by-value reduction relation. Used bool reduction and the sit
;; mentioned before as reference for implementation.
;;
;; Note for the instructor: I needed to add the "(e)" else the
;; translation gave an out of domain error even if I did something like:
;;
;;      [(translate (false)) (e)]
;;
;; I'm not sure why this is the case so any notes would be appreciated. I could
;; not find anything in the documentation for why this was necessary. Thanks in
;; advance.

(define-language LamBool
  (e ::= v (e e) (lambda (x) e) x true false (e) (if e_1 then e_2 else e_3))
  (x ::= variable-not-otherwise-mentioned)
  (C ::=
     (C e)
     (x C)
     (false C)
     (true C)
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
        if-true)
   (--> (in-hole C (if false then e_2 else e_3))
        (in-hole C e_3)
        if-false)
   (--> (in-hole C (e))
        (in-hole C e))))

 ;; test if sbustitution works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term ((lambda (x) y) (lambda (z) y))))
 (term (y)))

;; test if true works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term (if true then true else false)))
 (term (true)))

;; test if false works
(test-equal
 (apply-reduction-relation LamBool-Reduction (term (if false then true else false)))
 (term (false)))

;; test if reduction in if clause works
(test-equal
 (apply-reduction-relation* LamBool-Reduction (term (if ((lambda (x) x) false) then true else false)))
 (term (false)))

;; test if reduction in then clause works
(test-equal
 (apply-reduction-relation* LamBool-Reduction (term (if true then ((lambda (x) x) false) else false)))
 (term (false)))

;; test if reduction in else clause works
(test-equal
 (apply-reduction-relation* LamBool-Reduction (term (if false then ((lambda (x) x) false) else ((lambda (x) x) true))))
 (term (true)))

(test-results)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; My assumption with this is that the translate function shouldn't actually
; evaluate since that wouldn't make any sense. However, if there is a case where
; it can easily simplify, then it will. The only example of this here is in the
; case where it is if true or if false. Past that it simply looks for cases
; where Lam can't express the same thing and it modifies it, such as false goes
; to true.

(define-union-language ST LamBool Lam)

(define-metafunction ST
  translate : e -> e
  [(translate true) e]
  [(translate false) e]
  [(translate x) x]
  [(translate (if true then e_1 else e_2)) (translate e_1)]
  [(translate (if false then e_1 else e_2)) (translate e_2)]
  [(translate (lambda (x) e)) (lambda (x) (translate e))]
  [(translate (e_1 e_2)) ((translate e_1) (translate e_2))]
  [(translate (e)) (translate e)])

(default-language ST)

(test-equal (term (translate false)) (term e))
(test-equal (term (translate true)) (term e))
(test-equal
 (term (translate (if false then ((lambda (x) x) false) else ((lambda (x) x) true))))
 (term ((lambda (x) x) e)))

(test-results)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Part D ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the conjecture makes sure that if the input can be fully reduced then the
; translated version should be as well. The one problem that I had with this
; is handling the case of e == (e). I don't know how to handle this case. I
; updated the reduction relations for a (e) case but that didn't seem to do
; the trick. So what I have is pretty innefficient by checking for these two
; specific cases of e == (e) or e == e. Any feedback on how to get rid of those
; parentheses would be very appreciated.

#;(define-metafunction ST
  EquivalentConjecture : e -> boolean
  [(EquivalentConjecture e)
   ,(eqv? (term e_1) (term e_2))
      (where e_1 (translate ,(apply-reduction-relation* LamBool-Reduction (term e))))
      (where e_2 (apply-reduction-relation* Lam-Reduction (term (translate e))))])


(define-metafunction ST
  EquivalentConjecture : e -> boolean
  [(EquivalentConjecture e)
   ,(or
     (equal? (term (translate ,(apply-reduction-relation* LamBool-Reduction (term e)))) (apply-reduction-relation* Lam-Reduction (term (translate e))))
     (equal? (term ((translate ,(apply-reduction-relation* LamBool-Reduction (term e))))) (apply-reduction-relation* Lam-Reduction (term (translate e)))))])


(redex-check ST l (term (EquivalentConjecture l)))

