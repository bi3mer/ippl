#lang racket
(require redex)


;; Part A
(define-language Lam
  (e ::= x (lambda x e) (e e))
  (C ::= hole  (C e) (x C)))

#;(define Lam-reduction
  (reduction-relation
   Lam
   (--> (in-hole C ()))))


#;(define bool-red     ;; alternative definition, using E
  (reduction-relation
   bool-or
   (--> (in-hole E (f or e))
        (in-hole E e)
        or-false)
   (--> (in-hole E (t or e))
        (in-hole E t)
        or-true)))



;; Part B
(define-language LamBool
  (e ::= x (lambda x e) (e e) true false (if e then e else e)))

#;(define RLAM
  (reduction-relation ST)
  
  )

;; Part C
;; View README.txt for explanation
;(define-union-language ST (s. LamBool) (t. Lam))
(define-union-language ST LamBool Lam)


#;(define-metafunction ST
  translate : e -> e
  [(translate (if true then e_2 else e_3)) e_2]
  [(translate (if false then e_2 else e_3)) e_3]
  [(translate e) e])