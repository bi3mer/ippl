#lang racket
(require redex)

(define-language Expression
  (e ::= x (function x e) (e e))
  (x ::= string))

(define-language DBExpression
  (d ::= n x (function x d) (d d))
  (x ::= string)
  (n ::= number))

;; I found lookup in a tutorial for an implementation of IMP I'm working on.
;; I added exists for safety before getting the variable since it feels wrong
;; to return a number or a string in a metafunciton.
(define-metafunction DBExpression
  lookup : ((x n) ...) x -> n
  [(lookup (_ ... (x n) _ ...) x) n])

(define-metafunction DBExpression
  exists : ((x n) ...) x -> boolean
  [(exists (_ ... (x n) _ ...) x) #t]
  [(exists ((x_1 n) ...) x) #f])

(define-metafunction DBExpression
  addone : ((x n) ...) -> ((x n) ...)
  [(addone ()) ()]
  [(addone ((x n ) (x_rest n_rest) ...))
   ,(cons
     (term (x ,(+ (term n) 1)))
     (term (addone ((x_rest n_rest) ...))))])

;; also taken from my imp implementation but different from the tutorial's
;; implementation. This way made more sense to me but could probably use pattern
;; matchiing.
(define-metafunction DBExpression
  extend : ((x n) ...) x n -> ((x n) ...)
  [(extend () x n) ((x n))]
  [(extend ((x n) (x_rest n_rest) ...) x_1 n_1)
    ((x n_1) (x_rest n_rest) ...)
    (side-condition (eqv? (term x) (term x_1)))]
  [(extend ((x n) (x_rest n_rest) ...) x_1 n_1)
    ,(cons
      (term (x n))
      (term (extend ((x_rest n_rest) ...) x_1 n_1)))])

(define list (term (("a" 0) ("b" 1))))
(test-equal (term (exists ,list "a")) #t)
(test-equal (term (exists ,list "b")) #t)
(test-equal (term (exists ,list "c")) #f)

(test-equal (term (lookup ,list "a")) 0)
(test-equal (term (lookup ,list "b")) 1)

(test-equal (term (addone ,list)) (term (("a" 1) ("b" 2))))
(test-equal (term (extend ,list "a" 0)) (term (("a" 0) ("b" 1))))
(test-equal (term (extend ,list "c" 0)) (term (("a" 0) ("b" 1) ("c" 0))))

(define-metafunction DBExpression
  db : d ((x n) ...) -> d
  ;; x is in lookup
  [(db x ((x_1 n) ...))
   (lookup ((x_1 n) ...) x)
   (side-condition (term (exists ((x_1 n) ...) x)))]
  
  ;; x is not in lookup
  [(db x ((x_1 n) ...))
   x
   (side-condition (eqv? #f (term (exists ((x_1 n) ...) x))))]

  ;; add lookup and extend lookup
  [(db (function x d) ((x_1 n) ...))
   (function x (db d (addone (extend ((x_1 n) ...) x -1))))]

  ;; two expressions next to eachother
  [(db (d_1 d_2) ((x n) ...)) ((db d_1 ((x n) ...)) (db d_2 ((x n) ...)))])

;; all tests check values and that the input langauge is an Expression and the
;; output is a DBExpression.
;;
;; x remains even if there is a value in the store.
(define in1 (term "x"))
(test-equal (redex-match? Expression e in1) #t)
(define test1 (term (db ,in1 (("y" 0)))))
(test-equal test1 (term "x"))
(test-equal (redex-match? DBExpression d test1) #t)

;; x changes when there is a valid value for it in the store.
(define test2 (term (db ,in1 (("x" 0)))))
(test-equal test2 (term 0))
(test-equal (redex-match? DBExpression d test2) #t)

;; y remains in a function call with x.
(define in3 (term (function "x" "y")))
(test-equal (redex-match? Expression e in3) #t)
(define test3 (term (db ,in3 ())))
(test-equal test3 (term (function "x" "y")))
(test-equal (redex-match? DBExpression d test3) #t)

;; x changes to 0 when there is a binding occurrence.
(define in4 (term (function "x" "x")))
(test-equal (redex-match? Expression e in4) #t)
(define test4 (term (db ,in4 ())))
(test-equal test4 (term (function "x" 0)))
(test-equal (redex-match? DBExpression d test4) #t)

;; nothing should happen with to expressions next to eachother.
(define in5 (term ("x" "y")))
(test-equal (redex-match? Expression e in5) #t)
(define test5 (term (db ,in5 ())))
(test-equal test5 (term ("x" "y")))
(test-equal (redex-match? DBExpression d test5) #t)

;; binding should occur in first function but not in the second. Making sure
;; that the store doesn't persist.
(define in6 (term ((function "x" "x") (function "y" "x"))))
(test-equal (redex-match? Expression e in6) #t)
(define test6 (term (db ,in6 ())))
(test-equal test6 (term ((function "x" 0) (function "y" "x"))))
(test-equal (redex-match? DBExpression d test6) #t)

;; sampel test from problem to show that it works as expected.
(define in7 (term (function "y" ((function "x" ("x" "y")) ("x" "y")))))
(test-equal (redex-match? Expression e in7) #t)
(define test7 (term (db ,in7 ())))
(test-equal test7 (term (function "y" ((function "x" (0 1)) ("x" 0)))))
(test-equal (redex-match? DBExpression d test7) #t)

(test-results)