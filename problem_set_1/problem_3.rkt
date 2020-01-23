#lang racket
(require redex)


(define-language dblanguage
 (e v (function v e) (e e))
 (d natural v (function v d) (d d))
 (v string))

(define t1t (term (function "x" "x")))
(define t1a (term (function "x" 0)))

(define t2t (term (function "y" ((function "x" ("x" "y")) ("x" "y")))))
(define t2a (term (function "y" ((function "x" (0 1)) ("x" 0)))))

(define-metafunction dblanguage
  updatevarscope : d v_replacearg d_scope -> d
  [(updatevarscope (function v_arg v_2) v_replacearg d_scope)
   ,(cond
     [(eqv? (term v_arg) (term v_replacearg))
      (cond
        [(eqv? (term d_scope) 0) (term (function v_arg d_scope))]
        [else (term (function v_arg v_2))])]
     [(eqv? (term v_2) (term v_replacearg)) (term (function v_arg d_scope))])])

;; todo: I need my else case with recursion and combining the results
;; todo: condition for (d d)

(test-equal (term (updatevarscope (function "x" "x") "x" 0)) (term (function "x" 0)))
(test-equal (term (updatevarscope (function "x" "y") "y" 1)) (term (function "x" 1)))
;(test-equal (term (updatevarscope (function "x" (function "y" "z")) "z" 2)) (term (function "x" (function "y" "3") "z" 2)))

#;(define-metafunction dblanguage
  db : d -> d
  [(db d) d])

;(test-equal (term (db ,t1t)) t1a)
;(test-equal (term (db ,t2t)) t2a)


(test-results)