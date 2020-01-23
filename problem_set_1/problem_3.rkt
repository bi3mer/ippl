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
  [(updatevarscope v v_replacearg d_scope)
   ,(cond
     [(eqv? (term v) (term v_replacearg)) (term d_scope)]
     [else (term v)])]
  [(updatevarscope (function v_arg v_2) v_replacearg d_scope)
   ,(cond
     [(eqv? (term v_arg) (term v_replacearg))
      (cond
        [(eqv? (term d_scope) 0) (term (function v_arg d_scope))]
        [else (term (function v_arg v_2))])]
     [(eqv? (term v_2) (term v_replacearg)) (term (function v_arg d_scope))]
     [else (term (function v_arg v_2))])]
  [(updatevarscope (d_1 d_2) v_replace_arg d_scope)
   ,(term ((updatevarscope d_1 v_replace_arg d_scope) (updatevarscope d_2 v_replace_arg d_scope)))]
  [(updatevarscope (function v_arg d) v_replacearg d_scope)
   ,(cond
      [(eqv? (term v_arg) (term v_replacearg)) (term (function v_arg d))]
      [else (term (function v_arg (updatevarscope d v_replacearg ,(+ 1 (term d_scope)))))])])

;; todo: need last case of function with v_arg and d statement

(test-equal (term (updatevarscope "x" "x" 1)) 1)
(test-equal (term (updatevarscope (function "x" "x") "x" 0)) (term (function "x" 0)))
(test-equal (term (updatevarscope (function "x" "x") "x" 2)) (term (function "x" "x")))
(test-equal (term (updatevarscope (function "x" "y") "y" 1)) (term (function "x" 1)))
(test-equal (term (updatevarscope (function "x" "z") "y" 2)) (term (function "x" "z")))
(test-equal (term (updatevarscope (("x" "x") ("y" "x")) "y" 1)) (term (("x" "x") (1 "x"))))
(test-equal (term (updatevarscope (function "x" (function "y" "z")) "z" 2)) (term (function "x" (function "y" 3))))

#;(define-metafunction dblanguage
  db : d -> d
  [(db d) d])

;(test-equal (term (db ,t1t)) t1a)
;(test-equal (term (db ,t2t)) t2a)


(test-results)