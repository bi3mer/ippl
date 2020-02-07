#lang racket
(require redex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-language mobile
  (m w        ; atomic sculpture (weight)
     (m m w)) ; composite
  (w number)) ; weight is a number

(define m1 (term 3))
(define m2 (term 4))
(define m3 (term (,m1 ,m2 5)))
(define m4 (term (,m1 ,m3 7)))
(define m5 (term (,m3 ,m3 1)))

(test-equal (redex-match? mobile m (term 3)) #t)
(test-equal (redex-match? mobile m (term (,m1 ,m2 5))) #t)
(test-equal (redex-match? mobile m (term ,m3)) #t)

;; num-atomics
(define-metafunction mobile
  num-atomics : m -> natural
  [(num-atomics w) 1]
  [(num-atomics (m_1 m_2 w)) ,(+ (term (num-atomics m_1)) (term (num-atomics m_2)) 1)])

(test-equal (term (num-atomics ,m1)) 1)
(test-equal (term (num-atomics ,m2)) 1)
(test-equal (term (num-atomics ,m3)) 3)
(test-equal (term (num-atomics ,m4)) 5)

;; total-weight
(define-metafunction mobile
  total-weight : m -> natural
  [(total-weight w) w]
  [(total-weight (m_1 m_2 w))
   ,(+ (term (total-weight m_1)) (term (total-weight m_2)) (term w))])

(test-equal (term (total-weight ,m1)) 3)
(test-equal (term (total-weight ,m2)) 4)
(test-equal (term (total-weight ,m3)) 12)
(test-equal (term (total-weight ,m4)) 22)

;; depth
(define-metafunction mobile
  depth : m -> natural
  [(depth w) 1]
  [(depth (m_1 m_2 w)) ,(+ 1 (max (term (depth m_1)) (term (depth m_2))))])

(test-equal (term (depth ,m1)) 1)
(test-equal (term (depth ,m2)) 1)
(test-equal (term (depth ,m3)) 2)
(test-equal (term (depth ,m4)) 3)

;; replace. The way we are reading this is that if a mobile has a total weight we are going
;; to replace all of it. N is the replacement weight.
(define-metafunction mobile
  replace : m w_old w_new -> m
  [(replace w w_old w_new)
      ,(cond
        [(eqv? (term w) (term w_old)) (term w_new)]
        [else (term w)])]
  [(replace (m_1 m_2 w) w_old w_new)
      ,(cond
         [(eqv? (+ (term w) (term (total-weight m_1)) (term (total-weight m_2))) (term w_old)) (term w_new)]
         [else (term ((replace m_1 w_old w_new) (replace m_2 w_old w_new)  w))])])

(define mr1 (term 1))
(define mr2 (term 2))
(define mr3 (term 4))
(define mr4 (term (,mr3 ,mr2 3)))
(define mr5 (term (,mr3 ,mr4 7)))

(define mr6 (term 7))
(define mr7 (term (,mr3 ,mr6 7)))

(test-equal (term (replace ,m1 3 2)) 2)
(test-equal (term (replace ,m2 10 2)) 4)
(test-equal (term (replace ,m4 30 2)) m4)
(test-equal (term (replace ,mr5 9 7)) mr7)

;; balanced? We are not checking if all mobiles are balanced. Only the root
;; mobile based on the phrasing of the problem.
(define-metafunction mobile
  balanced? : m -> boolean
  [(balanced? w) #t]
  [(balanced? (m_1 m_2 w)) ,(eqv? (term (total-weight m_1)) (term (total-weight m_2)))])

(test-equal (term (balanced? ,m1)) #t)
(test-equal (term (balanced? ,m2)) #t)
(test-equal (term (balanced? ,m3)) #f)
(test-equal (term (balanced? ,m4)) #f)
(test-equal (term (balanced? ,m5)) #t)

(test-results)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-language Graph
  (g (graph n ... e ...))
  (n (node x))
  (e (edge x x))
  (x variable-not-otherwise-mentioned))


(define g1 (term (graph (node a) (node b) (node c)
                        (edge b a) (edge b c))))
(define g2 (term (graph (node a) (node b)
                        (edge b a) (edge b c))))



(test-equal (redex-match? Graph g g1) #t)
(test-equal (redex-match? Graph g g2) #t)


(define-metafunction Graph
  nodes : g -> (n ...)
  [(nodes (graph e ...)) ()]
  [(nodes (graph n_1 n ... e ...)) ,(cons (term n_1) (term (nodes (graph n ... e ...))))])

(define-metafunction Graph
  nodesplit : (n ...) -> (x ...)
  [(nodesplit ()) ()]
  [(nodesplit ((node x_1) n ...)) ,(append (list(term x_1))(term (nodesplit (n ...))))])


(define-metafunction Graph
  edges : g -> (e ...)
  [(edges (graph n ...)) ()]
  [(edges (graph n ... e_1 e ...)) ,(cons (term e_1) (term (edges (graph n ... e ...))))])

(define-metafunction Graph
  edgesplit : (e ...) -> (x ...)
  [(edgesplit ()) ()]
  [(edgesplit ((edge x_1 x_2) e ...)) ,(append (list(term x_1)(term x_2))(term (edgesplit (e ...))))])


;; Design the function good, which determines whether or not the edges in a Graph
;; g mention only names that also name a node in g. We changed it to good? since
;; it returns a boolean.
(define-metafunction Graph
  good? : g -> boolean
  [(good? (graph)) #t]
  [(good? (graph n ...)) #t]
  [(good? (graph n ... e ...)) ,(subset?
                                 (list->set(term (edgesplit(edges (graph n ... e ...)))))
                                 (list->set(term (nodesplit(nodes (graph n ... e ...))))))])


(test-equal (term (good? ,g1)) #t)
(test-equal (term (good? ,g2)) #f)

(test-results)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-language dblanguage
 (e v (function v e) (e e))
 (d n v (function v d) (d d))
 (v string)
 (n natural)) ;; added this to the definition for pattern matching

;; Replaces a variable with an integer if the variable is not an argument and is
;; the target variable to be replaced. This will stop running on leafs and when
;; the variable to be replaced is rescoped.
(define-metafunction dblanguage
  updatevarscope : d v_replacearg d_scope -> d
  [(updatevarscope v v_replacearg d_scope) ;; case when only receiving a string
   ,(cond
     [(eqv? (term v) (term v_replacearg)) (term d_scope)]
     [else (term v)])]
  [(updatevarscope (function v_arg v_2) v_replacearg d_scope) ;; case when function receives two strings
   ,(cond
     [(eqv? (term v_arg) (term v_replacearg))
      (cond
        [(eqv? (term d_scope) 0) (term (function v_arg d_scope))]
        [else (term (function v_arg v_2))])]
     [(eqv? (term v_2) (term v_replacearg)) (term (function v_arg d_scope))]
     [else (term (function v_arg v_2))])]
  [(updatevarscope (d_1 d_2) v_replace_arg d_scope) ;; case when receiving (d d)
   ,(term ((updatevarscope d_1 v_replace_arg d_scope) (updatevarscope d_2 v_replace_arg d_scope)))]
  [(updatevarscope (function v_arg d) v_replacearg d_scope) ;; case when receiving function with outter d that is more complex
   ,(cond
      [(eqv? (term v_arg) (term v_replacearg)) (term (function v_arg d))]
      [else (term (function v_arg (updatevarscope d v_replacearg ,(+ 1 (term d_scope)))))])]
  [(updatevarscope n v_arg d_scope) n]) ;;  natural number case


;; to clarify (updatevarscope (function "x" x") "x" 0) will return
;; (function "x" 0) because the scope is 0 meaning the caller has just come across
;; it. (upddatevarscope "x" "x") "x" 1) will return (function "x" "x") because this
;; is now declaring a new scope for "x". Meaning updatevarscope cannot touch it
;; anymore since the scope has changed.

(test-equal (term (updatevarscope 1 "x" 1)) 1)
(test-equal (term (updatevarscope "x" "x" 1)) 1)
(test-equal (term (updatevarscope (function "x" "x") "x" 0)) (term (function "x" 0)))
(test-equal (term (updatevarscope (function "x" "x") "x" 2)) (term (function "x" "x")))
(test-equal (term (updatevarscope (function "x" "y") "y" 1)) (term (function "x" 1)))
(test-equal (term (updatevarscope (function "x" "z") "y" 2)) (term (function "x" "z")))

(test-equal (term (updatevarscope (("x" "x") ("y" "x")) "y" 1))
            (term                 (("x" "x") (1 "x"))))

(test-equal (term (updatevarscope (function "x" (function "y" "z")) "z" 2))
            (term                 (function "x" (function "y" 3))))

(test-equal (term (updatevarscope (function "x" ((function "x" "x") ("z" "y")))  "z" 0))
            (term                 (function "x" ((function "x" "x") (1   "y")))))


(define t1t (term (function "x" "x")))
(define t1a (term (function "x" 0)))

(define t2t (term (function "y" ((function "x" ("x" "y")) ("x" "y")))))
(define t2a (term (function "y" ((function "x" (0 1)) ("x" 0)))))

;; I'm not sure if db is the name you wanted. But this finds function arguments
;; and uses updatevarscope to do the rest.
(define-metafunction dblanguage
  db : d -> d
  [(db (function v_arg d_else)) (function v_arg ,(term (db (updatevarscope d_else v_arg 0))))]
  [(db (d_1 d_2)) ,(term ((db d_1) (db d_2)))]
  [(db v) ,(term v)]
  [(db n) ,(term n)])

(test-equal (term (db (function "x" "x"))) (term (function "x" 0)))

(test-equal (term (db (function "y" ((function "x" ("x" "y")) ("x" "y")))))
            (term     (function "y" ((function "x" (0 1)) ("x" 0)))))


(test-results)