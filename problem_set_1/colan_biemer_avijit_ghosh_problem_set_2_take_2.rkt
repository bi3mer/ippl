#lang racket
(require redex)

;; Problem 1
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
  [(num-atomics
    (m_1 m_2 w))
   ,(+
     (term (num-atomics m_1))
     (term (num-atomics m_2)) 1)])

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

;; replace. The way we are reading this is that if a mobile has a total
;; weight we are going to replace all of it. N is the replacement weight
(define-metafunction mobile
  replace : m w_old w_new -> m
  [(replace w w_old w_new)
      ,(cond
        [(eqv? (term w) (term w_old)) (term w_new)]
        [else (term w)])]
  [(replace (m_1 m_2 w) w_old w_new)
      ,(cond
         [(eqv?
           (+
            (term w)
            (term (total-weight m_1))
            (term (total-weight m_2)))
           (term w_old))
          (term w_new)]
         [else
          (term (
                 (replace m_1 w_old w_new)
                 (replace m_2 w_old w_new)  w))])])

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
  [(balanced? (m_1 m_2 w))
   ,(eqv?
     (term (total-weight m_1))
     (term (total-weight m_2)))])

(test-equal (term (balanced? ,m1)) #t)
(test-equal (term (balanced? ,m2)) #t)
(test-equal (term (balanced? ,m3)) #f)
(test-equal (term (balanced? ,m4)) #f)
(test-equal (term (balanced? ,m5)) #t)

;; run tests
(test-results)



;; Problem 2
(define-language Graph
  (g (graph n ... e ...))
  (n (node x))
  (e (edge x x))
  (x variable-not-otherwise-mentioned))
 
(define g1 (term (graph (node a) (node b) (node c) (edge b a) (edge b c))))
(define g2 (term (graph (node a) (node b) (edge b a) (edge b c))))

(test-equal (redex-match? Graph g g1) #t)
(test-equal (redex-match? Graph g g2) #t)

;; give a node name x it will search the list of nodes on the right via pattern
;; matching to see if one exits. If one does then it will return true else it
;; will return false
(define-metafunction Graph
  nodeExists? : (n ...) x -> boolean
  [(nodeExists? (_ ... (node x) _ ...) x) #t]
  [(nodeExists? ((node x_1) ...) x) #f])

(test-equal (term (nodeExists? () a)) #f)
(test-equal (term (nodeExists? ((node b) (node c) (node d)) a )) #f)
(test-equal (term (nodeExists? ((node a) (node c) (node d)) a )) #t)
(test-equal (term (nodeExists? ((node b) (node a) (node d)) a )) #t)

;; - Given an empty graph, the function will return true. The graph is good.
;; - Given a graph with only nodes the function, the function will return true.
;; - Given a a graph with nodes and edges it will iterate through the edges and
;;   look for an edge that has a node that is not in nodes. To do this it goes
;;   to another metafunction, nodeExists?, that has its own documentation. If
;;   the side condition is meant then it recruses to the next call. Else the
;;   side condition is not meant and the graph is not good and the function
;;   wil return false. Essentially making it so that the really base case is
;;   when there are nodes but no edges.
(define-metafunction Graph
  good : g -> boolean
  [(good ()) #t] ; base case that never runs but you have asked for
  [(good (graph)) #t] ; base case
  [(good (graph n ...)) #t]
  [(good (graph n ... (edge x_1 x_2) e_rest ...))
   (good (graph n ... e_rest ...))
   (side-condition
    (and
     (term (nodeExists? (n ...) x_1))
     (term (nodeExists? (n ...) x_2))))]
  [(good (graph n ... (edge x_1 x_2) e_rest ...)) #f])

;; test base case
(test-equal (term (good (graph))) #t)

;; test no edges
(define noEdge (term (graph (node a) (node b) (node c))))
(test-equal (redex-match? Graph g noEdge) #t)
(test-equal (term (good ,noEdge)) #t)

;; test graph 1
(test-equal (term (good ,g1)) #t)

;; test graph 2
(test-equal (term (good ,g2)) #f)


(test-results)




;; Problem 3

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
  dbenv : d ((x n) ...) -> d
  ;; x is in lookup
  [(dbenv x ((x_1 n) ...))
   (lookup ((x_1 n) ...) x)
   (side-condition (term (exists ((x_1 n) ...) x)))]
  
  ;; x is not in lookup
  [(dbenv x ((x_1 n) ...))
   x
   (side-condition (eqv? #f (term (exists ((x_1 n) ...) x))))]

  ;; add lookup and extend lookup (if x is already in that it will be overset to
  ;; -1. The addone call sets the -1 to 0.
  [(dbenv (function x d) ((x_1 n) ...))
   (function x (dbenv d (addone (extend ((x_1 n) ...) x -1))))]

  ;; two expressions next to eachother
  [(dbenv (d_1 d_2) ((x n) ...))
   ((dbenv d_1 ((x n) ...))
    (dbenv d_2 ((x n) ...)))])

(define-metafunction DBExpression
  db : d -> d
  [(db d) (dbenv d ())])

;; all tests check values and that the input langauge is an Expression and
;; the output is a DBExpression.
;;
;; x remains even if there is a value in the store.
(define in1 (term "x"))
(test-equal (redex-match? Expression e in1) #t)
(define test1 (term (dbenv ,in1 (("y" 0)))))
(test-equal test1 (term "x"))
(test-equal (redex-match? DBExpression d test1) #t)

;; x changes when there is a valid value for it in the store.
(define test2 (term (dbenv ,in1 (("x" 0)))))
(test-equal test2 (term 0))
(test-equal (redex-match? DBExpression d test2) #t)

;; y remains in a function call with x.
(define in3 (term (function "x" "y")))
(test-equal (redex-match? Expression e in3) #t)
(define test3 (term (db ,in3)))
(test-equal test3 (term (function "x" "y")))
(test-equal (redex-match? DBExpression d test3) #t)

;; x changes to 0 when there is a binding occurrence.
(define in4 (term (function "x" "x")))
(test-equal (redex-match? Expression e in4) #t)
(define test4 (term (db ,in4)))
(test-equal test4 (term (function "x" 0)))
(test-equal (redex-match? DBExpression d test4) #t)

;; nothing should happen with to expressions next to eachother.
(define in5 (term ("x" "y")))
(test-equal (redex-match? Expression e in5) #t)
(define test5 (term (db ,in5)))
(test-equal test5 (term ("x" "y")))
(test-equal (redex-match? DBExpression d test5) #t)

;; binding should occur in first function but not in the second. Making sure
;; that the store doesn't persist.
(define in6 (term ((function "x" "x") (function "y" "x"))))
(test-equal (redex-match? Expression e in6) #t)
(define test6 (term (db ,in6)))
(test-equal test6 (term ((function "x" 0) (function "y" "x"))))
(test-equal (redex-match? DBExpression d test6) #t)

;; sampel test from problem to show that it works as expected.
(define in7 (term (function "y" ((function "x" ("x" "y")) ("x" "y")))))
(test-equal (redex-match? Expression e in7) #t)
(define test7 (term (db ,in7)))
(test-equal test7 (term (function "y" ((function "x" (0 1)) ("x" 0)))))
(test-equal (redex-match? DBExpression d test7) #t)

(test-results)