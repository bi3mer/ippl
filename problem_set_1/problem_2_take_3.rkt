#lang racket
(require redex)

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