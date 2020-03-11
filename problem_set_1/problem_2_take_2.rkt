#lang racket
(require redex)

(define-language Graph
  (g (graph n ... e ...))
  (n (node x))
  (e (edge x x))
  (x variable-not-otherwise-mentioned))


(define g0 (term (graph)))

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


;;(term (nodes ,g1))
;;(term (nodesplit(nodes ,g1)))
;;(set->list(list->set(term (nodesplit(nodes ,g1)))))
;;(term (edges ,g1))
;;(term (edgesplit(edges ,g1)))
;;(set->list(list->set(term (edgesplit(edges ,g1)))))


;; Design the function good, which determines whether or not the edges in a Graph
;; g mention only names that also name a node in g. 

(define-metafunction Graph
  goodutil : (x ...) (x ...) -> boolean
  [(goodutil ()()) #t]
  [(goodutil (x ...)()) #t]
  [(goodutil ()(x ...)) #f]
  
  [(goodutil (x_1 x ...)(x_p ... x_1 x ...)) (goodutil (x ...)(x_p ... x ...))]
  [(goodutil (x_1 x ...)(x_1 x ...)) (goodutil (x ...)(x ...))]
  [(goodutil (x_1 x ...)(x ... x_1)) (goodutil (x ...)(x ...))]

  [(goodutil (x_k ... x_1 x ...)(x_p ... x_1 x ...)) (goodutil (x_k ... x ...)(x_p ... x ...))]
  [(goodutil (x_k ... x_1 x ...)(x_1 x ...)) (goodutil (x_k ... x ...)(x ...))]
  [(goodutil (x_k ... x_1 x ...)(x ... x_1)) (goodutil (x_k ... x ...)(x ...))]

  [(goodutil (x ... x_1)(x_p ... x_1 x ...)) (goodutil (x ...)(x_p ... x ...))]
  [(goodutil (x ... x_1)(x_1 x ...)) (goodutil (x ...)(x ...))]
  [(goodutil (x ... x_1)(x ... x_1)) (goodutil (x ...)(x ...))]

  [(goodutil (x_1)(x_p ... x_1 x ...)) (goodutil ()(x_p ... x ...))]
  [(goodutil (x_1)(x_1 x ...)) (goodutil ()(x ...))]
  [(goodutil (x_1)(x ... x_1)) (goodutil ()(x ...))])


(define-metafunction Graph
  good : g -> boolean
  [(good (graph)) #t]
  [(good (graph n ...)) #t]
  [(good (graph n ... e ...)) (goodutil ,(set->list(list->set(term(nodesplit(nodes (graph n ... e ...)))))) ,(set->list(list->set(term(edgesplit(edges (graph n ... e ...))))))) ])


(test-equal (term (good ,g0)) #t)
(test-equal (term (good ,g1)) #t)
(test-equal (term (good ,g2)) #f)

;; run tests
(test-results)



