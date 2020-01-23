#lang racket
(require redex)

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

;; TODO: fix language
;; based off of in function from tutorial I did: https://docs.racket-lang.org/redex/redex2015.html
#;(define-metafunction Graph
  in : x (e ...) -> boolean
  [(in x (edge x_1 x_2) ... (edge x x_3 x_4) ...) #t]
  [(in x (edge x_1 x_2) ... (edge x_3 x x_4) ...) #t]
  [(in x (e_1 ... ) ) #f])

(define-metafunction Graph
  contains-node : x (e ...) -> boolean
  [(contains-node x ()) #f]
  [(contains-node x (e ...)]


;; Design the function good, which determines whether or not the edges in a Graph
;; g mention only names that also name a node in g. We changed it to good? since
;; it returns a boolean.
(define-metafunction Graph
  good? : g -> boolean
  [(good? (graph n ... e ...)) #t])

;(test-equal (term (good? (graph () ()))) #t)
(test-equal (term (good? ,g1)) #t)
;(test-equal (term (good? ,g2)) #f)

;; run tests
(test-results)



