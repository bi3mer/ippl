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
;; TODO: add citation note
(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #t]
  [(in x (x_1 ... ) ) #f])

;(test-equal (term (in x (x y z))) #t)
;(test-equal (term (in x (y z))) #f)

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