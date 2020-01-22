#lang racket
(require redex)

(define-language mobile
  (m w        ; atomic sculpture (weight)
     (m m w)) ; composite
  (w number)) ; weight is a number

(define m1 (term 3))
(define m2 (term 4))
(define m3 (term (,m1 ,m2 5)))
(define m4 (term (,m1 ,m3 7)))

(test-equal (redex-match? mobile m (term 3)) #t)
(test-equal (redex-match? mobile m (term (,m1 ,m2 5))) #t)
(test-equal (redex-match? mobile m (term ,m3)) #t)

;; num-atomics
(define-metafunction mobile
  num-atomics : m -> natural
  [(num-atomics w) 1]
  [(num-atomics (m_1 m_2 w)) ,(+ (+ (term (num-atomics m_1)) (term (num-atomics m_2))) 1)])

(test-equal (term (num-atomics ,m1)) 1)
(test-equal (term (num-atomics ,m2)) 1)
(test-equal (term (num-atomics ,m3)) 3)
(test-equal (term (num-atomics ,m4)) 5)

;; total-weight
(define-metafunction mobile
  total-weight : m -> natural
  [(total-weight w) w]
  [(total-weight (m_1 m_2 w)) ,(+ (+ (term (total-weight m_1)) (term (total-weight m_2))) (term w))])

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

;; replace


;; balanced?


;; run tests
(test-results)
