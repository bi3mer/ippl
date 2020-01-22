#lang racket
(require redex)

(define-language mobile
  (m w        ; atomic sculpture (weight)
     (m m w)) ; composite
  (w number)) ; weight is a number

(define m1 (term 3))
(define m2 (term 4))
(define m3 (term (,m1 ,m2 5)))

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

;; receives a mobile and returns a boolean to represent whether the weight of all
;; components on the left side are equal to the right side.


;; (test-equal mobile (term (balanced? )))



(test-results)
