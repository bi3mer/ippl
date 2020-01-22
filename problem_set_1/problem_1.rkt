#lang racket
(require redex)

(define-language mobile
  (m w        ; atomic sculpture (weight)
     (m m w)) ; composite
  (w number)) ; weight is a number


(redex-match? mobile m (term 3))

(define m1 (term 3))
(define m2 (term 4))
(redex-match? mobile m (term (,m1 ,m2 5)))

;; receives a mobile and returns a boolean to represent whether the weight of all
;; components on the left side are equal to the right side.


;; (test-equal mobile (term (balanced? )))