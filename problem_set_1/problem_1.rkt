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
;; to replace all of it. N is the replacement weight
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

;; run tests
(test-results)
