#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_metafunctions.rkt")

;; test meta->mathOperation
; addition
(test-equal (term (meta->mathOperation 3 + 2)) (term 5))
(test-equal (term (meta->mathOperation 3.12 + 2)) (term 5.12))

; subtraction
(test-equal (term (meta->mathOperation 2 - 3)) (term -1))
(test-equal (term (meta->mathOperation 3.12 - 2)) (term 1.12))

; multiplication
(test-equal (term (meta->mathOperation 3 * 2)) (term 6))
(test-equal (term (meta->mathOperation 3.12 * 2)) (term 6.24))

; division
(test-equal (term (meta->mathOperation 4 / 2)) (term 2))
(test-equal (term (meta->mathOperation 3.0 / 2.0)) (term 1.5))

; exponents
(test-equal (term (meta->mathOperation 3 ^ 2)) (term 9))
(test-equal (term (meta->mathOperation 2.0 ^ -2.0)) (term 0.25))

; modulus
(test-equal (term (meta->mathOperation 3 % 2)) (term 1))
(test-equal (term (meta->mathOperation 2 % 3)) (term 2))

;; test meta->vectorMathOperation
; multiplication
(test-equal
 (term (meta->vectorMathOperation (3 1 2 3) .* (2 2 2 3)))
 (term (6 2 4 9)))

; will throw error for size issue
;(term (meta->vectorMathOperation (3 1 2 3) .* (3 1)))

; division
(test-equal
 (term (meta->vectorMathOperation (3.0 1.0 10.0 9.0) ./ (3.0 2.0 4.0 5.0)))
 (term (1.0 0.5 2.5 1.8)))

; will throw error for size issue
;(term (meta->vectorMathOperation (3 1 2 3) ./ (3 1)))

(test-results)