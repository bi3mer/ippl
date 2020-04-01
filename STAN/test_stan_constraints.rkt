#lang racket
(require redex)
(require "stan_bnf.rkt")
(require "stan_environment.rkt")
(require "stan_constraints.rkt")

;; integer constraint tests
(test-equal
 (term (validateConstraint x 3 ((none)) i))
 (term ((x "no type specific constraint") ((((x "no error")))))))

(test-equal
 (term (validateConstraint x 3 ((upper = 2)) i))
 (term ((x "no type specific constraint") ((((x "upper constraint not met")))))))

(test-equal
 (term (validateConstraint x 3 ((lower = 4)) i))
 (term ((x "no type specific constraint") ((((x "lower constraint not met")))))))

(test-equal
 (term (validateConstraint x 3 ((lower = 2) (upper = 4)) i))
 (term ((x "no type specific constraint") ((((x "no error") (x "no error")))))))

;; real constraint tests
(test-equal
 (term (validateConstraint x 3.0 ((none)) r))
 (term ((x "no type specific constraint") ((((x "no error")))))))

(test-equal
 (term (validateConstraint x 2.01 ((upper = 2)) r))
 (term ((x "no type specific constraint") ((((x "upper constraint not met")))))))

(test-equal
 (term (validateConstraint x 3.99 ((lower = 4)) r))
 (term ((x "no type specific constraint") ((((x "lower constraint not met")))))))

(test-equal
 (term (validateConstraint x 2.41 ((lower = 2.4) (upper = 2.42)) r))
 (term ((x "no type specific constraint") ((((x "no error") (x "no error")))))))

;; v constraint tests
(test-equal
 (term (validateConstraint x (3 1 2) ((none)) v))
 (term
  ((x "no type specific constraint")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((upper = 3)) v))
 (term
  ((x "no type specific constraint")
   ((((x "upper constraint not met")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1)) v))
 (term
  ((x "no type specific constraint")
   ((((x "no error")) ((x "lower constraint not met")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1) (upper = 3)) v))
 (term ((x "no type specific constraint")
        ((((x "no error") (x "upper constraint not met")) ; 3
        ((x "lower constraint not met") (x "no error"))   ; 1
        ((x "no error") (x "no error")))))))              ; 2

;; row-vector constriant tests
(test-equal
 (term (validateConstraint x (3 1 2) ((none)) row-vector))
 (term
  ((x "no type specific constraint")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((upper = 3)) row-vector))
 (term
  ((x "no type specific constraint")
   ((((x "upper constraint not met")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1)) row-vector))
 (term
  ((x "no type specific constraint")
   ((((x "no error")) ((x "lower constraint not met")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (3 1 2) ((lower = 1) (upper = 3)) row-vector))
 (term ((x "no type specific constraint")
        ((((x "no error") (x "upper constraint not met")) ; 3
        ((x "lower constraint not met") (x "no error"))   ; 1
        ((x "no error") (x "no error")))))))              ; 2

;; ordered constraint tests
(test-equal
 (term (validateConstraint x (3 1 2) ((none)) ordered))
 (term
  ((x "vector is not ordered")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (-10 0 100) ((none)) ordered))
 (term
  ((x "vector is ordered")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

;; simplex constraint tests

;; ordered positive tests
(test-equal
 (term (validateConstraint x (3 1 2) ((none)) positive-ordered))
 (term
  ((x "vector is not positive ordered")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (-10 0 100) ((none)) positive-ordered))
 (term
  ((x "vector is not positive ordered")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x (1 2 100) ((none)) positive-ordered))
 (term
  ((x "vector is positive ordered")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

;; unit vector tests
(test-equal
 (term (validateConstraint x (1.0) ((none)) unit-vector))
 (term ((x "vector is a unit vector") ((((x "no error")))))))

(test-equal
 (term (validateConstraint x (0.1 0.1 0.799) ((none)) unit-vector))
 (term
  ((x "vector is not a unit vector")
   ((((x "no error")) ((x "no error")) ((x "no error")))))))

;; matrix tests. This builds on vector so no extensive unit tests are done.
(test-equal
 (term (validateConstraint x ((0.0 0.0)) ((none)) m))
 (term ((x "no type constraint") ((((x "no error")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x ((0.0 1.01)) ((lower = 1)) alwaysone))
 (term ((x "does not contain all ones") ((((x "lower constraint not met")) ((x "no error")))))))

(test-equal
 (term (validateConstraint x ((1.0 1.0)) ((lower = 0)) alwaysone))
 (term ((x "contains all ones") ((((x "no error")) ((x "no error")))))))

;; constraints->validate
(define env
  (term ((x 0 ((lower = 0)) i)
         (y 1.3 ((upper = 1.0)) r)
         (z (0.3 0.1) ((none)) unit-vector))))
(test-equal (redex-match? STAN_E σ env) #t)

(define expected
  (term
   (((x "no type specific constraint")
     ((((x "lower constraint not met")))))
    ((y "no type specific constraint")
     ((((y "upper constraint not met")))))
    ((z "vector is not a unit vector")
     ((((z "no error")) ((z "no error"))))))))

(test-equal (term (constraints->validate ,env)) expected)

;; constraint updates
(test-equal
 (term (constraints->update ((x 3.0 ((none)) r))))
 (term ((x 3.0 ((none)) r))))

(test-equal
 (term (constraints->update ((x 3 (offset = 1.0 : multiplier = 3.0) i))))
 (term ((x 3 (offset = 1.0 : multiplier = 3.0) i))))

(test-equal
 (term (constraints->update ((x 3.0 ((offset = 1.0 : multiplier = 3.0)) r))))
 (term ((x 10.0 ((offset = 1.0 : multiplier = 3.0)) r))))

(test-equal
 (term (constraints->update ((x (1.0 2.0) ((offset = 1.0 : multiplier = 3.0)) v))))
 (term ((x (4.0 7.0) ((offset = 1.0 : multiplier = 3.0)) v))))

(test-equal
 (term (constraints->update ((x (1.0 2.0) ((offset = 1.0 : multiplier = 3.0)) simplex))))
 (term ((x (4.0 7.0) ((offset = 1.0 : multiplier = 3.0)) simplex))))

(define cu_env (term
                ((x 3.0 ((offset = 1.0 : multiplier = 3.0)) r)
                 (y 3.0 ((offset = 2.0 : multiplier = 1.5)) r)
                 (z 1   ((offset = 3.0 : multiplier = 2.3)) i))))
(define cu_out (term
                ((x 10.0 ((offset = 1.0 : multiplier = 3.0)) r)
                 (y 6.5 ((offset = 2.0 : multiplier = 1.5)) r)
                 (z 1   ((offset = 3.0 : multiplier = 2.3)) i))))
                 
(test-equal (term (constraints->update ,cu_env)) cu_out)

(test-equal (redex-match? STAN_E σ (term ((x (0.0 0.0) ((none)) simplex)))) #t)

(test-equal
 (term (constraints->update ((x ((0.0 0.0)) ((offset = 3 : multiplier = 1)) m))))
 (term ((x ((3.0 3.0)) ((offset = 3 : multiplier = 1)) m))))

(test-equal
 (term (constraints->update ((x ((0.0)) ((none)) m))))
 (term ((x ((0.0)) ((none)) m))))

(test-equal
 (term
  (constraints->update
   ((x ((0.0 1.0 0.0) (1.0 0.0 1.0)) ((offset = 1.0 : multiplier = 3.0)) m))))
 (term ((x ((1.0 4.0 1.0) (4.0 1.0 4.00)) ((offset = 1.0 : multiplier = 3.0)) m))))


(test-results)