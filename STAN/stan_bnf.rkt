#lang racket
(require redex)

;; NOTES:
;; - the current approach for restrictions does allow a user to create multiple
;;   restrictions of the same type for a variable. For example, there is
;;   nothing wrong with giving a variable 100 lower bounds. The implementation
;;   will only use the last one but this could be seen as a limitation to the
;;   implementation provided.
;; - syntax simplified so a variable has to be declared before its value can be
;;   assigned.
;; - syntax simplified for <><> case to be < number ... > to simplify the AST to
;;   be more manageable.
;; - Arrays not implemened

;;;; Stan language
(define-language STAN
  (e ::= pv bool x (e ...) (e < e >) (e < e e >) (e MO e) (e AMO e) (e MBO e) (- e)) ;; 
  (s ::=
     ss
     (s ...)
     (i C x)
     (r C x)
     (vec-type int C x)
     (mat-type int int C x)
     (x = e)
     (x < e > = e)
     (x < e e > = e) 
     (if e then s else s)
     (for x in e : e do s)) 

  (ss ::= skip (skip ...))
  
  ;; primitive values
  (error ::= string)
  (int ::= integer)
  (num ::= number)
  (pv ::= int num)
  (bool ::= boolean)

  ;; vector
  (vec-type ::= v simplex ordered positive-ordered row-vector unit-vector)
  (vec ::= (number ...))

  ;; matrix, note that alwaysone is not in stan but is a stand in for more
  ;; complicated constraints that don't get at what we are trying to understand.
  (mat-type ::= m alwaysone) 
  (mat ::= (vec ...))

    ;; types
  (t ::= i r vec-type mat-type)

  ;; Values
  (V ::= pv vec mat)

  ;; Environment Values
  (EV ::= V x)
  
  ;; Constraints. Non-relevant constraints will not be used in the final checking
  (IC ::= 
      (none)
      (lower = number)
      (upper = number)
      (offset = number : multiplier = number))
  (C ::= (IC ...))

  ;; Math Operators
  (MO ::= + - * / ^ %)

  ;; Array Math Operators
  (AMO ::= .* ./)

  ;; Math Boolean Operators
  (MBO ::= > >= < <= == !=)

  ;; Operation Semantics
  (E ::=
     hole
     (ss ... E s ...)
     
     ;; assignments
     (x = E) 

     (x = E < e >)
     (x = vec < E >)
     (x = mat < E >)

     (x = E < e e >)
     (x = mat < E e >)
     (x = mat < int E >)

     (x  < E > = e)
     (x < int > = E)

     (x < E e > = e)
     (x < int E > = e)

     ; getting
     (E < e >)
     (vec < E >)
     (mat < E >)
     
     (E < e e >)
     (mat < E e >)
     (mat < int E >)

     ; operations
     (E MO e)
     (pv MO E)
     (- E)

     (E AMO e)
     (vec AMO E)

     (E MBO e)
     (pv MBO E)

     ; control flow
     (if E then s else s)
     (for x in E : e do s)
     (for x in int : E do s))

  ;; variable
  (x ::= variable-not-otherwise-mentioned))

(provide STAN)