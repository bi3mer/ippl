;; Colan Biemer, Avijit Ghosh
;; biemer.c@husky.neu.edu, avijit@ccs.neu.edu

(define-union-language ST LamBool Lam)

We use this option since we need to be able to take in a LamBool expression which the other version given cannot express. Without this version we wouldn't be able to write a translate function with our current knowledge of how Racket works.
