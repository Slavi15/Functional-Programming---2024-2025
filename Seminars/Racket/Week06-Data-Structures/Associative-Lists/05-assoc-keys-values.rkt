#lang racket

(define (assoc-keys lst)
  (if (null? lst)
      `()
      (cons (caar lst) (assoc-keys (cdr lst)))
      )
  )

(define (assoc-values lst)
  (if (null? lst)
      `()
      (cons (cdar lst) (assoc-values (cdr lst)))
      )
  )

(equal? (assoc-keys '((1 . "One") (2 . "Two") (3 . "Three"))) '(1 2 3))
(equal? (assoc-keys '()) '())

(equal? (assoc-values '((1 . "One") (2 . "Two") (3 . "Three"))) '("One" "Two" "Three"))
(equal? (assoc-values '()) '())