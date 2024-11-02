#lang racket

(define (assoc-del key lst)
  (cond
    [(null? lst) `()]
    [(equal? key (caar lst)) (cdr lst)]
    [else (cons (car lst) (assoc-del key (cdr lst)))])
  )

(assoc-del 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One")))
(equal? (assoc-del 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) '((2 . "Two") (3 . "Three")))

(newline)

(assoc-del 1 (list '(2 . "Two") '(3 . "Three")))
(equal? (assoc-del 1 (list '(2 . "Two") '(3 . "Three"))) '((2 . "Two") (3 . "Three")))