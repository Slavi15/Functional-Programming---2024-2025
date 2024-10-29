#lang racket

(define (my-flatten lst)
  (cond
    [(null? lst) `()]
    [(list? (car lst)) (append (my-flatten (car lst)) (my-flatten (cdr lst)))]
    [else (cons (car lst) (my-flatten (cdr lst)))])
  )

(my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))
(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))