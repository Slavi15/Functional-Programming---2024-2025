#lang racket

(define (concat l1 l2) (foldr (lambda (x xs) (cons x xs)) l2 l1))

(concat '(1 2 3) '(4 5 6)) ; => `(1 2 3 4 5 6)