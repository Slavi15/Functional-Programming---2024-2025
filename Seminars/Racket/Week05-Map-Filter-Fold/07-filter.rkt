#lang racket

(define (filter f l) (foldr (lambda (x xs) (if (f x) (cons x xs) xs)) `() l))

(filter odd? '(1 2 3 4 5)) ;; => '(1 3 5)