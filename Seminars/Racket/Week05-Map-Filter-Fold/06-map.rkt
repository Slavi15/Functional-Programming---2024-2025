#lang racket

(define (map f l) (foldr (lambda (x xs) (cons (f x) xs)) `() l))

(map (lambda (x) (* x 2)) '(1 2 3)) ;; => '(2 4 6)