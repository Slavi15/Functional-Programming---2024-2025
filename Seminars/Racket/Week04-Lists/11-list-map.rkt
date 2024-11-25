#lang racket

(define (map f l) (foldr (lambda (x xs) (cons (f x) xs)) `() l))

(map (lambda (x) (+ x 1)) '(1 2 3)) ;; => '(2 3 4)

(define (sq x) (* x x))

(map sq '(1 2 3 4 5)) ;; => '(1 4 9 16 25)