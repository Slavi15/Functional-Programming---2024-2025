#lang racket

(define (sum l) (foldr (lambda (x xs) (+ x xs)) 0 l))

(sum '(1 2 3)) ;; => 6