#lang racket

(define (any? p? l) (foldr (lambda (x xs) (if (p? x) #t xs)) #f l))

(any? odd? '(1 2 3 4 5)) ;; => #t
(any? odd? '(2 4 6))     ;; => #f