#lang racket

(define (any? p? l) (foldr (lambda (x xs) (or (p? x) xs)) #f l))

(define (all? p? l) (foldr (lambda (x xs) (and (p? x) xs)) #t l))

(any? odd? '(1 2 3 4 5)) ;; => #t
(any? odd? '(2 4 6))     ;; => #f

(all? even? '(1 2 3 4 5)) ;; => #f
(all? even? '(2 4 6))     ;; => #t