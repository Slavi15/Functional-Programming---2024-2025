#lang racket

(define (all? p? l) (foldr (lambda (x xs) (if (not (p? x)) #f xs)) #t l))

(all? odd? '(1 2 3 4 5)) ;; => #f
(all? odd? '(2 4 6))     ;; => #f
(all? even? '(2 4 6))     ;; => #t