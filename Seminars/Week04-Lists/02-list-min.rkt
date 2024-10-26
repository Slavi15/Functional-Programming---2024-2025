#lang racket

(define (min l) (foldr (lambda (x xs) (if (< x xs) x xs)) (car l) (cdr l)))

(min '(1 2 3)) ;; => 1
(min '(5 7 3)) ;; => 3