#lang racket

(define (make-f-alist lst f) (foldr (lambda (x xs) (cons (cons x (f x)) xs)) `() lst))

(make-f-alist '(1 2 3) add1)
(equal? (make-f-alist '(1 2 3) add1) '((1 . 2) (2 . 3) (3 . 4)))