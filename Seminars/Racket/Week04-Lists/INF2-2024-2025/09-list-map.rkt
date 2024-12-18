#lang racket

(define (list-map f l) (foldr (lambda (x xs) (cons (f x) xs)) `() l))

#|
(define (my-map f l)
  (define (iter lst list)
    (if (empty? list)
        (reverse lst)
        (iter (cons (f (car list)) lst) (cdr list)))
    )
  (iter `() l)
  )
|#