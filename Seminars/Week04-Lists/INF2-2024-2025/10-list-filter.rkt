#lang racket

(define (list-filter p? l) (foldr (lambda (x xs) (if (p? x) (cons x xs) xs)) `() l))

(define (my-filter p? l)
  (define (iter lst list)
    (if (empty? list)
        (reverse lst)
        (let ([current (car list)])
          (iter (if (p? current) (cons current lst) lst) (cdr list))
          )
        )
    )
  (iter `() l)
  )