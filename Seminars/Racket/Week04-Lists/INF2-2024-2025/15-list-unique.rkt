#lang racket

(define (my-not x xs) (not (eqv? x (car xs))))

(define (list-unique l)
  (foldr
   (lambda (x xs)
     (cond [(or (null? xs) (my-not x xs)) (cons x xs)]
           [else xs])) `() l)
  )

(define (unique l)
  (define (iter lst list)
    (cond [(empty? list) (reverse lst)]
          [(empty? lst) (iter (cons (car list) lst) (cdr list))]
          [(eq? (car lst) (car list)) (iter lst (cdr list))]
          [else (iter (cons (car list) lst) (cdr list))])
    )
  (iter `() l)
  )
