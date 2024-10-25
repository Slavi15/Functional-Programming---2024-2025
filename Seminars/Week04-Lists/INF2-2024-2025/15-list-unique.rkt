#lang racket

(define (unique l)
  (define (iter lst list)
    (cond [(empty? list) (reverse lst)]
          [(empty? lst) (iter (cons (car list) lst) (cdr list))]
          [(eq? (car lst) (car list)) (iter lst (cdr list))]
          [else (iter (cons (car list) lst) (cdr list))])
    )
  (iter `() l)
  )
