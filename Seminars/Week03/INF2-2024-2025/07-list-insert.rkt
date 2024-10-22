#lang racket

(define (insert x n l)
  (define (iter i lst list)
    (cond [(empty? list) (reverse lst)]
          [(= i n) (iter (+ i 1) (cons x lst) list)]
          [else (iter (+ i 1) (cons (car list) lst) (cdr list))])
    )
  (iter 0 `() l)
  )