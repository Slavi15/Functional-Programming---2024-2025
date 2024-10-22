#lang racket

(define (at n l)
  (define (iter i lst)
    (cond [(or (> i n) (empty? lst)) #f]
          [(= i n) (car lst)]
          [else (iter (+ i 1) (cdr lst))])
    )
  (iter 0 l)
  )