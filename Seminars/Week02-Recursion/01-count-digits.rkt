#lang racket

(define // quotient)
(define % modulo)

(define (count-digits n)
  (define (iter acc n)
    (if (< -10 n 10)
        acc
        (iter (+ acc 1) (// n 10)))
  )
  (iter 1 n)
)