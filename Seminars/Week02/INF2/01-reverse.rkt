#lang racket

(define // quotient)
(define % modulo)

(define (concat n digit)
  (+ (* n 10) digit)
)

(define (reverse n)
  (define (iter acc n)
    (if (zero? n)
        acc
        (iter (concat acc (% n 10)) (// n 10)))
  )
  (iter 0 n)
)

(= (reverse 0) 0)
(= (reverse 123) 321)
(= (reverse 987654321) 123456789)
(= (reverse 12000) 21)