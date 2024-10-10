#lang racket

(define // quotient)
(define % modulo)

(define (perfect? n)
  (define (iter sum i)
    (if (= i (- n 1))
        (= sum n)
        (iter (+ sum (if (zero? (% n i)) i 0)) (+ i 1)))
  )
  (iter 0 1)
)