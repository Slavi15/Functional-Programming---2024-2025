#lang racket

(define // quotient)
(define % modulo)

(define (count-divisors n)
  (define (iter cnt i)
    (if (= n i)
        cnt
        (iter (+ cnt (if (zero? (% n i)) 1 0)) (+ i 1)))
  )
  (iter 1 1)
)