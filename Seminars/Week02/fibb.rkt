#lang racket

(define (fibb n)
  (cond [(= n 0) 1]
        [(= n 1) 1]
        [else (+ (fibb (- n 1)) (fibb (- n 2)))])
)

(define (fibb-iter n)
  (define (iter n1 n2 i)
    (if (zero? i)
        n2
        (iter n2 (+ n1 n2) (- i 1)))
  )
  (iter 0 1 n)
)