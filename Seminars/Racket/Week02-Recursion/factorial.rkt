#lang racket

(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1))))
)

(define (fact-iter n)
  (define (iter product i)
    (if (zero? i)
        product
        (iter (* product i) (- i 1)))
  )
  (iter 1 n)
)