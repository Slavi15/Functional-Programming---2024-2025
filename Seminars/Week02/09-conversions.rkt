#lang racket

(define // quotient)
(define % modulo)

(define (base1-to-base2 base1 base2 n)
  (define (iter acc k i)
    (if (zero? i)
        acc
        (iter (+ acc
                 (* (expt base1 k) (% i base2))) (+ k 1) (// i base2))
        )
  )
  (iter 0 0 n)
)

(define (binary-to-decimal n)
  (base1-to-base2 2 10 n)
)

(define (decimal-to-binary n)
  (base1-to-base2 10 2 n)
)