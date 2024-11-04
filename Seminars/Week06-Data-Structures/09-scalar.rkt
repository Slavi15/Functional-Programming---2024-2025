#lang racket

(define (scalar-product xs ys)
  (if (or (null? xs) (null? ys))
      0
      (+
       (* (car xs) (car ys))
       (scalar-product (cdr xs) (cdr ys))
       )
      )
  )

(scalar-product `(1 2 3) `(4 5 6))