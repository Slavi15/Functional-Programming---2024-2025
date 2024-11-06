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

(define (transpose xss) (apply map list xss))

(define (matrix-multiply m t)
  (map
   (lambda (row)
     (map
      (lambda (col) (scalar-product row col))
      (transpose t))
     )
   m)
  )

(matrix-multiply `((2 2 2) (2 2 2) (2 2 2)) `((2 2 2) (2 2 2) (2 2 2)))