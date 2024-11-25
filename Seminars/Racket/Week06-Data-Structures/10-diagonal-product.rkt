#lang racket

(define (main-diagonal mtx)
  (define (iter i xss)
    (if (null? xss)
        `()
        (cons (list-ref (car xss) i) (iter (+ i 1) (cdr xss)))
      )
    )
  (iter 0 mtx)
  )

(define (secondary-diagonal mtx)
  (define (iter i xss)
    (if (null? xss)
        `()
        (cons (list-ref (car xss) i) (iter (- i 1) (cdr xss)))
        )
    )
  (iter (- (length mtx) 1) mtx)
  )

(define (scalar-product xs ys)
  (if (or (null? xs) (null? ys))
      0
      (+
       (* (car xs) (car ys))
       (scalar-product (cdr xs) (cdr ys))
       )
      )
  )

(define (diagonal-product matrix) (scalar-product (main-diagonal matrix) (secondary-diagonal matrix)))

(diagonal-product '((1 0 1) (0 2 0) (3 0 3))) ;; => 14
(diagonal-product '((1 0 3) (0 2 0) (1 0 3))) ;; => 10