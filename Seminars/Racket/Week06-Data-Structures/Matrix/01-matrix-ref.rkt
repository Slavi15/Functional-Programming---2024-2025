#lang racket

(define (at l i)
  (cond
    [(null? l) `()]
    [(zero? i) (car l)]
    [else (at (cdr l) (- i 1))])
  )

(define (matrix-ref m i j) (at (at m i) j))

(matrix-ref '((1 2 3) (4 5 6) (7 8 9)) 2 1) ;; => 8