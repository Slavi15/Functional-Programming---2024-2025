#lang racket

(define ! (lambda (x) (not x)))

(define (drop-while p? l)
  (if (! (p? (car l)))
      l
      (drop-while p? (cdr l))
      )
  )

(drop-while (lambda (x) (> x 5)) '(8 7 6 5 4 3)) ;; => '(5 4 3)
(drop-while odd? '(2 1 1 1 1)) ;; => '(2 1 1 1 1)