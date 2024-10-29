#lang racket

(define ! (lambda (x) (not x)))

(define (take-while p? l)
  (if (! (p? (car l)))
      `()
      (cons (car l) (take-while p? (cdr l)))
      )
  )

(take-while (lambda (x) (> x 5)) '(8 7 6 5 4 3)) ;; => '(8 7 6)
(take-while odd? '(2 1 1 1 1)) ;; => '()