#lang racket

(define (tabulate f)
  (lambda (a b) (map (lambda (x) (cons x (f x))) (range a (+ 1 b))))
  )

(equal? ((tabulate (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))