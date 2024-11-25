#lang racket

(define (range a b)
  (cond [(> a b) `()]
        [(= a b) (list a)]
        [else (cons a (range (+ a 1) b))])
  )

(range 1 10) ;; => '(1 2 3 4 5 6 7 8 9 10)
(range 0 0) ;; => '(0)
(range 3 1) ;; => '()