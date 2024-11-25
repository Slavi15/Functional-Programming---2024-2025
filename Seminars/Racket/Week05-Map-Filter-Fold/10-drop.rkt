#lang racket

(define (drop n l)
  (if (or (zero? n) (null? l))
      l
      (drop (- n 1) (cdr l))
      )
  )

(drop 5 '(1 2 3 4 5 6 7 8 9 10)) ;; => '(6 7 8 9 10)
(drop 0 '(1 2 3)) ;; => '(1 2 3)
(drop 5 '(1 2 3)) ;; => '()