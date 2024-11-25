#lang racket

(define (take n l)
  (if (or (zero? n) (null? l))
      `()
      (cons (car l) (take (- n 1) (cdr l)))
      )
  )

(take 5 '(1 2 3 4 5 6 7 8 9 10)) ;; => '(1 2 3 4 5)
(take 0 '(1 2 3)) ;; => '()
(take 5 '(1 2 3)) ;; => '(1 2 3)