#lang racket

(define (nth-column m n)
  (define (iter mtx i)
    (if (= i n)
        (map car mtx)
        (iter (map cdr mtx) (+ i 1))
        )
    )
  (iter m 1)
  )

(nth-column '((1 2 3) (4 5 6) (7 8 9)) 2) ;; => '(2 5 8)