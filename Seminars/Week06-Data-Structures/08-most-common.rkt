#lang racket

(define (count-occurences el l) (length (filter (lambda (x) (equal? x el)) l)))

(define (most-common l)
  (define (iter current max-count lst)
    (if (null? lst)
        current
        (let ([count (count-occurences (car lst) lst)])
          (if (> count max-count)
              (iter (car lst) count (cdr lst))
              (iter current max-count (cdr lst))
              )
          )
        )
    )
  (iter 0 0 l)
  )

(most-common '(1 2 3 2 2 1)) ;; => 2