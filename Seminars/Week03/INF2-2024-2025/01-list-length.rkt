#lang racket

(define (len l)
  (define (iter sum lst)
    (if (empty? lst)
        sum
        (iter (+ sum 1) (cdr lst)))
    )
  (iter 0 l)
  )