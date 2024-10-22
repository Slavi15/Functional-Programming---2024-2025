#lang racket

(define (repeat n x)
  (define (iter i list)
    (if (= i n)
        list
        (iter (+ i 1) (cons x list)))
    )
  (iter 0 `())
  )