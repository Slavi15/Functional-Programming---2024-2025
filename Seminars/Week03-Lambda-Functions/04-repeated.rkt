#lang racket

(define (repeated n f x)
  (if (= n 0)
      x
      (repeated (- n 1) f (f x)))
  )