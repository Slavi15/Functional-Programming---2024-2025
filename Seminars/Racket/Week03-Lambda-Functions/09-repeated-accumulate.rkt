#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define 1+ (lambda (x) (+ 1 x)))

(define (repeated n f x)
  (accumulate
   (lambda (x y) (f y))
   x
   1
   n
   f
   1+
   )
  )