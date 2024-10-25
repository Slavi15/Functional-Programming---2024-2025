#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define 1+ (lambda (x) (+ 1 x)))

(define (any? p? a b)
  (accumulate
   (lambda (x y) (or x y))
   #f
   a
   b
   (lambda (x) (p? x))
   1+
   )
  )