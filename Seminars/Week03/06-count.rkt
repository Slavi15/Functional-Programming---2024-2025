#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (count p? a b)
  (accumulate
   +
   0
   a
   b
   (lambda (x) (if (p? x) 1 0))
   (lambda (x) (+ 1 x))
   )
  )