#lang racket

(define (dimensions m)
  (if (null? m)
      `()
      (cons (length m) (length (car m)))
      )
  )

(dimensions '((1 2 3) (4 5 6))) ;; => '(2 . 3)