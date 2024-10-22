#lang racket

(define (foldr l op init)
  (if (empty? l)
      init
      (op (car l) (foldr (cdr l) op init))
      )
  )