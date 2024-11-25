#lang racket

(define (zip-with op l1 l2)
  (if (or (null? l1) (null? l2))
      `()
      (cons (op (car l1) (car l2)) (zip-with op (cdr l1) (cdr l2)))
      )
  )

(zip-with + '(1 2 3) '(5 4 2)) ;; => '(6 6 5)