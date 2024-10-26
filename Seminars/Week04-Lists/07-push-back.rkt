#lang racket

(define (push-back x l)
  (if (null? l)
      (list x)
      (cons (car l) (push-back x (cdr l))))
  )

(push-back 5 '(1 2 3 4)) ;; => '(1 2 3 4 5)
(push-back #f '()) ;; => '(#f)