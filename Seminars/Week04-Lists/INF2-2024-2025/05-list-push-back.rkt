#lang racket

(define (push-back x lst)
  (if (empty? lst)
      (list x)
      (cons (car lst) (push-back x (cdr lst))))
  )