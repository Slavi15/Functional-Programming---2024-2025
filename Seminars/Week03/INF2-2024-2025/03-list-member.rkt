#lang racket

(define (my-member? l x)
  (cond [(empty? l) #f]
        [(= x (car l)) #t]
        [else (my-member? (cdr l) x)])
  )