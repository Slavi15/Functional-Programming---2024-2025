#lang racket

(define (forall? p? l)
  (cond [(empty? l) #t]
        [(not (p? (car l))) #f]
        [else (forall? p? (cdr l))])
  )