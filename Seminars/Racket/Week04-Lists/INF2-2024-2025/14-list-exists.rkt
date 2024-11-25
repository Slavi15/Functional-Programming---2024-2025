#lang racket

(define (exists? p? l)
  (cond [(empty? l) #f]
        [(p? (car l)) #t]
        [else (exists? p? (cdr l))])
  )