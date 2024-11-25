#lang racket

(define (member? x l)
  (cond [(null? l) #f]
        [(eqv? x (car l)) l]
        [else (member? x (cdr l))])
  )

(member? "test" '(1 "test" 3 4)) ;; => '("test" 3 4)
(member? 5 '(1 "test"  3 4))     ;; => #f