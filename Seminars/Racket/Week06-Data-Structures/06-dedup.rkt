#lang racket

(define (count-occurences el l) (foldr (lambda (x acc) (if (= x el) (+ 1 acc) acc)) 0 l))

(define (dedup l)
  (cond
    [(null? l) `()]
    [(= (count-occurences (car l) l) 1) (cons (car l) (dedup (cdr l)))]
    [else (dedup (cdr l))])
  )

(dedup `(1 2 3 3 4 5 5 6))