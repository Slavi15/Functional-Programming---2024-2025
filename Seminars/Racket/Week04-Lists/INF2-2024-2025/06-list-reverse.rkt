#lang racket

(define (list-reverse l) (foldl cons `() l))

(define (my-reverse l)
  (define (iter lst list)
    (if (empty? list)
        lst
        (iter (cons (car list) lst) (cdr list)))
    )
  (iter `() l)
  )