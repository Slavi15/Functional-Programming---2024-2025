#lang racket

(define (my-filter p? l)
  (define (iter lst list)
    (if (empty? list)
        (reverse lst)
        (let ([current (car list)])
          (iter (if (p? current) (cons current lst) lst) (cdr list))
          )
        )
    )
  (iter `() l)
  )