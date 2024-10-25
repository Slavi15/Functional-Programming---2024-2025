#lang racket

(define (sum l)
  (define (iter res list)
    (if (empty? list)
        res
        (let ([curr (car list)])
          (iter (+ res curr) (cdr list))
          )
        )
    )
  (iter 0 l)
  )