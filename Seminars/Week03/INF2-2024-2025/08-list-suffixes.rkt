#lang racket

(define (all-suffix l)
  (define (iter lst list)
    (if (empty? list)
        lst
        (iter (cons (cdr (cons `() list)) lst) (cdr list)))
    )
  (iter `() l)
  )