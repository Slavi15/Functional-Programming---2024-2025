#lang racket

(define (all-prefixes l)
  (define (iter lst list)
    (if (empty? list)
        lst
        (iter (cons (reverse (cdr (cons `() list))) lst) (cdr list)))
    )
  (iter `() (reverse l))
  )