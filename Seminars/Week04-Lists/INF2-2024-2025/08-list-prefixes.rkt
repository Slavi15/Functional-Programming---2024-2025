#lang racket

(define (all-prefixes l)
  (define (iter lst list)
    (if (empty? list)
        lst
        (iter (cons (reverse (cdr (cons `() list))) lst) (cdr list)))
    )
  (iter `() (reverse l))
  )

(all-prefixes `(1 2 3 4 5)) ; => '((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))