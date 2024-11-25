#lang racket

(define (all? p? l) (foldr (lambda (x xs) (and (p? x) xs)) #t l))

(define (all-columns? p? m)
  (if (null? (car m))
      #t
      (and (all? p? (map car m)) (all-columns? p? (map cdr m)))
      )
  )

(all-columns? even? `((2 4 6) (2 4 6) (2 4 6)))
(all-columns? even? `((2 4 6) (2 3 6) (2 4 6)))