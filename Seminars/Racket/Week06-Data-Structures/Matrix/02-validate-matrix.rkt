#lang racket

(define (all? p? l) (foldr (lambda (x xs) (and (p? x) xs)) #t l))

(define (mat? m)
  (and
   (list? m)
   (all? list? m)
   (let ([row-length (length (car m))])
     (and
      (not (zero? row-length))
      (all? (lambda (row) (= row-length (length row))) (cdr m))
      )
     )
   )
  )

(mat? `((1 2 3) (4 5 6) (7 8 9)))