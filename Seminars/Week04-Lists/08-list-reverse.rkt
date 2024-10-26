#lang racket

(define (reverse l) (foldl cons `() l))

(define (reverse-iter l)
  (define (iter ls lst)
    (if (null? lst)
        ls
        (iter (cons (car lst) ls) (cdr lst)))
    )
  (iter `() l)
  )

(reverse '(1 2 3)) ;; => '(3 2 1)
(reverse-iter '(1 2 3)) ;; => '(3 2 1)