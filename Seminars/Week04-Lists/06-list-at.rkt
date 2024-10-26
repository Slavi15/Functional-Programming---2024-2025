#lang racket

(define (at n l)
  (define (iter i lst)
    (cond [(null? lst) #f]
          [(= i 0) (car lst)]
          [else (iter (- i 1) (cdr lst))])
    )
  (iter n l)
  )

(at 0 '(1 2 3)) ;; => 1
(at 3 '(1 2 3)) ;; => #f