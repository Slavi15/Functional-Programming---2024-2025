#lang racket

(define (len l)
  (define (iter acc lst)
    (if (null? lst)
        acc
        (iter (+ 1 acc) (cdr lst)))
    )
  (iter 0 l)
  )

(len '(1 2 3))     ;; => 3
(len (list))       ;; => 0
(len (cons 1 '())) ;; => 1