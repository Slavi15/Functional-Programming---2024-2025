#lang racket

(define (insert x n l)
  (cond
    [(= n 0) (cons x l)]
    [(null? l) (list x)]
    [else (cons (car l) (insert x (- n 1) (cdr l)))]
    )
  )

(insert 3 2 '(1 2 4 5)) ;; => '(1 2 3 4 5)
(insert 'ne6to 10000 '(ni6to)) ;; => '(ni6to ne6to)