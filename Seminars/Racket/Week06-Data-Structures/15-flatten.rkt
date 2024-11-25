#lang racket

(define (flatten xss)
  (cond
    [(null? xss) `()]
    [(list? (car xss)) (append (flatten (car xss)) (flatten (cdr xss)))]
    [else (cons (car xss) (flatten (cdr xss)))])
  )

(flatten '(1 2 3))                ;; => '(1 2 3)
(flatten '(1 (2 3) (3 (4 (5)))))  ;; => '(1 2 3 4 5)
(flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12))))))