#lang racket

(define 1+ (lambda (x) (+ 1 x)))

(define (deep-delete l)
  (define (iter depth lst)
    (cond
      [(null? lst) `()]
      [(list? (car lst)) (cons (iter (1+ depth) (car lst)) (iter depth (cdr lst)))]
      [(>= (car lst) depth) (cons (car lst) (iter depth (cdr lst)))]
      [else (iter depth (cdr lst))])
    )
  (iter 1 l)
  )

(deep-delete '(1 (2 (2 4) 1) 0 (3 (1))))
(equal? (deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) '(1 (2 (4)) (3 ())))