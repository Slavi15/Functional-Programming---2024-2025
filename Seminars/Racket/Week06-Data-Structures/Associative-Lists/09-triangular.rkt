#lang racket

(define (triangular? xss)
  (define (helper row idx)
    (andmap (lambda (x) (zero? x)) (take row idx))
    )
  (andmap (lambda (idx) (helper (list-ref xss idx) idx)) (range 0 (length xss)))
  )

(equal? (triangular? '((1 2 3)
                       (0 5 6)
                       (0 0 9))) #t)

(equal? (triangular? '((0 2 3)
                       (0 0 6)
                       (1 0 0))) #f)

(equal? (triangular? '((1 2 3)
                       (1 5 6)
                       (0 0 9))) #f)

(equal? (triangular? '((1 2 3 4)
                       (0 5 6 7)
                       (0 0 8 9)
                       (0 0 0 9))) #t)