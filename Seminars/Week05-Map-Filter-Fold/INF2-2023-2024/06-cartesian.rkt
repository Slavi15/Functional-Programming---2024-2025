#lang racket

(define (mega-pair x ys)
  (if (null? ys)
      `()
      (cons (cons x (car ys)) (mega-pair x (cdr ys)))
      )
  )

(define (append xs ys)
  (cond
    [(null? xs) ys]
    [else (cons (car xs) (append (cdr xs) ys))])
  )

(define (cartesian xs ys)
  (if (null? xs)
      `()
      (append (mega-pair (car xs) ys) (cartesian (cdr xs) ys)))
  )

(cartesian '(1 2) '(3 4))
(equal? (cartesian '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))

(cartesian '(1 2 3 4 5) '(6 7 8))
(equal? (cartesian '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))