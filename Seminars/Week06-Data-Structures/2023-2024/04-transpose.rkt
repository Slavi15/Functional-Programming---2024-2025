#lang racket

(define (transpose m)
  (if (null? (car m))
      `()
      (cons (map car m) (transpose (map cdr m)))
      )
  )

(define (transpose2 m) (apply map list m))

(transpose '((1 2 3) (4 5 6) (7 8 9))) ;; => '((1 4 7) (2 5 8) (3 6 9))

(newline)

(transpose2 '((1 2 3) (4 5 6) (7 8 9))) ;; => '((1 4 7) (2 5 8) (3 6 9))