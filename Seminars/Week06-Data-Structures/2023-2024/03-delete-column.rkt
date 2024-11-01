#lang racket

(define 1- (lambda (x) (- x 1)))

(define (delete i lst)
  (if (zero? i)
      (cdr lst)
      (cons (car lst) (delete (1- i) (cdr lst)))
      )
  )

(define (delete-column i m)
  (map (lambda (row) (delete i row)) m)
  )

(delete-column 1 '((1 2 3 5)
                   (6 7 8 0)
                   (5 6 3 4)))
;; => '((1 3 5)
;;      (6 8 0)
;;      (5 3 4))