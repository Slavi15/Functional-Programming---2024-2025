#lang racket

(define (min l) (foldr (lambda (x xs) (if (< x xs) x xs)) (car l) (cdr l)))

(define (remove-min min-num l)
  (if (= min-num (car l))
      (cdr l)
      (cons (car l) (remove-min min-num (cdr l)))
      )
  )

(define (selection-sort l)
  (if (null? l)
      `()
      (let* ([minimum (min l)]
            [rest (remove-min minimum l)])
        (cons minimum (selection-sort rest))
        )
      )
  )

(define lst `(5 4.4 3 6 1))
(selection-sort lst) ;; => `(1 3 4.4 5 6)