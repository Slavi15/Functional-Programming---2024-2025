#lang racket

(define (min l) (foldr (lambda (x xs) (if (< x xs) x xs)) (car l) (cdr l)))

(define (max ll) (foldr (lambda (x xs) (if (> (min x) xs) (min x) xs)) (min (car ll)) (cdr ll)))

(define (maxmin fm l)
  (max
   (map
    (lambda (fm-row)
      (map
       (lambda (fm)
         (foldr (lambda (x acc) (if (< (fm x) acc) (fm x) acc)) (fm (car l)) (cdr l))
         )
       fm-row)
      )
    fm)
   )
  )

(define 1+ (lambda (x) (+ x 1)))
(define square (lambda (x) (* x x)))

(maxmin (list (list square exp) (list cos 1+)) '(-1 0 1))