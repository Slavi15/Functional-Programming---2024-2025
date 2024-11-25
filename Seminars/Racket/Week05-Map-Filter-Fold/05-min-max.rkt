#lang racket

(define (min l) (foldr (lambda (x xs) (if (< x xs) x xs)) (car l) (cdr l)))

(define (max l) (foldr (lambda (x xs) (if (> x xs) x xs)) (car l) (cdr l)))

(min '(5 7 1 3)) ;; => 1
(max '(5 7 1 3)) ;; => 7

(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l) (foldr1 op (cdr l)))
      )
  )

(define (foldl1 op l)
  (if (null? (cdr l))
      (car l)
      (op (foldl1 op (cdr l)) (car l))
      )
  )

(define (minimum l) (foldr1 (lambda (x y) (if (< x y) x y)) l))

(define (maximum l) (foldr1 (lambda (x y) (if (> x y) x y)) l))

(minimum '(5 7 1 3)) ;; => 1
(maximum '(5 7 1 3)) ;; => 7