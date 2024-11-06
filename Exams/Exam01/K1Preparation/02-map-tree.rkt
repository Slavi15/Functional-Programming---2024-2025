#lang racket

(define (make-tree root left right) (list root left right))
(define mk make-tree)

(define (make-leaf root) (list root `() `()))

(define (root tr) (car tr))
(define (left-tree tr) (cadr tr))
(define (right-tree tr) (caddr tr))

(define (leaf? tr) (and (empty? (left-tree tr)) (empty? (right-tree tr))))

(define (empty? tr) (null? tr))

(define tree
  (mk
   (lambda (x) (+ 1 x))
   (mk
    (lambda (x) (expt x 2))
    (make-leaf (lambda (x) (* x 2)))
    (make-leaf (lambda (x) (- x 3)))
    )
   (make-leaf (lambda (x) (expt 3 x)))
   )
  )

(define (map-tree t n)
  (if (leaf? t)
      (list ((root t) n))
      (append (map-tree (left-tree t) ((root t) n)) (map-tree (right-tree t) ((root t) n)))
      )
  )

(map-tree tree 2)
(map-tree tree 1)