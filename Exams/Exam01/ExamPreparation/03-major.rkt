#lang racket

(define (sublist? l1 l2)
  (cond
    [(null? l1) #t]
    [(not (equal? (car l1) (car l2))) #f]
    [else (sublist? (cdr l1) (cdr l2))]
    )
  )

(define (major-list? l1 l2)
  (cond
    [(null? l1) #t]
    [(null? l2) #f]
    [else (and (<= (car l1) (car l2)) (major-list? (cdr l1) (cdr l2)))]
    )
  )

(define (get-sublists l)
  (if (null? l)
      (list)
      (append (list l) (get-sublists (drop l 1)))
      )
  )

(define (any? f l lst)
  (foldr (lambda (x xs) (or (f l x) xs)) #f lst)
  )

(define (is-major? l)
  (if (null? (cdr l))
      #t
      (and
       (any? major-list? (car l) (get-sublists (cadr l)))
       (is-major? (cdr l))
       )
      )
  )

(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12))) ;; => #t
(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12))) ;; => #f