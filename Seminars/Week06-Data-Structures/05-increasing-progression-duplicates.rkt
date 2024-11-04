#lang racket

(define (increasing? l)
  (define (iter prev lst)
    (if (null? lst)
        #t
        (and (<= prev (car lst)) (iter (car lst) (cdr lst)))
        )
    )
  (iter (car l) (cdr l))
  )

(increasing? `(1 2 3 3 4 5))

(newline)

(define (progression? l)
  (define (iter diff lst)
    (cond
      [(null? (cdr lst)) #t]
      [(not (= diff (- (cadr lst) (car lst)))) #f]
      [else (iter diff (cdr lst))])
    )
  (iter (- (cadr l) (car l)) l)
  )

(progression? `(1 3 5 7 9 11))

(newline)

(define (count-occurences el l) (foldr (lambda (x acc) (if (= el x) (+ 1 acc) acc)) 0 l))

(define (has-duplicates? l)
  (cond
    [(null? l) #f]
    [(> (count-occurences (car l) l) 1) #t]
    [else (has-duplicates? (cdr l))])
  )

(has-duplicates? `(1 2 3 3 4 5))