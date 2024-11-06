#lang racket

(define // quotient)
(define % modulo)

(define (divides n k) (zero? (% n k)))

(define (sum-divisors n) (foldr (lambda (x acc) (if (divides n x) (+ x acc) acc)) 0 (range 1 n)))

(define (done? n) (= (+ n 2) (sum-divisors n)))

(done? 20) ;; => #t
(done? 28) ;; => #f

(define (any? f l) (foldr (lambda (x xs) (or (f x) xs)) #f l))

(define (sum-almost-done a b)
  (let ([done-list (filter done? (range a b))])
    (foldr + 0
           (filter
            (lambda (x)
              (any? (lambda (curr-done) (< (abs (- x curr-done)) (min (- x a) (- b x)))) done-list)
              )
            (range a (+ 1 b)))
           )
    )
  )

(sum-almost-done 5 24) ;; => 153 сумата на числата от 13 до 21