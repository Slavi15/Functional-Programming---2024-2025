#lang racket

(define // quotient)
(define % modulo)

(define (square x) (* x x))

(define (automorphic? n)
  (= n (% (square n) 10))
)