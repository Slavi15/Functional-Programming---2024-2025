#lang racket

(define // quotient)
(define % modulo)

(define (concat n digit)
  (+ (* n 10) digit)
)

(define (reverse n)
  (define (iter acc n)
    (if (zero? n)
        acc
        (iter (concat acc (% n 10)) (// n 10)))
  )
  (iter 0 n)
)

(define (palindrome? n)
  (= n (reverse n))
)

(define (palindromes-between a b)
  (if (> a b)
      0
      (let loop ([i a]
                 [acc 0])
        (if (= i (+ b 1))
            acc
            (loop (+ i 1) (+ acc (if (palindrome? i) 1 0))))
        )
      )
)

(= (palindromes-between 1 101) 19)
(= (palindromes-between 1 100) 18)
(= (palindromes-between 100 1) 18)