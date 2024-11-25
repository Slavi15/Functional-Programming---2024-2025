#lang racket

(define (assoc-rec key lst)
  (if (null? lst)
      #f
        (let ([key-list (caar lst)]
              [value-list (cdar lst)])
          (if (= key key-list)
              value-list
              (assoc-rec key (cdr lst))
              )
          )
        )
  )

(define (assoc-hop key lst)
  (define filtered (filter (lambda (pair) (equal? key (car pair))) lst))
  (if (null? filtered)
      #f
      (cdar filtered)
      )
  )

(define (assoc-assoc key lst)
  (define maybe-pair (assoc key lst))
  (if (pair? maybe-pair) (cdr maybe-pair) #f)
  )

(assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One")))
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

(assoc-rec 4 (list '(2 . "Two") '(3 . "Three") '(1 . "One")))
(equal? (assoc-rec 4 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) #f)

(newline)

(equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")
(equal? (assoc-hop 4 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) #f)

(newline)

(equal? (assoc-assoc 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")
(equal? (assoc-assoc 4 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) #f)