#lang racket

(define (unique l)
  (foldr
   (lambda (x xs) (cond [(or (null? xs) (not (eqv? x (car xs)))) (cons x xs)]
                        [else xs]))
   `()
   l
   )
  )

(define (set-union l1 l2)
  (define (merge ls1 ls2 lst)
    (let ([first (car ls1)]
          [second (car ls2)])
      (if (<= first second)
          (iter (cdr ls1) ls2 (cons first lst))
          (iter ls1 (cdr ls2) (cons second lst))
          )
      )
    )

  (define (first-merge ls1 lst)
    (if (null? ls1)
        (unique (reverse lst))
        (let ([elem (car ls1)])
          (first-merge (cdr ls1) (cons elem lst))
          )
        )
    )

  (define (second-merge ls2 lst)
    (if (null? ls2)
        (unique (reverse lst))
        (let ([elem (car ls2)])
          (second-merge (cdr ls2) (cons elem lst))
          )
        )
    )
  
  (define (iter ls1 ls2 lst)
    (cond [(null? ls1) (second-merge ls2 lst)]
          [(null? ls2) (first-merge ls1 lst)]
          [else (merge ls1 ls2 lst)])
    )
  
  (iter l1 l2 `())
  )

(set-union '(1 3 5 7) '(5 7 13))
(set-union '(5 7 13) '(1 3 5 7))

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))