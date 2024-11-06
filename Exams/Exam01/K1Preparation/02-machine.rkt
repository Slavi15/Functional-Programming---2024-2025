#lang racket

(define (check x) (or (number? x) (symbol? x)))

(define (run-operation f n lst)
    (if (or (zero? n) (symbol? (car lst)) (symbol? (cadr lst)) (null? lst))
        lst
        (let ([first (car lst)]
              [second (cadr lst)])
          (run-operation f (- n 1) (cons (f first second) (cddr lst)))
          )
        )
  )

(define (run-machine l)
  (define (iter lst rules)
    (if (null? rules)
        lst
        (let ([rule (car rules)])
          (cond
            [(check rule) (iter (cons rule lst) (cdr rules))]
            [(pair? rule) (iter (run-operation (car rule) (cdr rule) lst) (cdr rules))]
            [else (iter (map (lambda (x) (if (number? x) (rule x) x)) lst) (cdr rules))])
          )
        )
    )
  (iter `() l)
  )

(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6))                       ;; => (6 5 4 3 a 2 x 1)
(run-machine (list 1 'x 4 'a 9 16 25 sqrt 6 (cons + 2) (cons * 5))) ;; => (45 a 2 x 1)