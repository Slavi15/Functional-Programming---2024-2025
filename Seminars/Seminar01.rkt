;;; Task 01

(define (square x) (* x x))

(+ (/ (+ 3 5) 2) (sqrt (- (square 4) (* 7 (square 2)))))
(/ (+ 5 1/4 (- 2 (- 3 (+ 6 1/5)))) (* 3 (- 6 2) (- 2 7)))
(/ (+ 15 21 1/5 (- 7 (square 2))) 16)

;;; Task 01

;;; Task 02

(define (my-not x) (if x #f #t))

(define (my-and x y) (if (and x y) #t #f))
(define (my-and x y) (if x (if y #t #f) #f))

(define (my-or x y) (if (or x y) #t #f))
(define (my-or x y) (if x #t (if y #t #f)))

(define (my-xor x y) (if (my-and (my-or x y) (my-not (my-and x y))) #t #f))

;;; Task 02

;;; Task 03

(define (fact n)
  (cond ((= n 0) 1)
        (else (* n (fact (- n 1))))
        )
  )

;;; Task 03

;;; Task 04

(define (fibb n)
  (cond ((= n 0) 1)
        ((=  n 1) 1)
        (else (+ (fibb (- n 1)) (fibb (- n 2))))
  )
)

;;; Task 04

;;; Task 05

(define (count-digits n)
  (define (count-digits-wrapper n counter)
    (if (< n 10) (+ counter 1) (count-digits-wrapper (/ n 10) (+ counter 1)))
  )
  (count-digits-wrapper n 0)
)

;;; Task 05

;;; Task 06

(define (convert-to-base n base)
  (let loop ((num n) (result ""))
    (if (= num 0)
        result
        (loop (quotient num base) (string-append (number->string (modulo num base)) result)))
    ))

(define (string-reverse str)
  (define (reverse-helper s acc)
    (if (string=? s "")
        acc
        (reverse-helper (substring s 1) (string-append (string (string-ref s 0)) acc)))
  )
  (reverse-helper str "")
)

(define (palindrome? n base)
  (let* ((str (convert-to-base n base))
         (reversed (string-reverse str)))
    (string=? str reversed))
)

;;; Task 06
