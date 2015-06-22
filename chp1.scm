#lang planet neil/sicp

;(define (square x) (* x x))

;(define (sum-of-squares x y)
;  (+ (square x) (square y)))

(define (abs x)
  (cond ((>= x 0) x)
        ((< x 0) (- x))))

;(define (abs x)
;  (if (< x 0)
;      (- x)
;      x))

(define (bad-cond-returns-undefined x)
  (cond (#f 2)
        (#f 3)))



(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;(define (sum-square-larger-pair x y z)
;  (if (= x (if (> x y) x y))
;      (sum-of-squares x (if (> y z) y z))
;      (sum-of-squares y (if (> x z) x z))))

; sum of square of the larger two numbers
(define (larger x y)
    (if (> x y) x y))

(define (sum-square-larger-pair x y z)
  (if (= x (larger x y))
      (sum-of-squares x (larger y z))
      (sum-of-squares y (larger x z))))


; NOTE: 
; - recursive call to sqrt-iter with improved guess
; - we can write any purely numerical program using just procedure calls (without any iterative looping constructs!)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

; NOTE: good-enough? is not effective for finding square roots of very small or very large numbers because of limited precision arithmetic on most computers. A better way might be to watch how guess changes through each iteration as a proportion of the itself. (delta guess / guess)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (improve guess x)
  (avg guess (/ x guess))) 

(define (avg x y)
  (/ (+ x y) 2))

; NOTE: mixed operations on decimals and rationals yield decimals. 
(define (sqrt x)
    (sqrt-iter 1.0 x))       


(define (factorial-1 n)
  (if (= n 1)
      1
      (* n (factorial-1 (- n 1)))))

(define (factorial-2 n)
  (define (fact-iter count product)
    (if (> count n)
        product
        (fact-iter (+ count 1) (* product count))))
  (fact-iter 1 1))

; 1.2.2
(define (fib n)
  (cond ((= n 1) 1)
        ((= n 0) 0)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; iterative fib
(define (fib-2 n)
  (define (fib-iter a b count)
    (if (= count n)
        b
        (fib-iter b (+ a b) (+ count 1))))
  (fib-iter 0 1 1))

(define (fib-3 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


; Exercise 1.11
; recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; iterative 
(define (f-2 n)
  (define (f-iter a b c count)
    (if (= count 2)
        a
        (f-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (dec count))))
  (f-iter 2 1 0 n))











        

         





