#lang planet neil/sicp

;;(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;; Exercise 2.1
;; The version of gcd I'm using produces a divisor that
;; is the sign of n/d so the naive make-rat produces the
;; signed rational number we desire.
;; The following procedure should do the same irrespective
;; of the sign of the divisor produced by any gcd procedure:
(define (make-rat-signed n d) 
  (let ((g (gcd n d)))
    (let ((numer (/ n g))
          (denom (/ d g)))
      (let ((sign (if (< d 0) - +)))
        (cons (sign numer) (sign denom))))))

;; Exercise 2.2 pg 89

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((x-start (x-point (start-segment s)))
        (y-start (y-point (start-segment s)))
        (x-end (x-point (end-segment s)))
        (y-end (y-point (end-segment s))))
    (make-point (average x-start x-end) (average y-start y-end))))

;; (midpoint-segment (make-segment (make-point 10 10)
;;                                 (make-point 0 0)))

;; Exercise 2.3 pg 90
(define (make-rect left-segment bottom-segment)
  (cons left-segment bottom-segment))

(define (left-segment r)
  (car r))

(define (bottom-segment r)
  (cdr r))

(define (length s)
  (define (square x) (* x x))
  (let ((x-start (x-point (start-segment s)))
        (y-start (y-point (start-segment s)))
        (x-end (x-point (end-segment s)))
        (y-end (y-point (end-segment s))))
    (sqrt (+ (square (- y-end y-start))
             (square (- x-end x-start))))))

(define (area r)
  (* (length (left-segment r))
     (length (bottom-segment r))))

(define (perimeter r)
  (+ (* 2 (length (left-segment r)))
     (* 2 (length (bottom-segment r)))))

;; We could just as easily have used right-segment and top-segment
;; to represent a rectangle and we would have to change just
;; the area and perimeter procedures. However, if we represented rectangles with
;; two points for example, we would need to change the area and perimeter procedures.

;; (area (make-rect (make-segment (make-point 0 0)
;;                                (make-point 0 5))
;;                  (make-segment (make-point 0 0)
;;                                (make-point 10 0))))

;; (perimeter (make-rect (make-segment (make-point 0 0)
;;                                     (make-point 0 5))
;;                       (make-segment (make-point 0 0)
;;                                     (make-point 10 0))))


;; Exercise 2.4 pg 92
;; see Section 1.1.5 page 14 for Substitution Model
;;
;; REVIEW
;; commented out
;;(define (cons x y)
;;  (lambda (m) (m x y)))

;;(define (car z)
;;  (z (lambda (p q) p)))

;;(define (cdr z)
;;  (z (lambda (p q) q)))

;; Verification using Substitution Model
;; (car (cons x y))

;; body of cons is
;; (lambda (m) (m x y))

;; replacing cons
;; (car (lambda (m) (m x y)))

;; body of car is
;; (z (lambda (p q) p))

;; replacing formal parameter z with body of cons
;; ((lambda (m) (m x y)) (lambda (p q) p))

;; body of first lambda
;; (m x y)

;; replacing formal paramter m body of first lambda with second lambda
;; ((lambda (p q) p) x y)

;; body of second lambda
;; p

;; replacing formal parameter p 
;; x


;; Exercise 2.5 pg 92

;; a and b are non-negative integers (nni)
(define (cons-nni a b)
  (define (cons-iter product a b)
    (cond ((= a b 0) product)
          ((> a 0) (cons-iter (* 2 product) (dec a) b))
          ((> b 0) (cons-iter (* 3 product) a (dec b)))
          (else (error "a and b must be non-negative integers"))))
  (cons-iter 1 a b))

;; number of times base divides number resulting in an integer quotient
(define (integer-log base number)
  (define (iter count quot)
    (if (= (remainder quot base) 0)
        (iter (inc count) (/ quot base))
        count))
  (iter 0 number))

(define (car-nni z)
  (integer-log 2 z))

(define (cdr-nni z)
  (integer-log 3 z))

;; (car-nni (cons-nni 3 4))
;; (cdr-nni (cons-nni 3 4))


;; Exercise 2.6 pg 93
;; Church Numerals

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))









