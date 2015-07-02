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



        





