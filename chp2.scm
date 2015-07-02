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












        





