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



;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
;     (* 3 (- 6 2) (- 2 7)))

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
    (cond ((< n 3) n)
          ((= count 2) a)
          (else (f-iter (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (dec count)))))
  (f-iter 2 1 0 n))

; 1.12 Pascal's Triangle
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;
; indices are 1-based
(define (pascal-element row col)
  (cond ((and (= row 1) (= col 1)) 1)
        ((or (< col 1) (> col row)) 0)
        (else (+ (pascal-element (dec row) (dec col))
                 (pascal-element (dec row) col)))))

; should bind col to 0
(define (pascal-iter-row row col)
  (cond ((< row col) 0) ; todo
        ((= row col) (display (pascal-element row col)))
        (else (display (pascal-element row col))
              (display #\space) ; todo
              (pascal-iter-row row (inc col)))))

(define (pascal-triangle depth)
  (define (pascal-iter-triangle current-depth)
    (cond ((= current-depth depth) (pascal-iter-row current-depth 1))
          (else (pascal-iter-row current-depth 1)
                (newline)
                (pascal-iter-triangle (inc current-depth)))))
  (pascal-iter-triangle 0))


; Exercise 1.14
; REVIEW

; Exercise 1.15
; a. 5 times, since 12.15/3^5 < 0.1
; b. steps -> 0(log(a))
;    space -> 0(log(a))

; 1.2.4 Exponentiation
; 0(n) steps
; 0(1) space
(define (expt b n)
  (define (expt-iter count product)
    (if (= count 0)
        product
        (expt-iter (dec count) (* b product))))
  (expt-iter n 1))

(define (even? n)
  (= 0 (remainder n 2)))
   
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Exercise 1.19
; a'  = bq + aq + ap
; b'  = bp + aq
; a'' = b'q + a'q + a'p
; b'' = b'p + a'q
;
; a'' = (bp + aq)q +
;       (bq + aq + ap)q +
;       (bq + aq + ap)p
;     = bpq + aq2 +
;       bq2 + aq2 + apq +
;       bqp + aqp + ap2
;     = b(2pq + q2) + 
;       a(2pq + q2) +
;       a(p2  + q2)
;       (which is of the required form)
;
; b'' = (bp + aq)p + 
;       (ba + aq + ap)q
;     = bp2 + aqp +
;       bq2 + aq2 + apq
;     = b(p2 + q2) +
;       a(2pq + q2)
;       (which is of the required form)
; 
; so,    p' = p2 + q2
; and    q' = 2pq + q2
;
; Computing Fibonacci numbers in log(n) steps
; using successive squaring of Transformation Tpq
; read pg 47
(define (fib-log n)
  (fib-log-iter 1 0 0 1 n))

(define (fib-log-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-log-iter a
                       b
                       (+ (* p p) (* q q))
                       (+ (* 2 p q) (* q q))
                       (/ count 2)))
        (else (fib-log-iter (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))

; Euclid's Algorithm for GCD - oldest 'algorithm'
; 0(log(n))
(define (gcd a b)
    (if (= b 0) 
        a
        (gcd b (remainder a b))))

; Exercise 1.20 pg 49
; normal order: certainly more than 4
;(gcd 206 40)
;(if (= 40 0) )
;(gcd 40 (reminder 206 40))
;(if (= (remainder 206 50) 0) )
;(if (= 6 0))
;(gcd (remainder 206 40) (reaminder 40 (remainder 206 40))
;.
;.
;.

; applicative order: 4 calls to remainder
;(gcd 206 40)
;(gcd 40 (remainder 206 40)) 
;(gcd 40 6) 
;(gcd 6 (remainder 40 6)) 
;(gcd 6 4) 
;(gcd 4 (remainder 6 4)) 
;(gcd 4 2) 
;(gcd 2 (remainder 4 2)) 
;(gcd 2 0) 


;;;SECTION 1.2.6

;; prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  ; NOTES:
  ; if n is not prime it must have a divisor <= sqrt(n)
  ; because if d is a divisor of n, then so is n/d.
  ; but d and n/d cannot both be greater than n
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


; The Fermat test (probabilistic primality algorithm)
; compute exponential of a number modulo another
; uses successive squaring idea used in fast-expt in 
; section 1.2.4 so num steps are order log(n)
;     
(define (expmod base exp m)
  (cond ((= exp 0) 1) ; anything to the power 0 is 1
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


; Exercise 1.22 pg 54

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((> start end) (newline) 
                       (display "search complete"))
        ((odd? start)(timed-prime-test start)
                     (search-for-primes (+ start 2) end))
        (else (search-for-primes (+ start 1) end))))

;(search-for-primes 1000 1020)
;1009 *** 7
;1013 *** 7
;1019 *** 6
;
;(search-for-primes 10000 10040)
;10007 *** 15
;10009 *** 14
;10037 *** 13
;
;(search-for-primes 100000 100050)
;100003 *** 37
;100019 *** 35
;100043 *** 35
;
;(search-for-primes 1000000 1000040)
;1000003 *** 106
;1000033 *** 97
;1000037 *** 97
;
; As n grows larger the steps correspond closer to the
; 0(sqrt(n)) order of growth, 35 * sqrt(10) -= 97

; Exercise 1.24 pg 55

; redefine timed prime test to use fast-prime? procedure


(define (search-for-primes-fast start end)
  (define (timed-fast-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))

  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  
  (cond ((> start end) (newline) 
                       (display "search complete"))
        ((odd? start)(timed-fast-prime-test start)
                     (search-for-primes-fast (+ start 2) end))
        (else (search-for-primes-fast (+ start 1) end))))

; These take twice as long if run independantly rather
; than within the search-for-primes
;(timed-prime-test 1009)
;(timed-fast-prime-test 1009)

;(search-for-primes-fast 1000 1020)
;1009 *** 7
;1013 *** 7
;1019 *** 6
;
;
;(search-for-primes-fast 10000 10040)
;10007 *** 14
;10009 *** 14
;10037 *** 14
;
;(search-for-primes-fast 100000 100050)
;100003 *** 33
;100019 *** 33
;100043 *** 33
;
;(search-for-primes-fast 1000000 1000040)
;1000003 *** 97
;1000033 *** 97
;1000037 *** 97
;
; Perhaps we have to use much larger numbers to see a difference
; in computation time between prime? and fast-prime?

; 1.3.1 Procedures as Arguments pg 57
; term : procedure that computes a term to be added
; next : procedure that computes the next value of a
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) 
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (approx-pi)
  (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;(integral cube 0 1 0.01)
; 0.24998750000000042
;(integral cube 0 1 0.001)
; 0.249999875000001
; actual value of integral between 0 and 1 is 1/4


; Exercise 1.30 pg 60

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Exercise 1.31 pg 60

; generates linear recursive process
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; generates linear iterative process
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial-product n)
  (product-iter identity 1 inc n))

(define (approx-pi-product)
  (define (pi-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* 4.0 (product-iter pi-term 1 inc 1000)))


; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-accumulate term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate-iter * 1 term a next b))

; Exercise 1.35 pg 70
; 
; 1 + 1/phi
; = (phi + 1) / phi
; = phi^2 / phi [since phi^2 = phi + 1 (page 38)]
; = phi
;
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
  
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))




          





         

  
  
    







                


                       
           
  
                           
                            
    
  








        

         





