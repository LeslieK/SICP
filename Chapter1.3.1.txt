#lang planet neil/sicp
;#lang racket

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
 
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
  
(define (divides? a b)
  (= (remainder b a) 0))
  
(define (prime? n)
  (= (smallest-divisor n) n))
 
 ;; gcd
(define (gcd a b)
   (if (= b 0)
       a
       (gcd b (remainder a b))))
 
;; 1.3.1 Procedures as Arguments
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

;(sum-integers 2 5)

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;(pi-sum 1 15)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (sum-cubes-2 a b)
  (sum cube a inc b))

;(sum-cubes-2 1 10)

(define (identity x) x)

(define (sum-integers-2 a b)
  (sum identity a inc b))

;(sum-integers-2 1 10)

(define (pi-next a)
  (+ a 4))

(define (pi-term a)
  (/ 1 (* a (+ a 2))))

(define (pi-sum-2 a b)
  (sum pi-term a pi-next b))

;(* 8 (pi-sum-2 1 1000)) ; pi-sum converges to pi/8

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

;(integral cube 0 1 0.01) => 0.2499875...
;(integral cube 0 1 0.001) => 0.249999875...

;; ex1.29 Simpson's Rule
(define (simpsons-rule2 f a b n)
  (define h (/ (- b a) n))
  (define (add-1h x) (+ x h))
  (define (add-2h x) (+ x h h))
  (define (coeff-f coeff x) (* coeff (f x)))
  (* 
  (+ (f a)
     (sum (coeff-f 4) (add-1h a) add-2h (- b 2))
     (sum (coeff-f 2) (add-2h a) add-2h (- b 1))
     (f b))
  (/ h 3)))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-term k)
    (define y-k (f (+ a (* k h))))
    (define coeff
      (cond ((or (= k 0) (= k n)) 1)
            ((even? k) 2)
            (else 4)))
    (* coeff y-k))
  (* (sum simpsons-term 0 inc n)
     (/ h 3)))
                        
;(simpsons-rule cube 0 1 100) ; 1/4
;(simpsons-rule cube 0 1 1000) ; 1/4
  
;; ex1-30
;; change sum procedure from a recursive process to an iterative process
(define (sum_i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum_integers-2 a b)
  (sum_i identity a inc b))

;(sum_integers-2 1 100)

; 1.31(a)
;; write an analagous procedure called product that returns the product of the values of a function at points over a given range
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; defined iteratively
(define (product-integers-2 a b)
  (product-it identity a inc b))

;; defined recursively
(define (product-integers a b)
  (product identity a inc b))

;(product-integers-2 1 5)
;(product-integers 1 5)

(define (factorial n)
  (product-integers-2 1 n))
 
;(factorial 10)
  
;; ex1.32
;; show that sum and product are special cases of accumulate

; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;(accumulate + 0 identity 1 inc 10)
;(accumulate * 1 identity 1 inc 5)

; iterative
(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;(accumulate-i + 0 identity 1 inc 10)
;(accumulate-i * 1 identity 1 inc 5)

;; ex1.33
;; combine only terms in range that are true wrt a predicate
 (define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (predicate a)
                           (combiner (term a) result)
                           result))))
  (iter a null-value))

 ;(filtered-accumulate + 0 identity 1 inc 10 even?)
 
 ; sum of square of the prime numbers in interval a b
 (define (sum-square-primes a b)
   (filtered-accumulate + 0 square a inc b prime?))
   
; (sum-square-primes 1 20)
 
 (define (prod-rel-primes a b)
   (define (relatively-prime? x)
       (= (gcd x b) 1))
   (filtered-accumulate * 1 identity a inc b relatively-prime?))
 
; (prod-rel-primes 1 10)

 ;; ex1-34
(define (f g)
  (g 2))
(f square)
(f cube)
;(f f) => (f 2) => (2 2)

;; 1.35





      