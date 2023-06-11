#lang racket
(struct derivative (v d) #:transparent)

(define (:+ a b)
  (match* (a b)
    [((derivative x dx) (derivative y dy))
     (derivative (+ x y) (+ dx dy))]))

(define (:- a b)
  (match* (a b)
    [((derivative x dx) (derivative y dy))
     (derivative (- x y) (- dx dy))]))

(define (:* a b)
  (match* (a b)
    [((derivative x dx) (derivative y dy))
     (derivative (* x y) (+ (* dx y) (* dy x)))]))

(define (:/ a b)
  (match* (a b)
    [((derivative x dx) (derivative y dy))
     (derivative (/ x y) (/ (- (* dx y) (* dy x)) (* y y)))]))

(define (:constant x)
  (derivative x 0))

(define (:sin a)
  (match a
    [(derivative x dx)
     (derivative (sin x) (* (cos x) dx))]))

(define (:cos a)
  (match a
    [(derivative x dx)
     (derivative (cos x) (* (- (sin x)) dx))]))


;;

(define (call-with-derivative f x)
  (f (derivative x 1)))

(define (f x)
  (:+ (:* x (:* x x)) x))

(call-with-derivative f 1)
(call-with-derivative f 4)

(define (g x)
  (:sin (:/ (:constant 1) x)))

(call-with-derivative g 0.2)
(call-with-derivative g 0.1)

(define (h x)
  (:- (:sin x) (:cos x)))

(call-with-derivative h 1)


