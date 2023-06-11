#lang racket
(require racket/control syntax/parse/define)
(define ad-tag (make-continuation-prompt-tag 'ad-tag))

(struct derivative (v d) #:transparent)

(define (:constant x)
  (derivative x (box 0)))

(define-simple-macro (define-derivative-op (op:id x:id ...)
                       #:result r-exp:expr #:propagate-factor p-exp:expr ...)
  #:with (dx-box ...) (generate-temporaries #'(x ...))
  (define (op x ...)
    (match* (x ...)
      [((derivative x dx-box) ...)
       (shift-at ad-tag k
                 (define rbox (box 0))
                 (k (derivative r-exp rbox))
                   (define r (unbox rbox))
                   (set-box! dx-box (+ (unbox dx-box) (* p-exp r))) ...)])))

(define-derivative-op (:+ x y)
  #:result (+ x y)
  #:propagate-factor 1 1)

(define-derivative-op (:- x y)
  #:result (- x y)
  #:propagate-factor 1 -1)

(define-derivative-op (:* x y)
  #:result (* x y)
  #:propagate-factor y x)

(define-derivative-op (:/ x y)
  #:result (/ x y)
  #:propagate-factor (/ 1 y) (- (/ (* x x) (* y y))))

(define-derivative-op (:sin x)
  #:result (sin x)
  #:propagate-factor (cos x))

(define-derivative-op (:cos x)
  #:result (cos x)
  #:propagate-factor (- (sin x)))

(define (call-with-derivative f x)
  (define xbox (box 0))
  (define result #f)
  (reset-at ad-tag (match (f (derivative x xbox))
                     [(derivative r rd-box)
                      (set-box! rd-box 1) (set! result r)]))
  (derivative result (unbox xbox)))
         

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


