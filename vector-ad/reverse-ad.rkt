#lang racket
(require racket/control syntax/parse/define (for-syntax racket))
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


;; Vector-Func :: R^n -> R^m = (Listof f_1 ... f_m) where f_x :: (Vectorof x_1 ... x_n) -> R
(struct Vector-Func (flst))

(define-simple-macro (define-vector-function (f-name:id x:id ...) results:expr ...)
  #:do [(define (detect-used-variables var-list result-expr ctx)
          (define used-list '())
          (define (add-to-used-list! v index)
            (set! used-list `((,(datum->syntax ctx (syntax-e v)) ,(datum->syntax ctx index)) . ,used-list)))
          (with-syntax ([((var proc) ...) (map (λ (v i) (list v (datum->syntax #f (λ () (add-to-used-list! v i))))) var-list (range 0 (length var-list)))]
                        [result-expr result-expr])
            (local-expand #`(let-syntax ([var (λ (stx)
                                                ('proc)
                                                #'(void))] ...)
                              result-expr) 'expression '()))
          used-list)]
  #:with (([used-vars index] ...) ...) (map (λ (r) (detect-used-variables (syntax->list #'(x ...)) r #'f-name)) (syntax->list #'(results ...)))       
  (define f-name (Vector-Func (list (λ (x-vec)
                       (let ([used-vars (vector-ref x-vec index)] ...)
                         results)) ...))))

(define-vector-function (f x y)
  (:+ x y) x y)


(define (call-with-derivative* f vec)
  (match-define (Vector-Func flst) f)
  (define (partial f)
    (define derivative-vec (for/vector ([v (in-vector vec)])
                             (derivative v (box 0))))
    (define result #f)
    (reset-at ad-tag (match (f derivative-vec)
                     [(derivative r rd-box)
                      (set-box! rd-box 1) (set! result r)]))
    (for/vector ([d (in-vector derivative-vec)])
      (match d
        [(derivative _ derbox) (derivative result (unbox derbox))])))
  (for/vector ([f (in-list flst)])
    (partial f)))


(call-with-derivative* f (vector 1 1))

(define-vector-function (g x y)
  (:* (:sin x) y) (:* (:sin y) x))

(call-with-derivative* g (vector 2 2))