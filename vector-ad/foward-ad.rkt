#lang racket
(require syntax/parse/define)
(require (for-syntax racket))
(struct derivative (v d) #:transparent)

(define (:constant x)
  (derivative x 0))

(define memoized? (make-parameter #f))

(define-simple-macro (define-derivative-op (op-name:id [x:id dx:id] ...) #:result r-expr:expr #:derivative d-expr:expr)
  (define op-name
    (let ([result-box (box #f)])
      (λ (x ...)
        (match* (x ...)
          [((derivative x dx) ...)
           (let ([need-memoize (not (memoized?))]
                 [calculate (λ ()
                              (define result r-expr)
                              (set-box! result-box result)
                              result)])
             (define result (if need-memoize (calculate) (unbox result-box)))
             (derivative result d-expr))])))))


(define-derivative-op (:+ [x dx] [y dy])
  #:result (+ x y)
  #:derivative (+ dx dy))

(define-derivative-op (:- [x dx] [y dy])
  #:result (- x y)
  #:derivative (- dx dy))

(define-derivative-op (:* [x dx] [y dy])
  #:result (* x y)
  #:derivative (+ (* dx y) (* dy x)))

(define-derivative-op (:/ [x dx] [y dy])
  #:result (/ x y)
  #:derivative (/ (- (* dx y) (* dy x)) (* y y)))

(define-derivative-op (:sin [x dx])
  #:result (sin x)
  #:derivative (* (cos x) dx))

(define-derivative-op (:cos [x dx])
  #:result (cos x)
  #:derivative (* (- (sin x)) dx))

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
    (define memoized #f)
    (for/vector ([i (vector-length vec)])
      ;; calculate ∂ f/∂ x_i
      (parameterize ([memoized? memoized])
      (define derivative-vec (for/vector ([(v j) (in-indexed vec)])
        (derivative v (if (= i j) 1 0))))
      (define result-derivative (f derivative-vec))
       (set! memoized #t)
        result-derivative)))
  (for/vector ([f (in-list flst)])
    (partial f)))
      
(call-with-derivative* f (vector 1 1))

(define-vector-function (g x y)
  (:* (:sin x) y) (:* (:sin y) x))

(call-with-derivative* g (vector 2 2))


