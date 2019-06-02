#lang racket
(provide dotprod
         proj
         nullvector?
         unitvector
         vectordiff
         vectorsum
         crossprod
         dotprod
         vectorscale
         perp
         mod
         neg
         dist
         projection)
         
(define (dotprod v1 v2)
  (cond [(null? v1) 0]
        [#t (+ (* (car v1) (car v2)) (dotprod (cdr v1) (cdr v2)))]))
(define (proj v1 v2)
  (let* ([uv2 (unitvector v2)]
         [x (dotprod v1 uv2)]) (map (lambda (y) (* x y)) uv2)))
(define (nullvector? v)
  (cond [(null? v) #t]
        [(= (car v) 0) (nullvector? (cdr v))]
        [#t #f]))
(define (unitvector v)
  (let ([l (sqrt (dotprod v v))])
    (cond [(= l 0) v]
          [#t (map (lambda (x) (/ x l)) v)])))
(define (vectordiff v1 v2)
  (map (lambda (x y) (- x y)) v1 v2))
(define (vectorsum v1 v2)
  (map (lambda (x y) (+ x y)) v1 v2))
(define (crossprod v1 v2)
  (define (element l i) (if (= i 1) (car l) (element (cdr l) (- i 1))))
  (let* ([a1 (element v1 1)]
         [a2 (element v1 2)]
         [a3 (element v1 3)]
         [b1 (element v2 1)]
         [b2 (element v2 2)]
         [b3 (element v2 3)]) (list (- (* a2 b3) (* a3 b2)) (- (* a3 b1) (* a1 b3)) (- (* a1 b2) (* a2 b1)))))
(define (vectorscale v r)
  (if (null? v) '()
      (cons (* (car v) r) (vectorscale (cdr v) r))))
(define (perp v)
  (if (= 0 (caddr v)) (crossprod (list 0 0 1) v)
      (begin
        ;(displayln v)
        (error "not defined"))))
(define (mod v)
  (sqrt (dotprod v v)))
(define (neg v)
  (if (null? v) '()
      (cons (- 0 (car v)) (neg (cdr v)))))
(define (dist r1 r2)
  (mod (vectordiff r1 r2)))
(define (projection v1 v2)
  (abs (/ (dotprod v1 v2) (mod v2))))
