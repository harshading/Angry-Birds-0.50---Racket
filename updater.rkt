#lang racket
(require "collision-detector.rkt")
(require "classes.rkt")
(require "collisions.rkt")
(require "vectors.rkt")

(provide updater)

(define (updater l)
    (cond[#t (collision (check-for-collision l))
             ;(displayln l)
             (map (lambda (x) (helper2 x)) l)]))

(define (helper2 x)
  (cond [(equal? (get-field name x) "rectangle") (let* ([u1 (get-field u x)]
                                                        [w1 (get-field w x)]
                                                        [or1 (get-field vec1 x)]
                                                        [pos1 (get-field pos x)])
                                                   (set-field! u x (vectorsum u1 (map (lambda (x1) (* x1 tick)) g)))
                                                   (set-field! pos x (vectorsum pos1 (map (lambda (x1) (* 0.5 x1 tick)) (vectorsum (get-field u x) u1))))
                                                   (set-field! vec1 x (vectorsum or1 (map (lambda (x1) (* x1 tick)) (crossprod w1 or1 ))))
                                                   ;(displayln (get-field pos x))
                                                   )]
        [(equal? (get-field name x) "ground") (let* ([u1 (get-field u x)]
                                                        [w1 (get-field w x)]
                                                        [or1 (get-field vec1 x)]
                                                        [pos1 (get-field pos x)])
                                                   ;(set-field! u x (vectorsum u1 (map (lambda (x1) (* x1 tick)) g)))
                                                   ;(set-field! pos x (vectorsum pos1 (map (lambda (x1) (* 0.5 x1 tick)) (vectorsum (get-field u x) u1))))
                                                   ;(set-field! vec1 x (vectorsum or1 (map (lambda (x1) (* x1 tick)) (crossprod w1 or1 ))))
                                                   ;(displayln (get-field pos x))
                                                void
                                                   )]
        [#t (let* ([u1 (get-field u x)]
                   [pos1 (get-field pos x)])
              (set-field! u x (vectorsum u1 (map (lambda (x1) (* x1 tick)) g)))
              (set-field! pos x (vectorsum pos1 (map (lambda (x1) (* 0.5 x1 tick)) (vectorsum (get-field u x) u1))))
              ;(displayln (get-field pos x))
              )]))