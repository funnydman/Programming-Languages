#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)


;; 1.
(define (sequence low high stride)
  (if (<= low high) (cons low (sequence (+ low stride) high stride)) '()))

;; 2.
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;; 3.
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list") (car (list-tail xs (remainder n (length xs)))))))

;; 4.
(define test-stream (stream 1 2 3))
(define (stream-for-n-steps s n) (
   (if (> n 0) (stream-car s(stream-for-n-steps s n-1)
