#lang racket


(require 2htdp/image)
(require 2htdp/universe)

(define (y x) (* x x))
(define (func rad) (circle rad "solid" "blue"))


(define (isAdult age) (
                     cond [(>= age 18) #t]
                          [(< age 18) #f]))
