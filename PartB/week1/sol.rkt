#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)


;; 1.
(define (sequence low high stride)
  (if (<= low high) (cons low (sequence (+ low stride) high stride)) null))

;; 2.
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;; 3.
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list") (car (list-tail xs (remainder n (length xs)))))))

;; 4.
(define (stream-for-n-steps s n)
   (if (> n 0) (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1))) null))

;; 5
(define (funny-number-stream)
  (define (f x) (cons (if (= 0 (remainder x 5)) (- x) x)
                      (lambda () (f (+ x 1)))))
  (f 1))

;; 6
(define (dan-then-dog)
(define (f1 x) (cons x (lambda () (f1 (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))
  (f1 "dan.jpg"))

;; 7
(define (stream-add-zero s)
  (define (f x) (cons (cons 0 (car (x)))
                 (lambda () (f (cdr (x))))))
  (lambda () (f s)))

;; 8
(define (cycle-lists xs ys)
  (letrec ([loop (lambda (n)
                   (cons (cons (list-nth-mod xs n)
                               (list-nth-mod ys n))
                         (lambda () (loop (+ n 1)))))])
    (lambda () (loop 0))))

;; 9
(define (vector-assoc v vec)
 (define len (vector-length vec))

 (define (helper v vec i)
    (cond [(= i len) #f]
        [else (if (and (pair? (vector-ref vec i)) (equal? (car (vector-ref vec i)) v)) (vector-ref vec i) (helper v vec (+ i 1)))]))
 (helper v vec 0))

;; 10
(define (cached-assoc lst n)
  (let ([cache (make-vector n #f)]
        [next-to-replace 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([ans (assoc v lst)])
            (and ans
                 (begin (vector-set! cache next-to-replace ans)
                        (set! next-to-replace 
                              (if (= (+ next-to-replace 1) n)
                                  0
                                  (+ next-to-replace 1)))
                        ans)))))))
