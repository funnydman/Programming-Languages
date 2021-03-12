#lang racket

(define x 3)

(define y (+ x 2))

(define cube1
 (lambda (x)
  (* x (* x x ))))

(define cube2
 (lambda (x)
  (* x x x)))

(define (cube3 x)
 (* x x x))


(define (pow1 x y)
 (if (= y 0) 1 (* x (pow1 x (- y 1)))))


(define pow2
 (lambda (x)
  (lambda (y)
   (pow1 x y))))

(define three-to-the (pow2 3))


#| Racket Lists |#

(define (sum xs)
 (if (null? xs) 0 (+ (car xs) (sum (cdr xs)))))

(define (prod xs)
 (if (null? xs) 1 (* (car xs) (prod (cdr)))))

(define (my-append xs ys)
    (if (null? xs) ys (cons (car xs) (my-append (cdr xs) ys))))

(define concat (my-append (list 1 2 3) (list 4 5 6)))


(define (my-map f xs)
 (if (null? xs) null (cons (f (car xs)) (my-map f (cdr xs)))))


(define foo (my-map (lambda (x) (+ x 1))  (cons 3 (cons 4 (cons 5 null)))))


(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))


(define (sum1 xs)
 (if (null? xs) 0 (if (number? (car xs))
                   (+ (car xs) (sum1 (cdr xs)))
                   (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
 (if (null? xs)
  0
  (if (number? (car xs))
   (+ (sum1 (car xs)) (sum1 (cdr xs)))
   (if (list? (car xs))
    (+ (sum2 (car xs)) (sum2 (cdr xs)))
    (sum2 (cdr xs))))))


#| cond |#

(define (sum3 xs)
 (cond [(null? xs) 0]
  [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
  [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))


(define (sum4 xs)
 (cond [(null? xs) 0]
  [(number? (car xs))  (+ (car xs) (sum4 (cdr xs)))]
  [(list? xs) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
  [#t (sum4 (cdr xs))]))


(define (count-falses xs)
 (cond [(null? xs) 0]
  [(car xs) (count-falses (cdr xs))]
  [#t (+ 1 (count-falses (cdr xs)))]))


#| local bidings |#

(define (max-of-list xs)
 (cond [(null? xs) (error "max of list given empty list")]
  [(null? (cdr xs)) (car x)]
  [#t (let ([tlans (max-of-list (cdr xs))])
      (if (> tlans (car xs))
       tlans
       (car xs)))]))

(define (silly-double x)
 (let ([x (+ x 3)]
       [y (+ x 2)])
  (+ x y -5)))

#| works like in ML |#
(define (silly-double2 x)
 (let* ([x (+ x 3)]
        [y (+ x 2)])
  (+ x y -8)))

(define (silly-triple x)
 (letrec ([y (+ x 2)]
          [f (lambda(z) (+ z y w x))]
          [w (+ x 7)])
  (f -9)))


#| Mutation with set! |#

(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4))
(set! b 5)
(define z (f 4))
(define w c)

#| 1, (true, "hi" |#
(define pair (cons 1 (cons #t "hi")))
(define alist (cons 1 (cons #t (cons "hi" null))))

#| mcons For Mutable Pairs |#
(define mpr (mcons 10 (mcons 3 (mcons "yay" "test"))))


#| Streams are nothing but lazy lists. |#

(define ones (lambda () (cons 1 ones)))
(define (func x) (cons (lambda () (func (+ x 1)))))
#| (define nats (lambda () (f 1))) |#

#| (define powers-of-two |#
#|  (letrec ([func (lambda (x) (cons x (lambda () (func (* x 2)))))]) |#
#|   (lambda (func 1)))) |#
(define (fibonacci1 x)
 (if (or (= x 1) (= x 2))
  1
  (+ (fibonacci1 (- x 1))
   (fibonacci1 (- x 2)))))
