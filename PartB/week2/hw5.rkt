;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)    #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 


;; Problem 1

(define (racketlist->mupllist rlist)
  (cond [(empty? rlist) (aunit)]
        [(list? rlist) (apair (car rlist) (racketlist->mupllist (cdr rlist)))]))
       
(define (mupllist->racketlist mulplist)
  (cond [(equal? mulplist (aunit)) empty]
        [(apair? mulplist) (cons (apair-e1 mulplist) (mupllist->racketlist (apair-e2 mulplist)))]))


;; Problem 2



;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))


(define (eval-under-env e env)
  (cond
    [(int? e) e]
    [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error (format "MUPL addition applied to non-number ~v" v2))))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env)
                   )
               (error "MULP ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [exp (eval-under-env (mlet-e e) env)])
               (eval-under-env (mlet-body e) (list (cons var exp))))]
       [(fun? e)
         (closure env e)]

        [(closure? e) e]
         
        ;[(call? e)
        ; (let* (
        ;     [actual (eval-under-env (call-actual e) env)] ; arg
        ;     [func (eval-under-env (call-funexp e) (list (cons (fun-formal (closure-fun (call-funexp e))) actual)))]) ; must be closure
        ;   (func actual))]


        [(call? e) 
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([c1 (closure-fun v1)]
                      [c2 (closure-env v1)]
                      [cname (cons (fun-nameopt c1) v1)]
                      [cform (cons (fun-formal c1) v2)])
                 (eval-under-env
                  (fun-body c1)
                  (if (eq? (car cname) #f)
                      (cons cform c2)
                      (cons cform (cons cname c2)))))
               (error "MUPL applied to non-closure")))]

          [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) (apair-e2 v) (error "MUPL snd applied to a non pair")))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) (apair-e1 v) (error "MUPL fst applied to a non pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(aunit? e) e]
               
        [#t (error (format "bad MUPL expression: ~v" e))]))



;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifaunit e1 e2 e3)
  (let ([res e1])
    (if (isaunit? res) e2  e3)))


(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))


(define (ifeq e1 e2 e3 e4)
  (let ([v1 e1]
        [v2 e2])
    (if (= (int-num v1) (int-num v2))  e3 e4)))

;; Problem 4


