;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
    (aunit)
    (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist mxs)
  (if (aunit? mxs)
    null
    (cons (apair-e1 mxs) (mupllist->racketlist (apair-e2 mxs)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)] [v2 (eval-under-env (ifgreater-e2 e) env)])
          (if (and (int? v1) (int? v2))
            (if (> (int-num v1) (int-num v2))
              (eval-under-env (ifgreater-e3 e) env)
              (eval-under-env (ifgreater-e4 e) env))
            (error "MUPL ifgreater appplied to non-number")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)] [var-value (eval-under-env (mlet-e e) env)])
          (if (string? var-name)
            (eval-under-env (mlet-body e) (cons (cons var-name var-value) env))
            (error "MUPL mlet applied to non-string-variable-name")))]
        [(call? e)
         (let
          ([closure-in-call (eval-under-env (call-funexp e) env)]
           [argument-in-call (eval-under-env (call-actual e) env)])
          (if (closure? closure-in-call)
            (let*
              ([fun-in-closure (closure-fun closure-in-call)]
               [env-in-closure (closure-env closure-in-call)]
               [fun-name (fun-nameopt fun-in-closure)]
               [parameter-name (fun-formal fun-in-closure)]
               [formal-to-actual (cons parameter-name argument-in-call)]
               [env-for-call (cons formal-to-actual env-in-closure)]
               [fun-name-to-closure (cons fun-name closure-in-call)]
               [env-for-call-with-fun (cons fun-name-to-closure env-for-call)])
              (eval-under-env
                (fun-body fun-in-closure)
                (if fun-name env-for-call-with-fun env-for-call)))
            (error "MUPL call applied to non-closure")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([result (eval-under-env (fst-e e) env)])
          (if (apair? result) (apair-e1 result) (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([result (eval-under-env (snd-e e) env)])
          (if (apair? result) (apair-e2 result) (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([result (eval-under-env (isaunit-e e) env)])
          (if (aunit? result) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

; (define (ifaunit e1 e2 e3) (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
    e2
    (let ([current-binding (car lstlst)])
      (mlet (car current-binding) (cdr current-binding) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
    (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun "map" "proc"
    (fun #f "lst"
      (ifaunit (var "lst")
        (aunit)
        (apair
          (call (var "proc") (fst (var "lst")))
          (call (call (var "map") (var "proc")) (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
    (fun #f "i"
      (fun #f "ilst"
        (call
          (call (var "map") (fun #f "element" (add (var "i") (var "element"))))
          (var "ilst"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

(define (get-free-vars e)
  (cond
    [(var? e) (set (var-string e))]
    [(fun? e)
      (let ([free-vars (set-remove (get-free-vars (fun-body e)) (fun-formal e))])
        (if (fun-nameopt e) (set-remove free-vars (fun-nameopt e)) free-vars))]
    [(call? e)
      (set-union
        (get-free-vars (call-funexp e))
        (get-free-vars (call-actual e)))]
    [(add? e)
      (set-union
        (get-free-vars (add-e1 e))
        (get-free-vars (add-e2 e)))]
    [(ifgreater? e)
      (set-union
        (get-free-vars (ifgreater-e1 e))
        (get-free-vars (ifgreater-e2 e))
        (get-free-vars (ifgreater-e3 e))
        (get-free-vars (ifgreater-e4 e)))]
    [(mlet? e)
      (set-remove
        (set-union (get-free-vars (mlet-e e)) (get-free-vars (mlet-body e)))
        (mlet-var e))]
    [(apair? e)
      (set-union
        (get-free-vars (apair-e1 e))
        (get-free-vars (apair-e2 e)))]
    [(fst? e) (get-free-vars (fst-e e))]
    [(snd? e) (get-free-vars (snd-e e))]
    [(isaunit? e) (get-free-vars (isaunit-e e))]
    [#t (set)]))

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond
    [(fun? e)
      (fun-challenge
        (fun-nameopt e)
        (fun-formal e)
        (compute-free-vars (fun-body e))
        (let ([free-vars (set-remove (get-free-vars (fun-body e)) (fun-formal e))])
          (if (fun-nameopt e) (set-remove free-vars (fun-nameopt e)) free-vars)))]
    [(call? e)
      (call
        (compute-free-vars (call-funexp e))
        (compute-free-vars (call-actual e)))]
    [(add? e)
      (add
        (compute-free-vars (add-e1 e))
        (compute-free-vars (add-e2 e)))]
    [(ifgreater? e)
      (ifgreater
        (compute-free-vars (ifgreater-e1 e))
        (compute-free-vars (ifgreater-e2 e))
        (compute-free-vars (ifgreater-e3 e))
        (compute-free-vars (ifgreater-e4 e)))]
    [(mlet? e)
      (mlet
        (mlet-var e)
        (compute-free-vars (mlet-e e))
        (compute-free-vars (mlet-body e)))]
    [(apair? e)
      (apair
        (compute-free-vars (apair-e1 e))
        (compute-free-vars (apair-e2 e)))]
    [(fst? e) (fst (compute-free-vars (fst-e e)))]
    [(snd? e) (snd (compute-free-vars (snd-e e)))]
    [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
    [#t e]))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun-challenge? e)
          (closure
            (filter
              (let ([freevars (fun-challenge-freevars e)])
                (lambda (current-env-element)
                  (not (set-member? freevars (car current-env-element)))))
              env)
            e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)] [v2 (eval-under-env-c (ifgreater-e2 e) env)])
          (if (and (int? v1) (int? v2))
            (if (> (int-num v1) (int-num v2))
              (eval-under-env-c (ifgreater-e3 e) env)
              (eval-under-env-c (ifgreater-e4 e) env))
            (error "MUPL ifgreater appplied to non-number")))]
        [(mlet? e)
         (let ([var-name (mlet-var e)] [var-value (eval-under-env-c (mlet-e e) env)])
          (if (string? var-name)
            (eval-under-env-c (mlet-body e) (cons (cons var-name var-value) env))
            (error "MUPL mlet applied to non-string-variable-name")))]
        [(call? e)
         (let
          ([closure-in-call (eval-under-env-c (call-funexp e) env)]
           [argument-in-call (eval-under-env-c (call-actual e) env)])
          (if (closure? closure-in-call)
            (let*
              ([fun-in-closure (closure-fun closure-in-call)]
               [env-in-closure (closure-env closure-in-call)]
               [fun-name (fun-challenge-nameopt fun-in-closure)]
               [parameter-name (fun-challenge-formal fun-in-closure)]
               [formal-to-actual (cons parameter-name argument-in-call)]
               [env-for-call (cons formal-to-actual env-in-closure)]
               [fun-name-to-closure (cons fun-name closure-in-call)]
               [env-for-call-with-fun (cons fun-name-to-closure env-for-call)])
              (eval-under-env-c
                (fun-challenge-body fun-in-closure)
                (if fun-name env-for-call-with-fun env-for-call)))
            (error "MUPL call applied to non-closure")))]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([result (eval-under-env-c (fst-e e) env)])
          (if (apair? result) (apair-e1 result) (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([result (eval-under-env-c (snd-e e) env)])
          (if (apair? result) (apair-e2 result) (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([result (eval-under-env-c (isaunit-e e) env)])
          (if (aunit? result) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
