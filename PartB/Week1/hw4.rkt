
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
    (map (lambda (element) (string-append element suffix)) xs))

; 3
(define (list-nth-mod xs n)
    (cond
        [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))]) (car (list-tail xs i)))]))

; 4
(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (let ([stream-value (s)])
            (cons (car stream-value) (stream-for-n-steps (cdr stream-value) (- n 1))))))

; 5
(define funny-number-stream
    (letrec
        ([funny-number-stream-start-from
            (lambda (index)
                (lambda ()
                    (cons
                        (if (= 0 (remainder index 5))
                            (- index)
                            index)
                        (funny-number-stream-start-from (+ 1 index)))))])
        (funny-number-stream-start-from 1)))

; 6
(define dan-then-dog
    (letrec (
        [dan-stream (lambda () (cons "dan.jpg" dog-stream))]
        [dog-stream (lambda () (cons "dog.jpg" dan-stream))])
        dan-stream))

; 7
(define (stream-add-zero s)
    (letrec ([stream-value (s)])
        (lambda ()
            (cons
                (cons 0 (car stream-value))
                (stream-add-zero (cdr stream-value))))))

; 8
(define (cycle-lists xs ys)
    (letrec
        ([cycle-lists-start-from
            (lambda (index)
                (lambda ()
                    (cons
                        (cons (list-nth-mod xs index) (list-nth-mod ys index))
                        (cycle-lists-start-from (+ 1 index)))))])
        (cycle-lists-start-from 0)))

; 9
(define (vector-assoc v vec)
    (letrec
        ([check-vector-from
            (lambda (index)
                (if (>= index (vector-length vec))
                    #f
                    (let ([element (vector-ref vec index)])
                        (if (and (pair? element) (equal? v (car element)))
                            element
                            (check-vector-from (+ 1 index))))))])
        (check-vector-from 0)))

; 10
(define (cached-assoc xs n)
    (let ([cache (make-vector n #f)] [next-cache-slot-index 0])
        (lambda (v)
            (let ([cached-assoc-result-pair (vector-assoc v cache)])
                (if cached-assoc-result-pair
                    (cdr cached-assoc-result-pair)
                    (let ([assoc-result (assoc v xs)])
                        (begin
                            (vector-set! cache next-cache-slot-index assoc-result)
                            (set! next-cache-slot-index (remainder (+ 1 next-cache-slot-index) n))
                            assoc-result)))))))

; 11
(define-syntax while-less
    (syntax-rules (do)
        [(while-less e1 do e2)
            (letrec
                ([e1-result e1]
                [loop (lambda ()
                    (if (>= e2 e1-result)
                        #t
                        (loop)))])
                (loop))]))