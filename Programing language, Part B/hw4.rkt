
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


;; put your code below


; Q1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Q2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Q3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Q4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Q5
(define funny-number-stream
  (letrec ([helper (lambda (x)
                     (if (= (remainder x 5) 0)
                         (cons (- x) (lambda () (helper (+ x 1))))
                         (cons x (lambda () (helper (+ x 1))))))])
    (lambda () (helper 1))))

; Q6
(define dan-then-dog
  (letrec ([helper (lambda (pic)
                     (if (equal? pic "dan.jpg")
                         (cons "dog.jpg" (lambda () (helper "dog.jpg")))
                         (cons "dan.jpg" (lambda () (helper "dan.jpg")))))])
    (lambda () (helper "dog.jpg"))))

; Q7
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

; Q8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
               (let ([x (list-nth-mod xs n)]
                    [y (list-nth-mod ys n)])
                 (cons (cons x y) (lambda () (f (+ n 1))))))])
  (lambda () (f 0))))

;Q9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (>= n (vector-length vec))
                    #f
                    (let ([ith (vector-ref vec n)])
                      (if (and (pair? ith) (equal? (car ith) v))
                          ith
                          (f (+ n 1))))))])
    (f 0)))
            
;Q10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v memo)
          (let ([new-ans (assoc v xs)])
            (and new-ans
                 (begin
                   (vector-set! memo pos new-ans)
                   (set! pos (remainder (+ pos 1) n))
                   new-ans)))))))





