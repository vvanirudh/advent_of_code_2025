#lang racket
;; sample fibonacci function
(define (fib n)
  (cond
    [(= n 1) 1]
    [(= n 2) 1]
    [else (+ (fib (- n 1 )) (fib (- n 2)))]))


;; Read input file
(define input (string-split (file->string "input.txt") "\n"))
;(displayln input)

;; Define count-zeros-at-end
(define (count-zeros-at-end rotations num)
  ; (displayln num)
  (cond
    [(empty? rotations) 0]
    [(= num 0) (+ 1 (count-zeros-at-end (cdr rotations) (next-num (car rotations) num)))]
    [else (count-zeros-at-end (cdr rotations) (next-num (car rotations) num))]))

;; Define next-num
(define (next-num rotation num)
  (cond
    [(equal? (substring rotation 0 1) "L") (op-next-num (substring rotation 1) num -)]
    [else (op-next-num (substring rotation 1) num +)]))

;; Define op-next-num
(define (op-next-num offset num op)
  (let ([new-num (op num (string->number offset))])
    (normalize new-num)))

;; normalize
(define (normalize num)
  (cond
    [(> num 99) (normalize (- num 100))]
    [(< num 0) (normalize (+ num 100))]
    [else num]))

;; puzzle 1
(count-zeros-at-end input 50)

;; deconstruct-rotations
(define (deconstruct-rotations rotations)
  (if (empty? rotations) rotations
      (let ([rest-rotations (cdr rotations)]
            [num (string->number (substring (car rotations) 1))]
            [dir (substring (car rotations) 0 1)])
        (cond
          [(= num 0) (deconstruct-rotations rest-rotations)]
          [else (cons
                 (string-append dir "1")
                 (deconstruct-rotations
                  (cons
                   (string-append dir (number->string (- num 1)))
                   rest-rotations)))]))))

;; puzzle 2
(count-zeros-at-end (deconstruct-rotations input) 50)