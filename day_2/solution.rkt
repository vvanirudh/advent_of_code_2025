#lang racket

;; parsing input
(define (strip string)
  (string-replace string "\n" ""))

(define (parse-range stringrange)
  (let([range (string-split stringrange "-")])
    (map string->number range)))

(define input (map (compose1 parse-range strip) (string-split (file->string "input.txt") ",")))

;; puzzle 1
(define (compute-sum-repeating-all ranges repeating-op?)
  (foldl + 0 (map (lambda (range) (compute-sum-repeating range repeating-op?)) ranges)))

(define (compute-sum-repeating range repeating-op?)
  (let([first (car range)]
       [second (car (cdr range))]
       [rest (cdr range)])
  (cond
    [(> first second) 0]
    [(repeating-op? first) (+ first (compute-sum-repeating (cons (+ 1 first) rest) repeating-op?))]
    [else (compute-sum-repeating (cons (+ 1 first) rest) repeating-op?)])))

(define (repeating-twice? number)
  (let*([digits (number->string number)]
        [half (quotient (string-length digits) 2)])
    (cond
      [(odd? (string-length digits)) #f]
      [else (equal? (substring digits 0 half) (substring digits half))])))

(compute-sum-repeating-all input repeating-twice?)

;; puzzle 2
(define (all-prefixes digits)
  (for/list ([i (string-length digits)])
    (substring digits 0 i)))

(define (repeating-n? number)
  (let*([digits (number->string number)])
    (ormap (lambda (pattern) (repeating-n-digits? digits pattern)) (all-prefixes digits))))

(define (repeating-n-digits? digits pattern)
  (let*([pattern-length (string-length pattern)])
    (cond
      [(equal? digits "") #t]
      [(equal? pattern "") #f]
      [(< (string-length digits) pattern-length) #f]
      [else (and (equal? pattern (substring digits 0 pattern-length)) (repeating-n-digits? (substring digits pattern-length) pattern))])))

(compute-sum-repeating-all input repeating-n?)