#lang racket
(define (parse-rotation rotation)
  (match rotation
    [(pregexp #px"^L(\\d+)$" (list _ (app string->number distance))) (- distance)]
    [(pregexp #px"^R(\\d+)$" (list _ (app string->number distance))) distance]))

(define input-data (map parse-rotation (file->lines "input.txt")))