#lang racket

(define (split-record str)
  (regexp-match* #px"^(\\w\\w\\w) = \\((\\w\\w\\w), (\\w\\w\\w)\\)$" #:match-select cdr str))

(define lines (file->lines "input.txt"))

(define instructions (list->vector (string->list (car lines))))

(define schema
  (for/hash ([record (map split-record (cddr lines))])
    (let ([line (first record)]) (values (first line) (list (second line) (third line))))))

(define (recursive instruction index step finished?)
  (if (finished? instruction)
      step
      (let ([next-index (if (= (add1 index) (vector-length instructions)) 0 (add1 index))]
            [element (hash-ref schema instruction)]
            [direction (vector-ref instructions index)])
        (if (char=? direction #\L)
            (recursive (first element) next-index (add1 step) finished?)
            (recursive (second element) next-index (add1 step) finished?)))))

(recursive "AAA" 0 0 (λ (x) (string=? x "ZZZ")))
(define starting-points (filter (λ (x) (string-suffix? x "A")) (hash-keys schema)))
(apply lcm (map (λ (start) (recursive start 0 0 (λ (x) (string-suffix? x "Z")))) starting-points))