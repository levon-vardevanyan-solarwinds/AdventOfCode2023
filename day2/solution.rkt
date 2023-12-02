#lang racket

(define (split-game-record str)
  (car (regexp-match* #px"^Game (\\d+):(.*)$" #:match-select cdr str)))

(define (restructure str)
  (for/list ([grab (string-split str ";")])
    (map (λ (x) (reverse (string-split x " "))) (string-split grab ","))))

(define game-data
  (for/list ([record (map split-game-record (file->lines "input.txt"))])
    (list (string->number (first record)) (restructure (second record)))))

(define limits '(("red" 12) ("green" 13) ("blue" 14)))

(define (within-limits? grab)
  (for/and ([limit limits])
    (let* ([color (car limit)] [max (cadr limit)] [current (assoc color grab)])
      (if current (<= (string->number (cadr current)) max) #t))))

(define (solve)
  (apply +
         (for/list ([line game-data] #:when (andmap within-limits? (cadr line)))
           (car line))))

(solve)

(define colors (map car limits))

(define dataset
  (for/list ([line game-data])
    (for/list ([grab (cadr line)])
      (for/list ([color colors])
        (let ([current (assoc color grab)]) (if current (string->number (cadr current)) 0))))))

(define (solve2)
  (apply +
         (for/list ([line dataset])
           (apply * (apply (curry map max) line)))))

(solve2)