#lang racket

(define (split-game-record str)
  (car (regexp-match* #px"^Card\\s+(\\d+):(.*)$" #:match-select cdr str)))

(define game-data
  (for/list ([record (map split-game-record (file->lines "input.txt"))])
    (let* ([splitted (map (λ (x) (string-split x " " #:repeat? #t)) (string-split (cadr record) "|"))]
           [winning (map string->number (car splitted))]
           [actual (map string->number (cadr splitted))])
      (list winning actual))))

(define matches
  (for/vector ([record game-data])
    (let* ([winning (list->set (car record))]
           [actual (list->set (cadr record))]
           [intersection (set-intersect winning actual)])
      (set-count intersection))))

(define (solve)
  (for/sum ([x matches]) (if (zero? x) 0 (expt 2 (sub1 x)))))

(solve)

(define (solve2)
  (define N (vector-length matches))
  (define cards (make-vector N 1))

  (for ([i (in-range N)])
    (for ([j (in-range (add1 i) (min N (+ i (vector-ref matches i) 1)))])
      (vector-set! cards j (+ (vector-ref cards j) (vector-ref cards i)))))
  (for/sum ([i cards]) i))

(solve2)
