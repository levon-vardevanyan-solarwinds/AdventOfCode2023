#lang racket

(define lines
  (for/list ([line (map string-split (file->lines "input.txt"))])
    (list->vector (map string->number line))))

(define (reduce v)
  (define N (vector-length v))
  (define result (make-vector (sub1 N) 0))
  (for/vector ([i (in-range 1 N)])
    (- (vector-ref v i) (vector-ref v (sub1 i)))))

(define (pyramide line result)
  (define N (vector-length line))
  (if (or (= N 0) (= (vector-count zero? line) N))
      result
      (let ([v (reduce line)]) (pyramide v (cons v result)))))

(define (next-element line f)
  (define p (pyramide line (list line)))
  (for/fold ([acc 0]) ([v p])
    (f acc v)))

(define (part1 acc v)
  (+ acc (vector-ref v (sub1 (vector-length v)))))

(define (part2 acc v)
  (- (vector-ref v 0) acc))

(apply + (map (λ (x) (next-element x part1)) lines))
(apply + (map (λ (x) (next-element x part2)) lines))
