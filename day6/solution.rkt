#lang racket

(define (split-game-record str)
  (string-split (cadr (string-split str ":")) " " #:repeat? #t))

(define dataset (map split-game-record (file->lines "input.txt")))

(define (solve)
  (apply *
         (map length
              (for/list ([time (map string->number (first dataset))]
                         [distance (map string->number (second dataset))])
                (filter (λ (x) (> x distance))
                        (for/list ([hold (in-range 1 time)])
                          (* hold (- time hold))))))))

(solve)

(define (solve2)
  (define time (string->number (string-join (first dataset) "")))
  (define distance (string->number (string-join (second dataset) "")))
  (for/sum ([hold (in-range 1 time)] #:when (> (* hold (- time hold)) distance)) 1))

(solve2)
