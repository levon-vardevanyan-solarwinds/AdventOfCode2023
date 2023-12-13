#lang racket

(define dataset
  (for/list ([raw (file->lines "input.txt")])
    (let* ([line (string-split raw)]
           [schema (car line)]
           [counts (map string->number (string-split (cadr line) ","))])
      (list schema counts))))

(define gc
  (let ([cache (make-hash)])
    ; depth is how deep we're in a # island
    ; isles is count of processed islands
    (define (gc lst counts depth isles)
      (define args (list lst counts depth isles))
      (cond
        [(empty? lst) (if (= (length counts) isles) 1 0)]
        [(hash-has-key? cache args) (hash-ref cache args)]
        [else
         (let ([value (cond
                        [(char=? (car lst) #\#) (gc (cdr lst) counts (add1 depth) isles)]
                        [(char=? (car lst) #\.)
                         (cond
                           ; we just finished an island and it's size fits the answer
                           [(and (< isles (length counts)) (= depth (list-ref counts isles)))
                            (gc (cdr lst) counts 0 (add1 isles))]
                           ; jumped from . to . so just continue
                           [(zero? depth) (gc (cdr lst) counts depth isles)]
                           ; either more islands than expected or island is too large etc
                           [else 0])]
                        ; if we hit ? branch out into # and .
                        [(char=? (car lst) #\?)
                         (+ (gc (cons #\# (cdr lst)) counts depth isles)
                            (gc (cons #\. (cdr lst)) counts depth isles))])])
           (hash-set! cache args value)
           value)]))
    gc))

(define get-count gc)

(define (solve line)
  (define schema (string->list (first line)))
  (define counts (second line))
  (get-count schema counts 0 0))

(apply + (map solve dataset))

(define (x5 x)
  (build-list 5 (λ (_) x)))

(define (unfold-schema schema)
  (string-join (x5 schema) "?"))

(define (unfold-counts counts)
  (apply append (x5 counts)))

(define (solve2 line)
  (define schema (string->list (string-append (unfold-schema (first line)) ".")))
  (define counts (unfold-counts (second line)))
  (get-count schema counts 0 0))

(apply + (map solve2 dataset))
