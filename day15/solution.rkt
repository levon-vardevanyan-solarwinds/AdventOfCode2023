#lang racket

(define raw (string-trim (file->string "input.txt")))
(define lines (string-split raw ","))

(define (custom-hash line)
  (for/fold ([current 0]) ([ch (string->list line)])
    (remainder (* (+ current (char->integer ch)) 17) 256)))

(apply + (map custom-hash lines))

(define dataset (map (λ (x) (string-split x #px"[=-]")) lines))

(define result (build-vector 256 (const '())))

(define (handle= pos record)
  (define label (first record))
  (define old-value (vector-ref result pos))
  (define index (index-where old-value (λ (x) (string=? label (first x)))))
  (vector-set! result pos (if index (list-set old-value index record) (cons record old-value))))

(define (handle- pos record)
  (define old-value (vector-ref result pos))
  (vector-set! result pos (remove record old-value (λ (lhs rhs) (string=? (first lhs) (first rhs))))))

(for ([record dataset])
  (define pos (custom-hash (first record)))
  (if (= (length record) 2) (handle= pos record) (handle- pos record)))

(define (focusing-power box n)
  (for/sum ([record (reverse box)] [i (in-naturals 1)]) (* n i (string->number (second record)))))

(for/sum ([box result] [n (in-naturals 1)]) (focusing-power box n))
