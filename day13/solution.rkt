#lang racket

(define (vectors-equal-in-reverse? lhs rhs)
  (equal? (vector->list lhs) (reverse (vector->list rhs))))

(define (list-eater lst result)
  (if (empty? lst)
      result
      (let ([index (index-of lst "")])
        (define-values (lhs rhs) (split-at lst index))
        (list-eater (cdr rhs) (cons lhs result)))))

(define (list->matrix lst)
  (for/vector ([line lst])
    (list->vector (string->list line))))

(define records (map list->matrix (list-eater (file->lines "input.txt") '())))

(define (transpose matrix)
  (define N (vector-length matrix))
  (define M (vector-length (vector-ref matrix 0)))
  (for/vector ([j (in-range 0 M)])
    (for/vector ([i (in-range 0 N)])
      (vector-ref (vector-ref matrix i) j))))

(define (mirror-row matrix)
  (define N (vector-length matrix))
  (for/list ([i (in-range 1 N)]
             #:when (let ([slice-len (min i (- N i))])
                      (define-values (lhs rhs) (vector-split-at matrix i))
                      (vectors-equal-in-reverse? (vector-take-right lhs slice-len)
                                                 (vector-take rhs slice-len))))
    i))

(define (mirrors matrix)
  (append (map (curry * 100) (mirror-row matrix)) (mirror-row (transpose matrix))))

(apply + (flatten (map mirrors records)))

(define (invert ch)
  (let ([dict (hash #\# #\. #\. #\#)]) (hash-ref dict ch)))

(define (solve record)
  (define old-mirrors (mirrors record))

  (define N (vector-length record))
  (define M (vector-length (vector-ref record 0)))

  (first (for*/fold ([result '()])
                    ([i (in-range 0 N)] [j (in-range 0 M)] #:break (not (empty? result)))
           (let* ([new-matrix (for/vector ([k (in-naturals)] [line record])
                                (let ([new-line (vector-copy line)])
                                  (when (= k i)
                                    (vector-set! new-line j (invert (vector-ref line j))))
                                  new-line))])
             (set-subtract (mirrors new-matrix) old-mirrors)))))

(apply + (map solve records))
