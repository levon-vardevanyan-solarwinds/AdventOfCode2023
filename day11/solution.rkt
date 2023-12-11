#lang racket

(define (transpose lst)
  (map list->string (apply map list (map string->list lst))))

(define (galaxies dataset)
  (for/fold ([acc '()]) ([i (in-naturals)] [line dataset])
    (append acc
            (for/list ([j (indexes-of (string->list line) #\#)])
              (cons i j)))))

(define (shortest-path lst)
  (let ([lhs (first lst)] [rhs (second lst)])
    (+ (abs (- (car lhs) (car rhs))) (abs (- (cdr lhs) (cdr rhs))))))

(define (empty-indexes lines)
  (for/list ([line lines]
             [i (in-naturals)]
             #:when (= (count (λ (x) (char=? x #\.)) (string->list line)) (string-length line)))
    i))

(define dataset (file->lines "input.txt"))
(define empty-rows (empty-indexes dataset))
(define empty-cols (empty-indexes (transpose dataset)))

(define (solve f)
  (define original (list->vector (galaxies (file->lines "input.txt"))))
  (define expanded (vector-copy original))

  (for* ([row empty-rows] [i (in-range (vector-length original))])
    (let* ([original-x (car (vector-ref original i))]
           [galaxy (vector-ref expanded i)]
           [x (car galaxy)]
           [y (cdr galaxy)])
      (when (> original-x row)
        (vector-set! expanded i (cons (f x) y)))))

  (for* ([col empty-cols] [i (in-range (vector-length original))])
    (let* ([original-y (cdr (vector-ref original i))]
           [galaxy (vector-ref expanded i)]
           [x (car galaxy)]
           [y (cdr galaxy)])
      (when (> original-y col)
        (vector-set! expanded i (cons x (f y))))))

  (apply + (map shortest-path (combinations (vector->list expanded) 2))))

(solve add1)
(solve (curry + 999999))