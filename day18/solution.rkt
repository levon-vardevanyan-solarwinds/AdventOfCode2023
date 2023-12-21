#lang racket

(require racket/draw)

(define (draw instructions)
  (define target (make-bitmap 1000 1000))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-pen "blue" 1 'solid)

  (for/fold ([cursor (cons 500 500)]) ([line instructions])
    (define direction (first line))
    (define distance (string->number (second line)))
    (cond
      [(string=? direction "U")
       (send dc draw-line (cdr cursor) (car cursor) (cdr cursor) (- (car cursor) distance))
       (cons (- (car cursor) distance) (cdr cursor))]
      [(string=? direction "D")
       (send dc draw-line (cdr cursor) (car cursor) (cdr cursor) (+ (car cursor) distance))
       (cons (+ (car cursor) distance) (cdr cursor))]
      [(string=? direction "L")
       (send dc draw-line (cdr cursor) (car cursor) (- (cdr cursor) distance) (car cursor))
       (cons (car cursor) (- (cdr cursor) distance))]
      [(string=? direction "R")
       (send dc draw-line (cdr cursor) (car cursor) (+ (cdr cursor) distance) (car cursor))
       (cons (car cursor) (+ (cdr cursor) distance))]))

  (send target save-file "flood.png" 'png))

(define part1 (map string-split (file->lines "input.txt")))
(draw part1)

(define number->direction (hash 0 "R" 1 "D" 2 "L" 3 "U"))

(define (parse-hex line)
  (define integers
    (map (λ (x) (string->number x 16))
         (car (regexp-match* #px"^[UDLR] \\d+ \\(#(.{5})(.)\\)$" #:match-select cdr line))))
  (list (hash-ref number->direction (second integers)) (number->string (first integers))))

(define part2 (map parse-hex (file->lines "input.txt")))

(define (instructions->points instructions)
  (list->vector
   (reverse (for/fold ([points (list (cons 0 0))]) ([line instructions])
              (define cursor (first points))
              (define direction (first line))
              (define distance (string->number (second line)))
              (cond
                [(string=? direction "U") (cons (cons (- (car cursor) distance) (cdr cursor)) points)]
                [(string=? direction "D") (cons (cons (+ (car cursor) distance) (cdr cursor)) points)]
                [(string=? direction "L") (cons (cons (car cursor) (- (cdr cursor) distance)) points)]
                [(string=? direction "R")
                 (cons (cons (car cursor) (+ (cdr cursor) distance)) points)])))))

(define (instructions->boundary instructions)
  (for/sum ([line instructions]) (string->number (second line))))

(define (solve instructions)

  (define points (instructions->points instructions))
  (define N (vector-length points))
  (define b (instructions->boundary instructions))

  ; SHOELACE FORMULA https://en.wikipedia.org/wiki/Shoelace_formula
  (define A
    (/ (abs (for/sum ([i (in-range 0 N)])
                     (define i-1 (if (zero? i) (sub1 N) (sub1 i)))
                     (define i+1 (modulo (add1 i) N))
                     (* (car (vector-ref points i))
                        (- (cdr (vector-ref points i+1)) (cdr (vector-ref points i-1))))))
       2))

  ; PICK'S THEOREM https://en.wikipedia.org/wiki/Pick%27s_theorem
  (define i (add1 (- A (/ b 2))))

  (+ i b))

(solve part1)
(solve part2)
