#lang racket

; rotate board counterclockwise
(define (rotate-ccw board)
  (map list->string (map reverse (apply map list (map string->list board)))))

; rotate board clockwise
(define (rotate-cw board)
  (reverse (map list->string (apply map list (map string->list board)))))

(define (tilt-line-west str)
  (define lst (map string->list (string-split str "#" #:trim? #f)))
  (string-join (map list->string (map (λ (x) (sort x char<?)) lst)) "#"))

(define (tilt-north board)
  (rotate-cw (map tilt-line-west (rotate-ccw board))))

(define (total-load-north board)
  (define stones-per-line (map (λ (x) (count (λ (ch) (char=? ch #\O)) x)) (map string->list board)))
  (for/sum ([stones (reverse stones-per-line)] [weight (in-naturals 1)]) (* stones weight)))

(define board (file->lines "input.txt"))
(total-load-north (tilt-north board))

(define (cycle board)
  (define (it x)
    (rotate-ccw (tilt-north x)))
  (it (it (it (it board)))))

(define history '())

(define knot
  (for/fold ([b board]) ([i (in-naturals)] #:break (member b history))
    (set! history (cons b history))
    (cycle b)))

(define noose (member knot (reverse history)))
(define noose-len (length noose))

(total-load-north (list-ref noose (modulo (- 1000000000 (- (length history) noose-len)) noose-len)))