#lang racket

(require racket/draw)

(define scale 3)

(define (str->point str)
  (map (λ (x) (* scale x)) (map string->number (string-split str))))

(define target (make-bitmap (* scale 140) (* scale 140)))
(define dc (new bitmap-dc% [bitmap target]))
(define blue-brush (new brush% [color "blue"]))
(send dc set-brush blue-brush)

(define points (file->lines "output.txt"))

(define start (str->point (car points)))

(define path (new dc-path%))
(send path move-to (first start) (second start))
(for ([point (cdr points)])
  (let ([next (str->point point)])
    (send path line-to (first next) (second next))))

(send dc draw-path path)
(send target save-file "maze.png" 'png)