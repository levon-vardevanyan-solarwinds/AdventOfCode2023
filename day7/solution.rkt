#lang racket

(define hand->bid
  (for/hash ([line (file->lines "input.txt")])
    (let* ([splitted (string-split line " ")]
           [lhs (string->list (first splitted))]
           [rhs (string->number (second splitted))])
      (values lhs rhs))))

(define (combination-strength combination)
  (define strengths
    (vector 'high-card
            'one-pair
            'two-pair
            'three-of-a-kind
            'full-house
            'four-of-a-kind
            'five-of-a-kind))
  (vector-memq combination strengths))

(define (assign hand)
  (define h (make-hash))
  (for ([card hand])
    (hash-update! h card add1 0))

  (define sorted
    (sort (for/list ([(key value) (in-hash h)])
            (cons value key))
          (λ (lhs rhs) (> (car lhs) (car rhs)))))

  (cond
    [(= 5 (car (first sorted))) 'five-of-a-kind]
    [(= 4 (car (first sorted))) 'four-of-a-kind]
    [(and (= 3 (car (first sorted))) (= 2 (car (second sorted)))) 'full-house]
    [(= 3 (car (first sorted))) 'three-of-a-kind]
    [(and (= 2 (car (first sorted))) (= 2 (car (second sorted)))) 'two-pair]
    [(= 2 (car (first sorted))) 'one-pair]
    [(= 1 (car (first sorted))) 'high-card]))

(define (substitute-jokers hand)
  (define possibilities '("2" "3" "4" "5" "6" "7" "8" "9" "T" "Q" "K" "A"))
  (argmax combination-strength
          (map assign
               (for/list ([J possibilities])
                 (string-replace hand "J" J)))))

(define (solve f strengths)
  (define (card-strength card)
    (vector-member card strengths))

  (define (lexicographic-less lhs rhs)
    (cond
      [(null? lhs) #f] ; assuming both empty - equal (not less)
      [(char=? (car lhs) (car rhs)) (lexicographic-less (cdr lhs) (cdr rhs))]
      [(< (card-strength (car lhs)) (card-strength (car rhs))) #t]
      [else #f]))

  (define dataset (make-vector 7 '()))

  (define hands (map first (map (λ (x) (string-split x " ")) (file->lines "input.txt"))))

  (for ([hand hands])
    (let* ([combination (f hand)] [strength (combination-strength combination)])
      (vector-set! dataset strength (cons (string->list hand) (vector-ref dataset strength)))))

  (define highscore (vector->list (vector-map (λ (x) (sort x lexicographic-less)) dataset)))

  (for/sum ([hand (apply append highscore)] [rank (in-naturals 1)])
           (* (hash-ref hand->bid hand) rank)))

(solve assign #(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))
(solve substitute-jokers #(#\J #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\Q #\K #\A))
