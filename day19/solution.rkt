#lang racket

(define (get-raiting all f)
  (f all))

(define x (curryr get-raiting first))
(define m (curryr get-raiting second))
(define a (curryr get-raiting third))
(define s (curryr get-raiting fourth))

(define string->accessor (hash "x" x "m" m "a" a "s" s))
(define string->comparator (hash "<" < ">" >))

(define (parse-pipe pipe)
  (define pipe-action (string-split pipe ":"))
  (list
   (apply build-predicate
          (car (regexp-match* #px"^([xmas])([<>])(\\d+)$" #:match-select cdr (first pipe-action))))
   (second pipe-action)))

(define (parse-flow line)
  (define tokens (reverse (string-split line ",")))
  (for/fold ([acc (list (list (const #t) (car tokens)))]) ([token (cdr tokens)])
    (cons (parse-pipe token) acc)))

(define (build-predicate lhs cmp rhs)
  (let ([comparator (hash-ref string->comparator cmp)]
        [accessor (hash-ref string->accessor lhs)]
        [number (string->number rhs)])
    (λ (ratings) (comparator (accessor ratings) number))))

(define (split-workflow str)
  (car (regexp-match* #px"^(.+)\\{(.+)\\}$" #:match-select cdr str)))

(define (solve)
  (define workflows
    (for/hash ([line (map split-workflow (file->lines "workflows.txt"))])
      (values (first line) (parse-flow (second line)))))

  (define (parse-ratings str)
    (map
     string->number
     (car (regexp-match* #px"^\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}$" #:match-select cdr str))))

  (define ratings (map parse-ratings (file->lines "ratings.txt")))

  (define (process rating flows)
    (let* ([flow (car flows)] [predicate (first flow)] [next (second flow)])
      (if (predicate rating)
          (cond
            [(string=? next "A") #t]
            [(string=? next "R") #f]
            [else (process rating (hash-ref workflows next))])
          (process rating (cdr flows)))))

  (apply + (flatten (filter (λ (rating) (process rating (hash-ref workflows "in"))) ratings))))

(solve)

(define workflows
  (for/hash ([line (map split-workflow (file->lines "workflows.txt"))])
    (values (first line) (map (λ (x) (string-split x ":")) (string-split (second line) ",")))))

(define (intersect-ranges a b)
  (let ([as (first a)] [ae (second a)] [bs (first b)] [be (second b)])
    (if (or (> as be) (> bs ae)) '() (list (max as bs) (min ae be)))))

(define result '())

(define (process ranges flows)
  (define flow (car flows))
  (cond
    [(or (empty? (x ranges)) (empty? (m ranges)) (empty? (a ranges)) (empty? (s ranges))) #f]
    [(= (length flow) 1)
     (cond
       [(string=? (first flow) "A") (set! result (cons ranges result)) #t]
       [(string=? (first flow) "R") #f]
       [else (process ranges (hash-ref workflows (first flow)))])]
    [else
     (let* ([splits (car (regexp-match* #px"^([xmas])([<>])(\\d+)$" #:match-select cdr (first flow)))]
            [accessor (first splits)]
            [cmp (second splits)]
            [number (string->number (third splits))])
       (define-values (satisfies inverse) (if (string=? cmp "<")
                                              (values (list 1 (sub1 number)) (list number 4000))
                                              (values (list (add1 number) 4000) (list 1 number))))
       (cond
         [(string=? accessor "x")
          (process (list (intersect-ranges (x ranges) satisfies) (m ranges) (a ranges) (s ranges)) (list (list (second flow))))
          (process (list (intersect-ranges (x ranges) inverse) (m ranges) (a ranges) (s ranges)) (cdr flows))]
         [(string=? accessor "m")
          (process (list (x ranges) (intersect-ranges (m ranges) satisfies) (a ranges) (s ranges)) (list (list (second flow))))
          (process (list (x ranges) (intersect-ranges (m ranges) inverse) (a ranges) (s ranges)) (cdr flows))]
         [(string=? accessor "a")
          (process (list (x ranges) (m ranges) (intersect-ranges (a ranges) satisfies) (s ranges)) (list (list (second flow))))
          (process (list (x ranges) (m ranges) (intersect-ranges (a ranges) inverse) (s ranges)) (cdr flows))]
         [(string=? accessor "s")
          (process (list (x ranges) (m ranges) (a ranges) (intersect-ranges (s ranges) satisfies)) (list (list (second flow))))
          (process (list (x ranges) (m ranges) (a ranges) (intersect-ranges (s ranges) inverse)) (cdr flows))]))]))

(process '((1 4000) (1 4000) (1 4000) (1 4000)) (hash-ref workflows "in"))

(for/sum ([ranges result])
  (for/product ([range ranges])
    (add1 (- (second range) (first range)))))
