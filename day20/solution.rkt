#lang racket

(define (parse-record str)
  (car (regexp-match* #px"^(broadcaster|[%&]\\w+) -> (.+)$" #:match-select cdr str)))

(define records (map parse-record (file->lines "input.txt")))

(define modules
  (for/hash ([line records])
    (values (if (string=? (first line) "broadcaster") (first line) (substring (first line) 1))
            (map string-trim (string-split (second line) ",")))))

(define module->type
  (for/hash ([line records] #:unless (string=? (first line) "broadcaster"))
    (values (substring (first line) 1) (substring (first line) 0 1))))

(define (starting-state)
  (make-hash (for/list ([line records] #:unless (string=? (first line) "broadcaster"))
               (define module (substring (first line) 1))
               (cons module
                     (if (string=? (substring (first line) 0 1) "%")
                         #f
                         (make-hash (for/list ([(key value) modules] #:when (member module value))
                                      (cons key 'low))))))))

(define (solve)
  (define queue '())
  (define result (make-hash (list (cons 'high 0) (cons 'low 0))))
  (define module->state (starting-state))

  (define (send-pulse to pulse from)
    (hash-update! result pulse add1)
    ;(printf "~a -~a -> ~a\n" from pulse to)
    (set! queue (append queue (list (list to pulse from)))))

  (define (button-push pulse)
    (send-pulse "broadcaster" 'low "button")
    (for ([module (hash-ref modules "broadcaster")])
      (send-pulse module pulse "broadcaster")))

  (define (process-queue)
    (unless (empty? queue)
      (define module (caar queue))
      (define pulse (cadar queue))
      (define from (caddar queue))
      (define type (hash-ref module->type module "0"))
      (cond
        [(string=? type "%")
         ; Flip-flop module
         (when (eq? pulse 'low)
           (define state (hash-ref module->state module))
           (hash-set! module->state module (not state))
           (for ([to (hash-ref modules module)])
             (send-pulse to (if state 'low 'high) module)))]
        [(string=? type "&")
         ; Conjunction module
         (let ([memory (hash-ref module->state module)])
           (hash-set! memory from pulse)
           (for ([to (hash-ref modules module)])
             (send-pulse to
                         (if (andmap (λ (x) (eq? x 'high)) (hash-values memory)) 'low 'high)
                         module)))]
        [(string=? type "0") #t])
      (set! queue (cdr queue))
      (process-queue)))

  (for ([i (in-range 1000)])
    (button-push 'low)
    (process-queue))

  ; visualise schema with graphviz
  (define out-port (open-output-file "graph.dat" #:exists 'replace))

  (displayln "digraph {" out-port)

  (for ([[key value] modules])
    (for ([to value])
      (fprintf out-port "  ~a -> ~a\n" key to)))

  (for ([(module type) module->type])
    (cond
      [(string=? type "&") (fprintf out-port "  ~a [color=\"red\"]\n" module)]
      [(string=? type "%") (fprintf out-port "  ~a [color=\"blue\"]\n" module)]
      [else (fprintf out-port "  ~a [color=\"green\"]\n" module)]))

  (displayln "}" out-port)

  (close-output-port out-port)
  (system "/opt/homebrew/bin/dot -Tsvg graph.dat >output.svg")

  (apply * (hash-values result)))

(solve)

(define (button-press-count stop-to stop-from)
  (define queue '())
  (define module->state (starting-state))

  (define (send-pulse to pulse from)
    (set! queue (append queue (list (list to pulse from)))))

  (define (button-push pulse)
    (send-pulse "broadcaster" 'low "button")
    (for ([module (hash-ref modules "broadcaster")])
      (send-pulse module pulse "broadcaster")))

  (define stop-flag #f)

  (define (process-queue)
    (unless (empty? queue)
      (define module (caar queue))
      (define pulse (cadar queue))
      (define from (caddar queue))
      (when (and (string=? module stop-to) (eq? pulse 'low) (string=? from stop-from))
        (set! stop-flag #t))
      (define type (hash-ref module->type module "0"))
      (cond
        [(string=? type "%")
         ; Flip-flop module
         (when (eq? pulse 'low)
           (define state (hash-ref module->state module))
           (hash-set! module->state module (not state))
           (for ([to (hash-ref modules module)])
             (send-pulse to (if state 'low 'high) module)))]
        [(string=? type "&")
         ; Conjunction module
         (let ([memory (hash-ref module->state module)])
           (hash-set! memory from pulse)
           (for ([to (hash-ref modules module)])
             (send-pulse to
                         (if (andmap (λ (x) (eq? x 'high)) (hash-values memory)) 'low 'high)
                         module)))]
        [(string=? type "0") #t])
      (set! queue (cdr queue))
      (process-queue)))

  (for/last ([i (in-naturals 1)] #:break stop-flag)
    (button-push 'low)
    (process-queue)
    i))

(lcm (button-press-count "vd" "nx")
     (button-press-count "bh" "zp")
     (button-press-count "ns" "dj")
     (button-press-count "dl" "bz"))
