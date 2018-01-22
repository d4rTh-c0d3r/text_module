#lang racket
(struct dictionary (word branches) #:transparent)
(define my-dictionary (dictionary "" (make-vector 26 #f)))
(define (null-string? s) (equal? s ""))

(define (add-to-dictionary-word word book)
  (define (add-to-dictionary-helper current word book)
    (cond [(null-string? current) (if (dictionary? book) (dictionary word (dictionary-branches book))
                                      (dictionary word (make-vector 26 #f)))]
          [else
           (let* ([n (get-index (string-ref current 0))]
                  [d (if (dictionary? book) book (dictionary "" (make-vector 26 #f)))]
                  [w (dictionary-word d)]
                  [b (dictionary-branches d)]
                  [v (vector-ref b n)])
             (set! v (add-to-dictionary-helper (substring current 1) word (if (not (equal? #f v)) v (dictionary "" (make-vector 26 #f)))))
             (vector-set! b n v)
             (dictionary w b))]))
  (set! book (add-to-dictionary-helper word word book)))

(define (add-to-dictionary-list words book)
  (map (lambda (w) (begin (add-to-dictionary-word w book) w)) words)
  (set! words words))

(define (get-index c)
  (cond [(char=? c #\a) 00] [(char=? c #\b) 01] [(char=? c #\c) 02] [(char=? c #\d) 03]
        [(char=? c #\e) 04] [(char=? c #\f) 05] [(char=? c #\g) 06] [(char=? c #\h) 07]
        [(char=? c #\i) 08] [(char=? c #\j) 09] [(char=? c #\k) 10] [(char=? c #\l) 11]
        [(char=? c #\m) 12] [(char=? c #\n) 13] [(char=? c #\o) 14] [(char=? c #\p) 15]
        [(char=? c #\q) 16] [(char=? c #\r) 17] [(char=? c #\s) 18] [(char=? c #\t) 19]
        [(char=? c #\u) 20] [(char=? c #\v) 21] [(char=? c #\w) 22] [(char=? c #\x) 23]
        [(char=? c #\y) 24] [(char=? c #\z) 25]))

(define (print-dictionary book)
  (let ([ans '()]
        [w (dictionary-word book)]
        [v (dictionary-branches book)])
    (cond [(not (null-string? w)) (set! ans (cons w ans))])
    (begin
      (map (lambda (b) (begin (cond [(not (eq? #f b)) (set! ans (append ans (print-dictionary  b)))]) #t)) (vector->list v))
      ans)))

(define (auto-complete word book)
  (cond [(not (dictionary? book)) '()]
        [(null-string? word) (print-dictionary book)]
        [else (auto-complete (substring word 1) (vector-ref (dictionary-branches book) (get-index (string-ref word 0))))]))