#lang racket

; Delete later
(struct dictionary (word frequency branches) #:transparent)
(define my-dictionary (dictionary "" 0 (make-vector 26 #f)))
(define (null-string? s) (equal? s ""))
(define (get-index c)
  (cond [(char=? c #\a) 00] [(char=? c #\b) 01] [(char=? c #\c) 02] [(char=? c #\d) 03]
        [(char=? c #\e) 04] [(char=? c #\f) 05] [(char=? c #\g) 06] [(char=? c #\h) 07]
        [(char=? c #\i) 08] [(char=? c #\j) 09] [(char=? c #\k) 10] [(char=? c #\l) 11]
        [(char=? c #\m) 12] [(char=? c #\n) 13] [(char=? c #\o) 14] [(char=? c #\p) 15]
        [(char=? c #\q) 16] [(char=? c #\r) 17] [(char=? c #\s) 18] [(char=? c #\t) 19]
        [(char=? c #\u) 20] [(char=? c #\v) 21] [(char=? c #\w) 22] [(char=? c #\x) 23]
        [(char=? c #\y) 24] [(char=? c #\z) 25]))
(define (sort-words vec)
  (vector-sort vec (lambda (a b) (> (cdr a) (cdr b)))))
; Delete later

(define alphabets '( #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                     #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define (correct? word book)
  (define (correct?-helper current book)
    (cond [(not (dictionary? book)) #f]
          [(null-string? current)
           (if (string=? word (dictionary-word book))
               (dictionary-frequency book)
               #f)]
          [else
           (let ([n (get-index (string-ref current 0))])
             (correct?-helper
              (substring current 1)
              (vector-ref (dictionary-branches book) n)))]))
  (correct?-helper word book))

(define (auto-correct word)
  (if
   (correct? word my-dictionary)
   word
   (let* ([a (type-1 word)]
          [b (type-2 word)]
          [c (type-3 word)]
          [d (type-4 word)]
          [e (add-ranks (append a b c d))])
     (vector->list (sort-words (list->vector e))))))

(define (type-1 word)
  (let ([l (string->list word)]
        [ans '()])
    (begin (map (lambda (c) (begin (set! ans (cons (list->string (remove c l)) ans)) c)) l)
           ans)))

(define (type-2 word)
  (define (type-2-helper start d end ans)
    (cond [(null? end) (cons (list->string (append start (list d))) ans)]
          [else
           (type-2-helper (append start (list (car end)))
                          d
                          (cdr end)
                          (cons (list->string (append start (list d) end)) ans))]))
  (let ([ans '()])
    (begin (map (lambda (d) (begin (set! ans (cons (type-2-helper '() d (string->list word) '()) ans)) d)) alphabets)
           (flatten ans))))

(define (type-3 word)
  (define (type-3-helper start d end ans)
    (cond [(null? end) ans]
          [(type-3-helper (append start (list (car end))) d (cdr end) (cons (list->string (append start (list d) (cdr end))) ans))]))    
  (let ([ans '()])
    (begin (map (lambda (d) (begin (set! ans (cons (type-3-helper '() d (string->list word) '()) ans)) d)) alphabets)
           (flatten ans))))

(define (type-4 word)
  (define (type-4-helper start w1 w2 end ans)
    (cond [(null? end) (cons (list->string (append start (list w2) (list w1))) ans)]
          [(type-4-helper (append start (list w1))
                          w2 (car end) (cdr end)
                          (cons (list->string (append start (list w2) (list w1) end)) ans))]))
  (let ([ans '()]
        [l (string->list word)])
    (type-4-helper '() (car l) (cadr l) (cddr l) '())))

(define (add-ranks words)
  (define (refine l ans)
    (if (null? l) ans
        (let* ([a (car l)]
               [b (if (eq? #f (cdr a)) ans (cons a ans))])
          (refine (cdr l) b))))
  (refine (map (lambda (w) (cons w (correct? w my-dictionary))) words) '()))





  
  




