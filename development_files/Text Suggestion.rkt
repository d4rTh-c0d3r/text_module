#lang racket
(require "data.rkt")
(require graphics)
(struct dictionary (word frequency branches) #:transparent)
(define my-dictionary (dictionary "" 0 (make-vector 26 #f)))
(define my-table (list ))
(define (null-string? s) (equal? s ""))

(define (get-index c)
  (cond [(char=? c #\a) 00] [(char=? c #\b) 01] [(char=? c #\c) 02] [(char=? c #\d) 03]
        [(char=? c #\e) 04] [(char=? c #\f) 05] [(char=? c #\g) 06] [(char=? c #\h) 07]
        [(char=? c #\i) 08] [(char=? c #\j) 09] [(char=? c #\k) 10] [(char=? c #\l) 11]
        [(char=? c #\m) 12] [(char=? c #\n) 13] [(char=? c #\o) 14] [(char=? c #\p) 15]
        [(char=? c #\q) 16] [(char=? c #\r) 17] [(char=? c #\s) 18] [(char=? c #\t) 19]
        [(char=? c #\u) 20] [(char=? c #\v) 21] [(char=? c #\w) 22] [(char=? c #\x) 23]
        [(char=? c #\y) 24] [(char=? c #\z) 25]))

(define (dictionary->vector book)
  (let ([ans '()]
        [w (dictionary-word book)]
        [n (dictionary-frequency book)]
        [v (dictionary-branches book)])
    (cond [(not (= n 0)) (set! ans (cons (cons w n) ans))])
    (map (lambda (s) (begin (cond [(dictionary? s) (set! ans (append ans (vector->list (dictionary->vector s))))]) #f)) (vector->list v))
    (list->vector ans)))

(define (sort-words vec)
  (vector-sort vec (lambda (a b) (> (cdr a) (cdr b)))))

(define (print-dictionary book)
  (map (lambda (w) (car w)) (vector->list (sort-words (dictionary->vector book)))))

(define (print-table words)
  (map (lambda (w) (car w)) words))

(define (add-to-dictionary-word word book)
  (define (add-to-dictionary-helper current word book)
    (cond [(null-string? current)
           (let* ([d (if (dictionary? book) book (dictionary "" 0 (make-vector 26 #f)))]
                  [w (dictionary-word d)]
                  [n (dictionary-frequency d)]
                  [v (dictionary-branches d)])
             (dictionary word (+ n 1) v))]
          [else
           (let* ([n (get-index (string-ref current 0))]
                  [d (if (dictionary? book) book (dictionary "" 0 (make-vector 26 #f)))]
                  [m (dictionary-frequency d)]
                  [w (dictionary-word d)]
                  [v (dictionary-branches d)]
                  [b (vector-ref v n)])
             (set! b (add-to-dictionary-helper (substring current 1) word (if (dictionary? b) b (dictionary "" 0 (make-vector 26 #f)))))
             (vector-set! v n b)
             (dictionary w m v))]))
  (add-to-dictionary-helper word word book))

(define (add-to-dictionary-list words book)
  (map (lambda (w) (begin (set! book (add-to-dictionary-word w book)) w)) words)
  (set! words words))

(define (add-to-table word-1 word-2 table)
  (let ([w (get-word word-1 table)])
    (cond [(eq? #f w) (insert-table (cons word-1 (list (cons word-2 1))) table)]
          [else
           (set! table (cdr w))
           (let ([l (cdr (car w))])
             (set! l (vector->list (sort-words (list->vector (insert-list word-2 l)))))
             (insert-table (cons word-1 l) table))])))

(define (add-to-table-list l table)
  (cond [(> (length l) 1) (begin (set! my-table (add-to-table (car l) (cadr l) table)) (add-to-table-list (cdr l) my-table))]))

(define (insert-table word* table)
  (cond [(null? table) (list word*)]
        [(string>? (car (car table)) (car word*)) (cons word* table)]
        [else
         (cons (car table) (insert-table word* (cdr table)))]))

(define (insert-list word l)
  (cond [(null? l) (list (cons word 1))]
        [(string=? word (car (car l))) (cons (cons word (+ 1 (cdr (car l)))) (cdr l))]
        [(string<? word (car (car l))) (cons (cons word 1) l)]
        [else
         (cons (car l) (insert-list word (cdr l)))]))

(define (get-word word table)
  (if (null? table) #f
      (let ([w (car (car table))])
        (cond [(string>? w word) #f]
              [(string=? w word)
               (let ([s (car table)])
                 (cons s (cdr table)))]
              [else
               (let ([t (get-word word (cdr table))])
                 (if (eq? #f t) #f
                     (cons (car t) (cons (car table) (cdr t)))))]))))           

(define (add-to-repository sentence book table)
  (let ([words (string-split sentence)])
    (add-to-dictionary-list words book)
    (add-to-table-list words table)))

(define (auto-complete word book)
  (cond [(not (dictionary? book)) '()]
        [(null-string? word) (print-dictionary book)]
        [else (auto-complete (substring word 1) (vector-ref (dictionary-branches book) (get-index (string-ref word 0))))]))

(define (suggest-word word table)
  (cond [(null? table) '()]
        [else
         (let ([w (car table)])
           (cond [(string=? (car w) word) (print-table (cdr w))]
                 [else
                  (suggest-word word (cdr table))]))]))

(define (main word)
  (cond [(char-whitespace? (string-ref word (- (string-length word) 1))) (suggest-word (car (string-split word)) my-table)]
        [else (auto-complete word my-dictionary)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (list word)
   (let* ([a (type-1 word)]
          [b (type-2 word)]
          [c (type-3 word)]
          [d (type-4 word)]
          [e (add-ranks (append a b c d))])
     (map (lambda (c) (car c)) (vector->list (sort-words (list->vector e))))))
  )

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
    (if (= 1 (length l)) ans
        (type-4-helper '() (car l) (cadr l) (cddr l) '()))))

(define (add-ranks words)
  (define (refine l ans)
    (if (null? l) ans
        (let* ([a (car l)]
               [b (if (eq? #f (cdr a)) ans (cons a ans))])
          (refine (cdr l) b))))
  (refine (map (lambda (w) (cons w (correct? w my-dictionary))) words) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(open-graphics)

(define (launch-module)
  (let ([vp (open-viewport "Text Module" 500 500)])
    (begin
      ((draw-rectangle vp) (make-posn 50 50) 400 20 "black")
      (start-first-step vp))))

(define (start-first-step vp)
  (let* ([c (get-mouse-click vp)]
         [p (mouse-click-posn c)]
         [x (posn-x p)]
         [y (posn-y p)])
    (if (and (> x 50) (< x 450) (> y 50) (< y 70))
        (begin
          ((draw-rectangle vp) (make-posn 50 50) 400 20 "green")
          (start-second-step vp "" ""))
        (start-first-step vp))))

(define (start-second-step vp sent wor)
  (let* ([c (get-key-press vp)]
         [v (key-value c)])
    (if (char? v)
        (begin
          (cond [(char-upper-case? v) (set! v (char-downcase v))])
          (cond [(char=? v #\.) (begin (clear-text-box vp) (add-to-repository
                                                            (string-append sent (string-append " " wor))
                                                            my-dictionary my-table)
                                       (start-second-step vp "" ""))]
                [(char-blank? v) (begin ((draw-solid-rectangle vp) (make-posn 50 75) 400 400 "white")
                                        (display-list vp (suggest-word wor my-table) 1) (set! sent (string-append sent (string-append " " wor)))
                                        (display-sent vp sent) (start-second-step vp sent ""))]
                [else (begin (set! wor (string-append wor (string v))) ((draw-solid-rectangle vp) (make-posn 50 75) 400 400 "white")
                             (display-list vp (auto-complete wor my-dictionary) 1)
                             (display-sent vp (string-append sent (string-append " " wor))) (start-second-step vp sent wor))]))
        (start-second-step vp sent wor))))

(define (clear-text-box vp)
  ((draw-solid-rectangle vp) (make-posn 50 50) 400 20 "white")
  ((draw-rectangle vp) (make-posn 50 50) 400 20 "green"))
        
(define (display-list vp l n)
    (cond [(and (not (null? l)) (<= n 10))
           (begin
             ((draw-string vp) (make-posn 50 (+ (* n 30) 60)) (car l))
             (display-list vp (cdr l) (+ n 1)))]))

(define (display-sent vp sent)
  ((draw-string vp) (make-posn 52 65) sent))



