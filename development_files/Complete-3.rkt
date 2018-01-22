#lang racket
(require "data.rkt")
(require graphics)
(struct dictionary (word frequency branches) #:transparent)
(define my-dictionary (dictionary "" 0 (make-vector 26 #f)))
(define my-table (list ))
(define (null-string? s) (equal? s ""))

(define (get-index c)
  (cond [(char-ci=? c #\a) 00] [(char-ci=? c #\b) 01] [(char-ci=? c #\c) 02] [(char-ci=? c #\d) 03]
        [(char-ci=? c #\e) 04] [(char-ci=? c #\f) 05] [(char-ci=? c #\g) 06] [(char-ci=? c #\h) 07]
        [(char-ci=? c #\i) 08] [(char-ci=? c #\j) 09] [(char-ci=? c #\k) 10] [(char-ci=? c #\l) 11]
        [(char-ci=? c #\m) 12] [(char-ci=? c #\n) 13] [(char-ci=? c #\o) 14] [(char-ci=? c #\p) 15]
        [(char-ci=? c #\q) 16] [(char-ci=? c #\r) 17] [(char-ci=? c #\s) 18] [(char-ci=? c #\t) 19]
        [(char-ci=? c #\u) 20] [(char-ci=? c #\v) 21] [(char-ci=? c #\w) 22] [(char-ci=? c #\x) 23]
        [(char-ci=? c #\y) 24] [(char-ci=? c #\z) 25]))

(define (calc-num word)
  (let ([n (string-length word)])
    (+ 1 (quotient n 5))))

(define (remove-dot word)
  (list->string (remove #\. (string->list word))))

(define (capitalise l)
  (map (lambda (w) (string-titlecase w)) l))

(define (del i l)
  (if (= i 0)
      (cdr l)
      (cons (car l) (del (- i 1) (cdr l)))))

(define (member? a l)
  (cond [(null? l) #f]
        [(equal? a (car l)) #t]
        [else (member? a (cdr l))]))

(define (single-out l)
  (define (single-out-helper li ans)
    (if (null? li)
        ans
        (if (null-string? (car li))
            (single-out-helper (cdr li) ans)
            (if (member? (car li) ans)
                (single-out-helper (cdr li) ans)
                (single-out-helper (cdr li) (append ans (list (car li))))))))
    (single-out-helper l '()))

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
  (begin
    (set! word (remove-dot word))
    (set! word (string-downcase word))
    (add-to-dictionary-helper word word book)))

(define (add-to-dictionary-list words book)
  (map (lambda (w) (begin (set! book (add-to-dictionary-word w book)) w)) words)
  (set! words words))

(define (add-to-table word-1 word-2 table)
  (begin
    (set! word-1 (string-downcase word-1))
    (set! word-2 (string-downcase word-2))
    (set! word-2 (remove-dot word-2))
    (let ([word-3 (remove-dot word-1)])
      (begin
        (cond [(not (equal? word-1 word-3)) (set! word-1 ".")])
        (let ([w (get-word word-1 table)])
          (cond [(eq? #f w) (insert-table (cons word-1 (list (cons word-2 1))) table)]
                [else
                 (set! table (cdr w))
                 (let ([l (cdr (car w))])
                   (set! l (vector->list (sort-words (list->vector (insert-list word-2 l)))))
                   (insert-table (cons word-1 l) table))]))))))

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
    (add-to-table-list (cons "." words) table)))

(define (auto-complete-1 word book)
  (cond [(not (dictionary? book)) '()]
        [(null-string? word) (print-dictionary book)]
        [else (auto-complete-1 (substring word 1) (vector-ref (dictionary-branches book) (get-index (string-ref word 0))))]))

(define (auto-complete word)
  (cond [(equal? #\. (string-ref word (- (string-length word) 1))) '()]
        [else (let* ([n (string-length word)]
                     [a (if (> n 3) (type-1-n word (calc-num word)) '())]
                     [b (if (> n 3) (type-2 word) '())]
                     [c (if (> n 3) (type-3 word) '())]
                     [d (if (> n 3) (type-4-n word (calc-num word)) '())])
                (single-out (append (auto-complete-1 word my-dictionary) (flatten (map (lambda (l) (auto-complete-1 l my-dictionary)) (append a b c d))))))]))

(define (suggest-word-1 word table)
  (cond [(null? table) '()]
        [else
         (let ([w (car table)])
           (cond [(string=? (car w) word) (print-table (cdr w))]
                 [else
                  (suggest-word-1 word (cdr table))]))]))

(define (suggest-word word)
  (set! word (string-downcase word))
  (let ([l (if (equal? #\. (string-ref word (- (string-length word) 1))) (single-out (capitalise (suggest-word-1 "." my-table)))
               (single-out (suggest-word-1 word my-table)))])
        l))

(define (main word)
  (cond [(char-whitespace? (string-ref word (- (string-length word) 1))) (suggest-word (car (string-split word)))]
        [else (auto-complete word)]))

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
   (let* ([a (type-1-n word (calc-num word))]
          ;[b (type-2-n word (calc-num word))]
          ;[c (type-3-n word (calc-num word))]
          [d (type-4-n word (calc-num word))]
          ;[a (type-1 word)]
          [b (type-2 word)]
          [c (type-3 word)]
          ;[d (type-4 word)]
          [e (add-ranks (append a b c d))])
     (single-out (map (lambda (c) (car c)) (vector->list (sort-words (list->vector e)))))))
  )

(define (type-1-n words n)
  (cond [(not (list? words)) (set! words (list words))])
  (if (= n 0)
      words
      (let ([l (flatten (map (lambda (w) (type-1 w)) words))])
        (type-1-n (append words l) (- n 1)))))    

(define (type-1 word) ; Adds letter
  (define (type-1-helper l i n ans)
    (if (= i n)
        ans
        (type-1-helper l (+ i 1) n (cons (list->string (del i l)) ans))))
  (let* ([l (string->list word)]
         [ans '()]
         [n (length l)])
    (type-1-helper l 0 n ans)))

(define (type-2-n words n)
  (cond [(not (list? words)) (set! words (list words))])
  (if (= n 0)
      words
      (let ([l (flatten (map (lambda (w) (type-2 w)) words))])
        (type-2-n (append words l) (- n 1))))) 

(define (type-2 word) ; Deletes letter
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

(define (type-3-n words n)
  (cond [(not (list? words)) (set! words (list words))])
  (if (= n 0)
      words
      (let ([l (flatten (map (lambda (w) (type-3 w)) words))])
        (type-3-n (append words l) (- n 1))))) 

(define (type-3 word) ; Changes a letter
  (define (type-3-helper start d end ans)
    (cond [(null? end) ans]
          [(type-3-helper (append start (list (car end))) d (cdr end) (cons (list->string (append start (list d) (cdr end))) ans))]))    
  (let ([ans '()])
    (begin (map (lambda (d) (begin (set! ans (cons (type-3-helper '() d (string->list word) '()) ans)) d)) alphabets)
           (flatten ans))))

(define (type-4-n words n)
  (cond [(not (list? words)) (set! words (list words))])
  (if (= n 0)
      words
      (let ([l (flatten (map (lambda (w) (type-4 w)) words))])
        (type-4-n (append words l) (- n 1))))) 

(define (type-4 word) ; Checks swapping
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

(open-graphics)

(define (launch-module)
  (let ([vp (open-viewport "Text Package" 500 500)])
    (main-final vp "" 0)))

(define (main-final vp sent k)
  (begin
    ((draw-solid-rectangle vp) (make-posn 0 0) 500 500 "white")
    (let* ([n (display-sent vp sent)]
           [p (string-split sent)]
           [q (if (null? p) "" (list-ref p (- (length p) 1)))]
           [l (if (null-string? sent)
                  (suggest-word ".")
                  (if (char-whitespace? (string-ref sent (- (string-length sent) 1)))
                      (if (null-string? q) '() (suggest-word q))
                      (if (null-string? q) '() (auto-complete q))))])
      (begin
        (display-list vp l n 0 k)
        (let ([t (get-key vp)])
          (cond [(or (equal? t 1) (equal? t -1)) (main-final vp sent (modulo (+ k t) (length l)))]
                [(and (not (null? l)) (equal? t #\tab))
                 (begin
                   (cond
                     [(or (null-string? sent) (char-whitespace? (string-ref sent (- (string-length sent) 1))))
                      (set! sent (string-append sent (list-ref l k) " "))]
                     [else (set! sent (string-append (string-make (reverse (cons (list-ref l k) (cdr (reverse (string-split sent))))) "") " "))])
                   (main-final vp sent 0))]
                [(equal? t #\backspace) (begin
                                 (set! sent (if (equal? sent "") "" (substring sent 0 (- (string-length sent) 1))))
                                 (main-final vp sent 0))]
                [(equal? t #\return) (begin (add-to-repository sent my-dictionary my-table) (main-final vp "" 0))]
                [else (main-final vp (string-append sent (string t)) 0)]))))))

(define (string-make l ans)
  (if (null? l) ans
      (string-make (cdr l) (string-append ans " " (car l)))))

(define (get-key vp)
  (let* ([c (get-key-press vp)]
         [v (key-value c)])
    (cond [(char? v) v]
          [(equal? 'down v) 1]
          [(equal? 'up v) -1]
          [else (get-key vp)])))

(define (display-list vp l n i k)
  (cond [(and (not (null? l)) (<= i 10))
         (begin
           ((draw-string vp) (make-posn 50 (+ (* (+ i 1) 30) (+ (* 20 n) 60))) (car l) (if (= i k) "red" "black"))
           (display-list vp (cdr l) n (+ i 1) k))]))
 
(define (display-sent vp sent)
  (define (display-sent-helper vp l i)
    (cond [(not (null? l))
           (begin
             ((draw-rectangle vp) (make-posn 50 (+ 50 (* 20 i))) 400 20)
             ((draw-string vp) (make-posn 52 (+ 65 (* 20 i))) (car l) "black")
             (display-sent-helper vp (cdr l) (+ i 1)))]
          [else i]))
  (let* ([l1 (string-split sent)]
         [l2 (join-to-strings l1 '() "")])
    (display-sent-helper vp l2 0)))

(define (join-to-strings l1 ans strng)
  (cond [(null? l1) (append ans (list strng))]
        [(> (string-length (string-append strng " " (car l1))) 60)
         (join-to-strings l1 (append ans (list strng)) "")]
        [else (join-to-strings (cdr l1) ans (string-append strng " " (car l1)))]))
         













