#lang racket/base
(require racket/string
         racket/file
         racket/list
         racket/match)
(provide read-db)

(struct card (sc ref vs))

(define (clamp m x M)
  (min M (max m x)))

(define-syntax-rule (implies p q)
  (or (not p) q))

(define NEL (string #\u0085))

(define (read-db the-db)
  (define unsorted
    (for/list ([l (in-list (file->lines the-db))])
      (match-define (list (app string->number sc) ref vs) (string-split l "\t"))
      (card sc ref vs)))
  (match-define (cons (card score ref vs) after)
    (sort unsorted >= #:key card-sc))

  (define (save-db! idx->word)
    (define success? (correct? idx->word))
    (define adj (if success? sub1 add1))
    (define new-sorted
      (sort (cons (card (adj score) ref vs) after) >= #:key card-sc))
    (with-output-to-file the-db
      #:exists 'replace
      (λ ()
        (for ([c (in-list new-sorted)])
          (match-define (card sc ref vs) c)
          (printf "~a\t~a\t~a\n" sc ref vs)))))

  (define verses (string-split vs NEL))
  (define verses-words
    (for/list ([v (in-list verses)])
      (define len (string-length v))
      (define ps (regexp-match-positions* #px"\\W+" v))
      (let loop ([i 0] [ps ps])
        (match ps
          [`()
           (cond [(= i len) `()]
                 [else (list (cons (substring v i len) ""))])]
          [(cons (cons start end) ps)
           (cons
            (cons (substring v i start)
                  (substring v start end))
            (loop end ps))]))))
  (define word-count (apply + (map length verses-words)))
  (define word-indexes (range word-count))
  (define shuffled-indexes (shuffle word-indexes))
  (define how-many-blank (clamp 0 (- word-count score) word-count))
  (define lost-indexes (take shuffled-indexes how-many-blank))

  (define (correct? idx->word)
    (define idx -1)
    (for/and ([words (in-list verses-words)])
      (for/and ([w*g (in-list words)])
        (match-define (cons w g) w*g)
        (set! idx (add1 idx))
        (implies (memq idx lost-indexes)
                 (equal? w (hash-ref idx->word idx))))))

  (values
   save-db!
   ref
   verses-words
   lost-indexes))
