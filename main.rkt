#lang racket/base
(require racket/string
         racket/file
         racket/list
         racket/format
         racket/match
         struct-define
         lux
         raart)

(define NEL (string #\u0085))

(define (clamp m x M)
  (min M (max m x)))

(define (string-pad s w c)
  (string-append (substring s 0 (min (string-length s) w))
                 (make-string (max 0 (- w (string-length s))) c)))
(define-syntax-rule (implies p q)
  (or (not p) q))
(define (string-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(struct card (sc ref vs))

(define (memorizeN the-db to-do)
  (let/ec quit
    (for ([completed (in-range to-do)])
      (let/ec next
        (define unsorted
          (for/list ([l (in-list (file->lines the-db))])
            (match-define (list (app string->number sc) ref vs) (string-split l "\t"))
            (card sc ref vs)))
        (define sorted (sort unsorted >= #:key card-sc))
        (match-define (cons (card score ref vs) after) sorted)

        (define (save! success?)
          (define adj (if success? sub1 add1))
          (define new-sorted
            (sort (cons (card (adj score) ref vs) after)
                  >= #:key card-sc))
          (with-output-to-file the-db
            #:exists 'replace
            (λ ()
              (for ([c (in-list new-sorted)])
                (match-define (card sc ref vs) c)
                (printf "~a\t~a\t~a\n" sc ref vs))))

          (next))

        (define verses (string-split vs NEL))
        (define verses-words (map (λ (v) (string-split v " ")) verses))
        (define word-count (apply + (map length verses-words)))
        (define word-indexes (range word-count))
        (define shuffled-indexes (shuffle word-indexes))
        (define how-many-blank (clamp 0 (- word-count score) word-count))
        (define lost-indexes (take shuffled-indexes how-many-blank))

        (define base (word #:fps 0.0 #:label "Memorize" #:return #t))

        (define (correct? idx->word)
          (define idx -1)
          (for/and ([words (in-list verses-words)])
            (for/and ([w (in-list words)])
              (set! idx (add1 idx))
              (implies (memq idx lost-indexes)
                       (equal? w (hash-ref idx->word idx))))))
        (define (render-card idx->word mode)
          (define idx -1)
          (vappend*
           #:halign 'left
           (list*
            (style (if (eq? mode 'reveal) 'bold 'normal) (text ref))
            (for/list ([words (in-list verses-words)])
              (vappend2
               #:halign 'center
               (para* 80
                      (for/list ([w (in-list words)])
                        (set! idx (add1 idx))
                        (cond
                          [(memq idx lost-indexes)
                           (define guess (hash-ref idx->word idx #f))
                           (with-drawing
                             (if (eq? mode idx)
                               'inverse
                               'bold)
                             (and (eq? mode 'reveal)
                                  (if (equal? w guess)
                                    'green
                                    'red))
                             #f
                             (text
                              (cond
                                [(eq? mode 'reveal)
                                 w]
                                [guess
                                 ;; XXX don't hide and force typing of punctuation
                                 (string-pad guess (string-length w) #\-)]
                                [else
                                 (regexp-replace* #px"\\w" w "-")])))]
                          [else
                           (text w)])))
               (blank))))))

        (define (read-word idx->word this-idx)
          (define current
            (word base
                  #:output (render-card idx->word this-idx)
                  #:return idx->word
                  #:event
                  (match-lambda
                    ["C-M" #f]
                    ["C-C" (quit)]
                    ;; XXX make this less brittle and more about editting something
                    [(? string-char? s)
                     (read-word
                      (hash-update idx->word this-idx
                                   (λ (old)
                                     (string-append old s)))
                      this-idx)]
                    ["<backspace>"
                     (read-word
                      (hash-update idx->word this-idx
                                   (λ (old)
                                     (substring old 0
                                                (max 0 (sub1 (string-length old))))))
                      this-idx)]
                    [_ current])))
          current)
        (define (check-words idx->word)
          (define revealed
            (word base
                  #:output (render-card idx->word 'reveal)
                  #:event
                  (match-lambda
                    ["C-M" (save! (correct? idx->word))]
                    ["C-C" (quit)]
                    [_ revealed])))
          revealed)

        (define final-idx->word
          (for/fold ([idx->word (hasheq)]) ([this-idx (in-list (sort lost-indexes <=))])
            (fiat-lux (read-word (hash-set idx->word this-idx "") this-idx))))
        (fiat-lux (check-words final-idx->word))))))

(module+ main
  (require racket/cmdline
           raart/lux-chaos)
  (define (go! db how-many)
    (call-with-chaos
     (make-raart)
     (λ () (memorizeN db how-many))))
  (command-line #:program "memorize"
                #:args (db how-many)
                (go! db (string->number how-many))
                (void)))
