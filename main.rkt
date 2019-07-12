#lang racket/base
(require racket/match
         struct-define
         lux
         raart
         "lib.rkt")

(define (string-pad s w c)
  (string-append (substring s 0 (min (string-length s) w))
                 (make-string (max 0 (- w (string-length s))) c)))
(define (string-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(define (memorize1 the-db)
  (let/ec next
    (define-values (save-db! ref verses-words lost-indexes)
      (read-db the-db))

    (define base (word #:fps 0.0 #:label "Memorize" #:return #t))

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
                  (for/list ([w*g (in-list words)])
                    (match-define (cons w g) w*g)
                    (set! idx (add1 idx))
                    (happend
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
                        (text w)])
                     (text g))))
           (blank))))))

    (define (read-word idx->word this-idx)
      (define current
        (word base
              #:output (render-card idx->word this-idx)
              #:return idx->word
              #:event
              ;; XXX be able to go back a word
              (match-lambda
                ["C-M" #f]
                ["C-C" (next #f)]
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
                ["C-M"
                 (save-db! idx->word)
                 (next #t)]
                ["C-C" (next #f)]
                [_ revealed])))
      revealed)

    (define final-idx->word
      (for/fold ([idx->word (hasheq)]) ([this-idx (in-list (sort lost-indexes <=))])
        (fiat-lux (read-word (hash-set idx->word this-idx "") this-idx))))
    (fiat-lux (check-words final-idx->word))))

(define (memorizeN the-db to-do)
  (for/and ([completed (in-range to-do)])
    (memorize1 the-db)))

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
