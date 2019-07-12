#lang racket/base
(require racket/match
         struct-define
         lux
         raart
         "lib.rkt")

(define (string-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(define (memorize1 the-db)
  (let/ec next
    (define-values (save-db! lost-indexes cloze)
      (read-db the-db))

    (define base (word #:fps 0.0 #:label "Memorize" #:return #t))

    (define (render-card idx->word mode)
      (define-values (ref content) (cloze idx->word))
      (vappend*
       #:halign 'left
       (list*
        (style (if (eq? mode 'reveal) 'bold 'normal) (text ref))
        (for/list ([words (in-list content)])
          (vappend2
           #:halign 'center
           (para* 80
                  (for/list ([cnt (in-list words)])
                    (match-define (vector idx w g mcloze) cnt)
                    (happend
                     (cond
                       [mcloze
                        (with-drawing
                          (if (eq? mode idx)
                            'inverse
                            'bold)
                          (and (eq? mode 'reveal)
                               (if (equal? w mcloze)
                                 'green
                                 'red))
                          #f
                          (text
                           (if (eq? mode 'reveal)
                             w
                             mcloze)))]
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
