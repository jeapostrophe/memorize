#lang racket/base
(require racket/match
         struct-define
         lux
         raart
         "lib.rkt")

(define (string-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(define (render-card res mode)
  (match-define (cons ref content) res)
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

(define (memorize1 the-db)
  (let/ec next
    (define-values (save-db! lost-indexes cloze)
      (read-db the-db))

    (define base (word #:fps 0.0 #:label "Memorize" #:return #t))
    (define (read-word idx->guess before current)
      (match current
        ['()
         (word/rec
          this base
          #:output (render-card (cloze idx->guess) 'reveal)
          #:event
          (match-lambda
            ["C-M" (save-db! idx->guess)
                   (next #t)]
            ["C-C" (next #f)]
            [_ this]))]
        [(cons this-idx afterN)
         (word/rec
          this base
          #:output (render-card (cloze idx->guess) this-idx)
          #:return idx->guess
          #:event
          (match-lambda
            ["["
             (match before
               [(cons before1 beforeN)
                (read-word idx->guess beforeN (cons before1 current))]
               ['()
                (read-word idx->guess before current)])]
            [(or "]" "C-M")
             (read-word idx->guess (cons this-idx before) afterN)]
            ["C-C" (next #f)]
            ;; XXX make this less brittle and more about editting something
            [(? string-char? s)
             (read-word
              (hash-update idx->guess this-idx
                           (λ (old)
                             (string-append old s))
                           "")
              before current)]
            ["<backspace>"
             (read-word
              (hash-update idx->guess this-idx
                           (λ (old)
                             (substring old 0
                                        (max 0 (sub1 (string-length old)))))
                           "")
              before current)]
            [_ this]))]))

    (fiat-lux (read-word (hasheq) '() (sort lost-indexes <=)))))

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
