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

(struct card (sc ref vs))

(define (make-memorize the-db completed)
  (define unsorted
    (for/list ([l (in-list (file->lines the-db))])
      (match-define (list (app string->number sc) ref vs) (string-split l "\t"))
      (card sc ref vs)))
  (define sorted (sort unsorted >= #:key card-sc))
  (match-define (cons next after) sorted)
  
  (match-define (card score ref vs) next)
  (define verses (string-split vs NEL))
  (define verses-words (map (λ (v) (string-split v " ")) verses))
  (define word-count (apply + (map length verses-words)))
  (define word-indexes (range word-count))
  (define shuffled-indexes (shuffle word-indexes))
  (define how-many-blank (clamp 0 (- word-count score) word-count))
  (define lost-indexes (take shuffled-indexes how-many-blank))

  (define (render-card reveal?)
    (define idx -1)
    (vappend*
     #:halign 'left
     (list* (style 'inverse (text (~a "Completed " completed)))
            (blank)
            (style (if reveal? 'bold 'normal) (text ref))
            (for/list ([words (in-list verses-words)])
              (vappend2 #:halign 'center
                        (para* 80
                               (for/list ([w (in-list words)])
                                 (set! idx (add1 idx))
                                 (if (memq idx lost-indexes)
                                   (if reveal?
                                     (style 'bold (text w))
                                     (text (regexp-replace* #px"\\w" w "-")))
                                   (text w))))
                        (blank))))))  

  (define base (word #:fps 0.0 #:label "Memorize" #:return (void)))
  (define hidden
    (word base
          #:output (render-card #f)
          #:event
          (match-lambda
            [" " revealed]
            ["q" #f]
            [_ hidden])))
  (define revealed
    (word base
          #:output (render-card #t)
          #:event
          (match-lambda
            ["<left>" (save! #f)]
            ["<right>" (save! #t)]
            ["q" #f]
            [_ revealed])))

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

    (make-memorize the-db (add1 completed)))

  hidden)

(module+ main
  (require racket/cmdline
           raart/lux-chaos)
  (define the-db (command-line #:program "memorize" #:args (db) db))
  (call-with-chaos
   (make-raart)
   (λ ()
     (fiat-lux (make-memorize the-db 0)))))
