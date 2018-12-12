#lang racket/base
(require racket/string
         racket/file
         racket/list
         racket/match)

(define (get-word-count verses)
  (define word-count 0)
  (define word-verses
    (for/list ([v (in-list verses)])
      (define ws (string-split v " "))
      (set! word-count (+ word-count (length ws)))
      ws))
  (values word-count word-verses))

(struct db-entry1 (card score wc ts) #:prefab)
(define (db-entry<= x y)
  (define (proj x)
    (match-define (db-entry1 _ xs xwc xts) x)
    (define xc (/ xs xwc))
    (define same-day?
      (<= (abs (- xts (current-seconds)))
          (* 24 60 60)))
    (cond
      [(or same-day? (= xc 1))
       ;; The time is big, so we put it later
       xts]
      [else
       ;; This puts things with the same word count in the same spot
       ;; and then the fractional part is the completion, so less
       ;; complete things come first.
       (+ xwc xc)]))
  (<= (proj x) (proj y)))

(define (make-memorize-http dir)
  (define db.rktd (build-path dir "db.rktd"))

  (define source.rktd (build-path dir "source.rktd"))
  (define source (list->vector (file->value source.rktd)))

  (define current-entry #f)
  (define other-entries #f)

  (define (sort-db db-l)
    (sort db-l db-entry<=))
  (define (write-db!)
    (write-to-file
     (sort-db (cons current-entry other-entries))
     db.rktd
     #:exists 'replace))
  (define (save-db!)
    (write-db!)
    (load-db!))

  (define (load-db!)
    (define old
      (if (file-exists? db.rktd)
        (file->value db.rktd)
        empty))
    (define old-i->score*ts
      (let ()
        (struct db-entry (card score wc) #:prefab)
        (for/hasheq ([de (in-list old)])
          (match de
            [(db-entry c s _)
             (values c (cons s 0))]
            [(db-entry1 card score _ ts)
             (values card (cons score ts))]))))

    (define r
      (sort-db
       (for/list ([i (in-naturals)]
                  [s (in-vector source)])
         (match-define (list reference verses) s)
         (define-values (wc _) (get-word-count verses))
         (define-values (old-score old-ts)
           (match (hash-ref old-i->score*ts i #f)
             [(cons s t) (values s t)]
             [#f (values 0 0)]))
         (db-entry1 i old-score wc old-ts))))

    (set! current-entry (first r))
    (set! other-entries (rest r))

    (write-db!))
  (load-db!)

  (define (current-format)
    (match-define (db-entry1 card score _ _) current-entry)
    (match-define (list reference verses)
      (vector-ref source card))
    (define-values (word-count word-verses)
      (get-word-count verses))
    (define word-indexes
      (build-list word-count (λ (x) x)))
    (define shuffled-indexes
      (shuffle word-indexes))
    (define lost-indexes
      (take shuffled-indexes
            (max 0 (min word-count score))))
    (define (full lose?)
      (define this-count 0)
      `(div
        (p ([class "reference"]) ,reference)
        ,@(for/list ([wv (in-list word-verses)])
            (begin0
                `(p ([class "verse"])
                    ,@(add-between
                       (for/list ([w (in-list wv)]
                                  [i (in-naturals this-count)])
                         (if (memq i lost-indexes)
                           (if lose?
                             (cdata #f #f
                                    (string-append*
                                     (map (λ (c)
                                            (if (char-alphabetic? c)
                                              "&#8209;"
                                              (string c)))
                                          (string->list w))))
                             `(span ([class "lost"]) ,w))
                           w))
                       " "))
              (set! this-count
                    (+ this-count
                       (length wv)))))))
    (values (full #t)
            (full #f)))
  (define (click! success?)
    (define f (if success? add1 sub1))
    (match-define (db-entry1 card score wc ts) current-entry)
    (set! current-entry
          (struct-copy db-entry1 current-entry
                       [ts (current-seconds)]
                       [score (min wc (max 0 (f score)))]))
    (save-db!)
    #t)

  (for ([e (in-list (sort-db (cons current-entry other-entries)))])
    (match-define (db-entry1 card score wc ts) e)
    (match-define (list reference verses) (vector-ref source card))
    (printf "~a\t~a\t~a\n"
            (- wc score)
            reference
            (string-join verses (string #\u0085))))

  (local-require web-server/dispatch
                 pkg-index/official/jsonp
                 (only-in xml cdata xexpr->string))
  (define-jsonp (api/next)
    (define-values (front back) (current-format))
    (hash 'front (xexpr->string front)
          'back (xexpr->string back)))
  (define-jsonp (api/click/no)
    (click! #f))
  (define-jsonp (api/click/yes)
    (click! #t))

  (define-values (mem-dispatch mem-url)
    (dispatch-rules
     [("api" "next") api/next]
     [("api" "click" "no") api/click/no]
     [("api" "click" "yes") api/click/yes]))

  mem-dispatch)

(module+ main
  (require racket/cmdline
           racket/runtime-path
           web-server/servlet-env)

  (define-runtime-path static "static")

  (command-line
   #:program "memorize"
   #:args (dir)
   (serve/servlet (make-memorize-http dir)
                  #:launch-browser? #f
                  #:servlet-regexp #rx""
                  #:extra-files-paths (list static)
                  #:port 7332)))
