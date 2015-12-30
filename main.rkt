#lang racket/base
(require racket/string
         racket/file
         racket/list
         racket/match
         web-server/dispatch
         web-server/http
         pkg-index/official/jsonp
         xml
         json)

(define (make-memorize-http dir)
  (define db.rktd (build-path dir "db.rktd"))

  (define source.rktd (build-path dir "source.rktd"))
  (define source (list->vector (file->value source.rktd)))

  (define (get-word-count verses)
    (define word-count 0)
    (define word-verses
      (for/list ([v (in-list verses)])
        (define ws (string-split v " "))
        (set! word-count (+ word-count (length ws)))
        ws))
    (values word-count word-verses))
  
  (struct db-entry (card score wc) #:prefab)
  (define (db-entry<= x y)
    (match-define (db-entry _ xs xwc) x)
    (match-define (db-entry _ ys ywc) y)
    (define xc (/ xs xwc))
    (define yc (/ ys ywc))

    (cond
      ;; If something is complete, then put it at the end with high
      ;; probability
      [(= xc 1)
       (> (random) 0.1)]
      [(= yc 1)
       (< (random) 0.9)]
      ;; When they are close in length, sort by completion
      [(< (abs (- xwc ywc)) 5)
       (<= xc yc)]
      ;; If not, then if it is shorter, then put it first
      [else
       (<= xwc ywc)]))

  (define current-entry #f)
  (define other-entries #f)

  (define (current-modify-score! f)
    (match-define (db-entry card score wc) current-entry)
    (set! current-entry
          (struct-copy db-entry current-entry
                       [score (min wc (max 0 (f score)))]))
    (save-db!))

  (define (sort-db db-l)
    (sort db-l db-entry<=))
  (define (save-db!)
    (write-to-file
     (sort-db (cons current-entry other-entries))
     db.rktd
     #:exists 'replace)
    (load-db!))

  (define (load-db!)
    (define r
      (cond
        [(file-exists? db.rktd)
         (file->value db.rktd)]
        [else
         (sort-db
          (for/list ([i (in-naturals)]
                     [s (in-vector source)])
            (match-define (list reference verses) s)
            (define-values (wc _) (get-word-count verses))
            (db-entry i 0 wc)))]))

    (set! current-entry (first r))
    (set! other-entries (rest r)))
  (load-db!)

  (define (current-format)
    (match-define (db-entry card score _) current-entry)
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

  (define (click! which?)
    (current-modify-score! (if which? add1 sub1))
    #t)

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
