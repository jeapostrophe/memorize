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
  (define the-db (file->value db.rktd))
  (define (current-front)
    (first (first the-db)))
  (define (current-back)
    (xexpr->string
     `(div
       ,@(for/list ([v (in-list (second (first the-db)))])
           `(p ,v)))))

  (define (click! which?)
    ;; XXX
    #t)

  (define-jsonp (api/next)
    (hash 'front (current-front)
          'back (current-back)))
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
