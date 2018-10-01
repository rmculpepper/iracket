#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         racket/pretty
         racket/port
         xml
         file/convertible
         (for-syntax racket/base)
         json
         net/base64
         (prefix-in ipy: "ipython-message.rkt")
         (prefix-in ipy: "ipython-services.rkt"))

(provide make-execute)


;; execute_request
(define (make-display-text v)
  (cons 'text/plain (format "~v" v)))

(define (make-display-html v)
  (define-values (pin pout) (make-pipe-with-specials))
  (define (size-hook v d? out)
    (cond [(and (convertible? v) (convert v 'png-bytes)) 1]
          [else #f]))
  (define (print-hook v d? out)
    (define png-data (convert v 'png-bytes))
    (define img-src (format "data:image/png;base64,~a" (base64-encode png-data)))
    (define img-style
      "display: inline; vertical-align: baseline; padding: 0pt; margin: 0pt; border: 0pt")
    (write-special `(img ((style ,img-style) (src ,img-src))) out))
  (parameterize ((pretty-print-columns 'infinity)
                 (pretty-print-size-hook size-hook)
                 (pretty-print-print-hook print-hook))
    (pretty-print v pout))
  (close-output-port pout)
  ;; read-contents : InputPort[Special] -> (Listof (U String Special))
  (define (read-contents in)
    (define buf (make-bytes 1000))
    (define (bs->string bs)
      (bytes->string/utf-8 (apply bytes-append (reverse bs))))
    (define (loop acc bs)
      (define next (read-bytes-avail! buf in))
      (cond [(eof-object? next)
             (let ([acc (cons (bs->string bs) acc)])
               (reverse acc))]
            [(exact-positive-integer? next)
             (loop acc (cons (subbytes buf 0 next) bs))]
            [(procedure? next)
             (let ([acc (cons (bs->string bs) acc)])
               (loop (cons (next #f #f #f #f) acc) null))]))
    (loop null null))
  (cons 'text/html (xexpr->string `(code ,@(read-contents pin)))))

(define (make-display-convertible conversion-type mime-type v
                                  #:encode [encode values])
  (define result (and (convertible? v) (convert v conversion-type)))
  (if result
      (cons mime-type (bytes->string/latin-1 (encode result)))
      #f))

(define (make-display-c3 v)
  (match v
    [`(c3-data . ,d) (cons 'application/x-c3-data (jsexpr->string d))]
    [else #f]))

(define (make-display-results v)
  (filter values
          (list (make-display-c3 v)
                (make-display-html v)
                (make-display-text v))))

(define (make-kill-thread/custodian cust)
  (λ (t)
    (parameterize ([current-custodian cust])
      (kill-thread t))))

;;
;(define/contract (input-reply msg)
 ; (any/c ipy:message? . -> . jsexpr?)
  ;(define code (hash-ref (ipy:message-content msg) 'value))
  ;(+ 1 2)
  ;)

(define (make-execute services e)
  (define execution-count 0)
  (define user-cust (get-user-custodian e))
  ;; let other cells' threads be killed
  (call-in-sandbox-context e
   (λ ()
     (eval
      `(define notebook-kill-thread
         ,(make-kill-thread/custodian user-cust)))))
  (λ (msg)
    (set! execution-count (add1 execution-count))
    (define code (hash-ref (ipy:message-content msg) 'code))
    (define allow-stdin (hash-ref (ipy:message-content msg) 'allow_stdin))
    (call-in-sandbox-context e
     (λ ()
       (when allow-stdin (current-input-port (ipy:make-stdin-port services msg)))
       (current-output-port (ipy:make-stream-port services 'stdout msg))
       (current-error-port (ipy:make-stream-port services 'stderr msg))))
    (call-with-values
     (λ () (e code))
     (λ vs
       (match vs
         [(list (? void?)) void]
         [else (for ([v (in-list vs)])
                 (define results (make-display-results v))
                 (ipy:send-exec-result msg services execution-count
                                       (make-hasheq results)))])))
    (hasheq
     'status "ok"
     'execution_count execution-count
     'user_expressions (hasheq))))
