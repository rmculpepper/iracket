#lang racket/base

(require racket/string
         racket/match
         racket/contract
         racket/sandbox
         json
         net/base64
         (prefix-in ipy: "ipython-message.rkt")
         (prefix-in ipy: "ipython-services.rkt")
         (prefix-in ipy: "ipython.rkt"))

(provide complete is-complete-request)

;; complete_request
(define/contract (string-prefix? prefix word)
  (string? string? . -> . boolean?)
  (define len (string-length prefix))
  (equal? prefix (substring word 0 (min (string-length word) len))))

;; TODO
(define/contract (is-complete-request e msg)
  (any/c ipy:message? . -> . jsexpr?)
  (define code (hash-ref (ipy:message-content msg) 'code))
  (hasheq
   'status "unknown"
   ))

(define/contract (complete e msg)
  (any/c ipy:message? . -> . jsexpr?)
  (define code (hash-ref (ipy:message-content msg) 'code))
  (define cursor-pos (hash-ref (ipy:message-content msg) 'cursor_pos))
  (define prefix (car (regexp-match #px"[^\\s,)(]*$" code 0 cursor-pos)))
  (define suffix (car (regexp-match #px"^[^\\s,)(]*" code (sub1 cursor-pos))))
  (define words (call-in-sandbox-context e namespace-mapped-symbols))
  (define matches
    (sort (filter (λ (w) (string-prefix? prefix w))
                  (map symbol->string words))
          string<=?))
  (hasheq
   'matches matches
   'cursor_start (- cursor-pos (string-length prefix))
   'cursor_end (+ cursor-pos (string-length suffix) -1)
   'status "ok"))
