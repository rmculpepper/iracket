#lang racket/base

;; Jupyter kernel for Racket
;; You probably don't want to run this directly.

(require racket/list
         racket/contract
         racket/sandbox
         "private/iracket-execute.rkt"
         "private/iracket-connect.rkt"
         "private/iracket-kernel-info.rkt"
         "private/iracket-comm-info.rkt"
         "private/iracket-complete.rkt"
         (prefix-in ipy: "private/ipython-message.rkt")
         (prefix-in ipy: "private/ipython-services.rkt")
         (prefix-in ipy: "private/ipython.rkt"))

(provide main)

(define (main config-file-path)
  ;; ipython hides stdout, but prints stderr, so this is for debugging
  (current-output-port (current-error-port))
  (display "Kernel starting.\n")
  (define cfg (with-input-from-file config-file-path ipy:read-config))
  (parameterize ([ipy:connection-key (ipy:config-key cfg)]
                 [sandbox-eval-limits (list #f #f)]
                 [sandbox-memory-limit #f]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-namespace-specs (list sandbox-make-namespace 'file/convertible)]
                 [sandbox-path-permissions (list (list 'read "/"))])
    (define e (make-evaluator '(begin)))
    (ipy:call-with-services cfg (λ (services) (work cfg services e))))
  (display "Kernel terminating.\n"))

(define (work cfg services e)
  (define handlers (create-handlers cfg services e))
  (let loop ()
    (define-values (msg respond-to) (ipy:receive-request services))
    (ipy:send-status services (ipy:message-header msg) 'busy)
    (define-values (response shutdown?) (handle handlers msg))
    (ipy:send-response services respond-to response)
    (ipy:send-status services (ipy:message-header msg) 'idle)
    (unless shutdown? (loop))))

(struct handlers
  (execute
   complete
   connect
   kernel-info
   is-complete-request
   comm-info
   shutdown)
  #:transparent)

(define (create-handlers cfg services e)
  (handlers
   (make-execute services e)
   (λ (msg) (complete e msg))
   (λ (_msg) (connect cfg))
   (λ (_msg) kernel-info)
   (λ (msg) (is-complete-request e msg))
   (λ (_msg) comm-info)
   (λ (_msg) (hasheq 'restart #f))))

(define (handle handlers msg)
  (define msg-type (ipy:header-msg-type (ipy:message-header msg)))
  (define handler
    (case msg-type
      [(kernel_info_request) handlers-kernel-info]
      [(comm_info_request) handlers-comm-info]
      [(connect_request) handlers-connect]
      [(execute_request) handlers-execute]
      [(is_complete_request) handlers-is-complete-request]
      [(complete_request) handlers-complete]
      [(shutdown_request) handlers-shutdown]
      [else
       ;; (eprintf "unknown message type: ~a\n" msg-type)
       #f]))
  (values (and handler (ipy:make-response msg ((handler handlers) msg)))
          (eq? 'shutdown_request msg-type)))
