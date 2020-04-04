#lang racket/base

;; Jupyter kernel for Racket
;; You probably don't want to run this directly.

(require racket/list
         racket/string
         racket/contract
         racket/sandbox
         "private/kernel.rkt"
         "private/jupyter.rkt")

(define (start-kernel config-file-path lang-line)
  ;; Jupyter hides stdout, but prints stderr, so use eprintf for debugging.
  (eprintf "Kernel starting.\n")
  (define cfg (with-input-from-file config-file-path read-config))
  (define evaluator (call-with-kernel-sandbox-configuration make-racket-evaluator lang-line))
  (run-kernel cfg
              (lambda (services)
                (hasheq 'kernel_info_request (lambda (msg) kernel-info)
                        'execute_request ((make-execute evaluator) services)
                        'complete_request (lambda (msg) (complete evaluator msg)))))
  (eprintf "Kernel terminating.\n"))

;; The default kernel sandbox configuration should be less restrictive than the
;; default (for example, no time/memory limits) and more restrictive than the
;; "trusted" configuration (for example, we do want to wrap the exit handler, we
;; probably want some security guard checks by default, etc; cf
;; call-with-trusted-sandbox-configuration).
(define (call-with-kernel-sandbox-configuration proc lang-line)
  (parameterize (;; -- Same as default:
                 ;; [sandbox-propagate-breaks #t]
                 ;; [sandbox-override-collection-paths '()]
                 ;; [sandbox-make-logger current-logger]
                 ;; [sandbox-eval-handlers (list #f call-with-custodian-shutdown)]
                 ;; -- Retain default, because trusted is too relaxed:
                 ;; [sandbox-security-guard      _] ;; trusted = current-security-guard
                 ;; [sandbox-exit-handler        _] ;; trusted = (exit-handler)
                 ;; [sandbox-make-inspector      _] ;; trusted = current-inspector
                 ;; [sandbox-make-code-inspector _] ;; trusted = current-code-inspector
                 ;; [sandbox-make-plumber 'propagate] ;; trusted = current-plumber
                 ;; [sandbox-make-environment-variables *copy*] ;; trusted = current-environment-variables
                 ;; -- Same as trusted:
                 [sandbox-memory-limit  #f] ;; default = 30 (MB)
                 [sandbox-eval-limits   #f] ;; default = '(30 20) (sec,MB)
                 ;; -- Not set by call/trusted:
                 [sandbox-gui-available #f] ;; GUI makes no sense for Jupyter kernel
                 [sandbox-propagate-exceptions #f] ;; default = #t -- FIXME?
                 [sandbox-namespace-specs (cons sandbox-make-namespace '(file/convertible))]
                 [sandbox-path-permissions '((read "/"))])
    (proc lang-line)))

(define (make-racket-evaluator lang-line)
  (define-values (meta-lang lang)
    (apply values (let* ([slist (string-split lang-line)]
                         [lenl (length slist)])
                        (cond [(= lenl 0) (list null 'racket)]
                              [(= lenl 1) (if (equal? (car slist) "#lang")
                                              (list null 'racket)
                                              (list null (string->symbol (car slist))))]
                              [(= lenl 2) (if (equal? (car slist) "#lang")
                                              (list null (string->symbol (cadr slist)))
                                              (raise-syntax-error 'bad-lang
                                                                  (format "bad lang line ~a" lang-line)))]
                              [(= lenl 3) (if (equal? (car slist) "#lang")
                                              (list (string->symbol (cadr slist)) (string->symbol (caddr slist)))
                                              (raise-syntax-error 'bad-lang
                                                                  (format "bad lang line ~a" lang-line)))]
                              [else (raise-syntax-error 'bad-lang (format "bad lang line ~a" lang-line))]))))
  (make-evaluator lang))


;; ============================================================
;; New kernel invocation interface

(module+ main
  (require racket/cmdline)
  (command-line
   #:args (config-file-path  lang-line)
   (start-kernel config-file-path lang-line)))

;; ============================================================
;; Old kernel invocation interface

(provide main)
(define (main config-file-path lang-line)
  (eprintf "Notice: IRacket kernel started through old interface.\n")
  (start-kernel config-file-path lang-line))
