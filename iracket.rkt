#lang racket/base

;; Jupyter kernel for Racket
;; You probably don't want to run this directly.

(require racket/list
         racket/contract
         racket/sandbox
         "private/kernel.rkt"
         "private/jupyter.rkt")

(define (start-kernel trusted-sandbox config-file-path)
  ;; Jupyter hides stdout, but prints stderr, so use eprintf for debugging.
  (eprintf "Kernel starting.\n")
  (define cfg (with-input-from-file config-file-path read-config))
  (define evaluator (if trusted-sandbox
                      (call-with-kernel-trusted-sandbox-configuration make-racket-evaluator)
                      (call-with-kernel-sandbox-configuration make-racket-evaluator)))
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
(define (call-with-kernel-sandbox-configuration proc)
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
    (proc)))

(define (call-with-kernel-trusted-sandbox-configuration proc)
  (parameterize (;; -- Same as default:
                 ;; [sandbox-propagate-breaks #t]
                 ;; [sandbox-override-collection-paths '()]
                 ;; [sandbox-make-logger current-logger]
                 ;; [sandbox-eval-handlers (list #f call-with-custodian-shutdown)]
                 ;; -- Retain default, because trusted is too relaxed:
                 [sandbox-security-guard      (current-security-guard)] 
                 ;; [sandbox-exit-handler        _] ;; trusted = (exit-handler)
                 ;; [sandbox-make-inspector      _] ;; trusted = current-inspector
                 ;; [sandbox-make-code-inspector _] ;; trusted = current-code-inspector
                 ;; [sandbox-make-plumber 'propagate] ;; trusted = current-plumber
                 ;; [sandbox-make-environment-variables *copy*] ;; trusted = current-environment-variables
                 ;; -- Same as trusted:
                 [sandbox-memory-limit  #f] ;; default = 30 (MB)
                 [sandbox-eval-limits   #f] ;; default = '(30 20) (sec,MB)
                 [sandbox-gui-available #f] ;; GUI makes no sense for Jupyter kernel
                 [sandbox-propagate-exceptions #f] ;; default = #t -- FIXME?
                 [sandbox-namespace-specs (cons sandbox-make-namespace '(file/convertible))]
                 [sandbox-path-permissions '((read "/"))])
    (proc)))

(define (make-racket-evaluator)
  (define e (make-evaluator '(begin)))
  (call-in-sandbox-context e
    (lambda ()
      ;; Goal: obtain a nearly-empty namespace with registry of sandbox's
      ;; initial namespace and with attached modules BUT that does not prevent
      ;; GC of that namespace if the sandbox's namespace is later replaced.
      (define init-ns (current-namespace))
      (define varref0 (eval '(#%variable-reference)))
      (define root-ns (variable-reference->empty-namespace varref0))
      (define mods-to-attach (cdr (sandbox-namespace-specs)))
      (define varref
        (parameterize ((current-namespace root-ns))
          (for ([mod (in-list (cdr (sandbox-namespace-specs)))])
            (namespace-attach-module init-ns mod))
          (namespace-require '(only racket/kernel #%variable-reference))
          (eval '(#%variable-reference))))
      (current-kernel-make-namespace
       (lambda (language)
         (define ns (variable-reference->empty-namespace varref))
         (parameterize ((current-namespace ns))
           (for ([mod (in-list mods-to-attach)])
             (namespace-attach-module root-ns mod))
           (namespace-require language)
           ns)))))
  e)


;; ============================================================
;; New kernel invocation interface

(module+ main
  (require racket/cmdline)
  (define trusted-sandbox (make-parameter #f))
  (command-line
   #:once-each
   [("-t" "--trusted") "Create a trusted sandbox" (trusted-sandbox #t)]
   #:args (config-file-path)
   (start-kernel (trusted-sandbox) config-file-path)))

;; ============================================================
;; Old kernel invocation interface

(provide main)
(define (main config-file-path)
  (eprintf "Notice: IRacket kernel started through old interface.\n")
  (start-kernel #f config-file-path))
