#lang racket/base
(require (for-syntax racket/base)
         racket/contract
         racket/cmdline
         raco/command-name
         racket/list
         racket/match
         racket/string
         racket/file
         racket/path
         racket/port
         racket/system
         racket/runtime-path
         setup/dirs
         json)
(provide
 (contract-out
  [install-iracket!
   (->* [] [#:jupyter-exe (or/c #f (and/c path-string? complete-path?))
            #:racket-exe (or/c 'auto 'this-version path-string?)]
        void?)]
  [check-iracket
   (->* [] [#:jupyter-exe (or/c #f (and/c path-string? complete-path?))]
        void?)]))

;; Script for configuring iracket.

;; TODO:
;; - uninstallation?

(define-runtime-path-list other-static-files
  (list "static/logo-32x32.png" "static/logo-64x64.png"))

(define *use-jupyter-exe* (make-parameter #f (lambda (x) (if x (build-path x) x))))

(define (get-jupyter-exe [fail-ok? #f])
  (or (*use-jupyter-exe*)
      (find-executable-path
       (case (system-type 'os)
         [(windows) "jupyter.exe"]
         [else "jupyter"]))
      (if fail-ok? #f (raise-user-error "Cannot find jupyter executable."))))

(define (get-jupyter-dir [fail-ok? #f])
  (let ([jupyter (get-jupyter-exe fail-ok?)])
    (cond [jupyter
           (string->path
            (string-trim
             (with-output-to-string
               (lambda ()
                 (define s (system*/exit-code jupyter "--data-dir"))
                 (unless (zero? s)
                   (raise-user-error "Received non-zero exit code from jupyter command."))))))]
          [fail-ok? #f]
          [else (raise-user-error "Cannot find jupyter data directory.")])))

(define (get-racket-kernel-dir [fail-ok? #f])
  (define jupyter-dir (get-jupyter-dir fail-ok?))
  (and jupyter-dir (build-path jupyter-dir "kernels" "racket")))

;; ============================================================
;; Commands

;; ----------------------------------------
;; Check status

(define (cmd:check args)
  (command-line
   #:program (short-program+command-name)
   #:argv args
   #:once-any
   [("--jupyter-exe") jupyter
    "Use given jupyter executable"
    (*use-jupyter-exe* jupyter)]
   #:args ()
   (with-handlers ([exn:fail:user?
                    (lambda (e) (printf "~a\n" (exn-message e)))])
     (do-check-iracket))))

(define (check-iracket #:jupyter-exe [jupyter-exe #f])
  (parameterize ((*use-jupyter-exe* jupyter-exe))
    (do-check-iracket)))

(define (do-check-iracket)
  (define (yn b) (if b "yes" "no"))
  ;; - jupyter executable exists
  ;; - kernel exists
  ;;   - looks correct?
  ;; - zeromq library found, works
  (begin
    (printf "IRacket install-history file: ~v\n" (path->string PREF-FILE))
    (printf "  file exists?: ~a\n" (yn (file-exists? PREF-FILE)))
    (when (file-exists? PREF-FILE)
      (match (get-pref 'installed)
        [(list* data-dir _)
         (printf "  file says IRacket was registered with Jupyter\n")
         (printf "    Jupyter data directory: ~v\n" data-dir)]
        [#f
         (printf "  file says IRacket has not been registered with Jupyter\n")]
        [_
         (printf "  file has bad contents\n")]))
    (define jupyter (get-jupyter-exe))
    (printf "Jupyter executable: ~v\n" (path->string jupyter))
    (define jupyter-dir (get-jupyter-dir))
    (printf "Jupyter data directory: ~v\n" (path->string jupyter-dir))
    (define kernel-dir (get-racket-kernel-dir))
    (define kernel-path (build-path kernel-dir "kernel.json"))
    (printf "IRacket kernel file: ~v\n" (path->string kernel-path))
    (printf "  kernel file exists?: ~a\n" (yn (file-exists? kernel-path)))
    (void)))

;; (define (candidate-racket-paths)
;;   (printf "exec-file = ~v\n" (find-system-path 'exec-file))
;;   (printf "Racket executables:\n~v\n"
;;           (map (lambda (p) (and p (path->string p)))
;;                (list (build-path "racket")
;;                      (find-system-path 'exec-file)
;;                      (find-executable-path (find-system-path 'exec-file))
;;                      (find-executable-path (find-system-path 'exec-file) "racket" #f)
;;                      (find-executable-path (find-system-path 'exec-file) "racket" #t)
;;                      (build-path (find-console-bin-dir) "racket")))))
;; (candidate-racket-paths)

;; ----------------------------------------
;; Prefs

(define PREF-FILE (build-path (find-system-path 'pref-dir) "iracket.rktd"))

;; Preference file keys:
;; - 'installed : (list* DataDir ???) -- eg, '("/home/me/.local/share/jupyter")

(define (get-pref k) (get-preference k (lambda () #f) 'timestamp PREF-FILE))
(define (put-pref k v) (put-preferences (list k) (list v) #f PREF-FILE))

;; ----------------------------------------
;; Install kernel

(define (cmd:install args)
  (define racket-command 'auto)
  (command-line
   #:program (short-program+command-name)
   #:argv args
   #:once-each
   [("--jupyter-exe") jupyter
    "Use given jupyter executable"
    (*use-jupyter-exe* jupyter)]
   #:help-labels
   "Selecting the `racket` command that Jupyter will use to run the kernel:"
   #:once-any
   [("--racket-exe") racket-exe
    "Use the given command (eg, `racket`)"
    (set! racket-command racket-exe)]
   [("--this-version-racket-exe")
    "Use the absolute path of this version of Racket"
    (set! racket-command 'this-version)]
   [("--auto-racket-exe")
    "Use `racket`, but only if it is in the executable search path"
    (set! racket-command 'auto)]
   #:help-labels
   "Meta options:"
   #:args ()
   (do-install-iracket! racket-command)))

(define (install-iracket! #:jupyter-exe [jupyter-exe #f]
                          #:racket-exe [racket-exe #f])
  (parameterize ((*use-jupyter-exe* jupyter-exe))
    (do-install-iracket! racket-exe)))

(define (do-install-iracket! [racket-exe #f])
  (let ([racket-exe (resolve-racket-exe racket-exe)])
    (write-iracket-kernel-json! racket-exe)
    (for ([file (in-list other-static-files)])
      (define dest-file (build-path (get-racket-kernel-dir) (file-name-from-path file)))
      (copy-file file dest-file #t))
    (put-pref 'installed (list (path->string (get-jupyter-dir))))))

(define (write-iracket-kernel-json! racket-exe)
  (define racket-kernel-dir (get-racket-kernel-dir))
  (make-directory* racket-kernel-dir)
  (define kernel-json (make-kernel-json racket-exe))
  (define dest-file (build-path racket-kernel-dir "kernel.json"))
  (when (file-exists? dest-file)
    (printf "Replacing old ~s\n" (path->string dest-file)))
  (with-output-to-file dest-file #:exists 'replace
    (lambda () (write-json kernel-json)))
  (printf "Kernel installed in ~s\n" (path->string racket-kernel-dir)))

(define (make-kernel-json racket-exe)
  (hash 'argv `(,(path->string racket-exe)
                "-l" "iracket/iracket"
                "--" "{connection_file}")
        'display_name "Racket"
        'language "racket"))

(define (resolve-racket-exe racket-exe)
  (cond [(path-string? racket-exe)
         (when (or (not (file-exists? racket-exe))
                   (not (memq 'execute (file-or-directory-permissions racket-exe))))
           (eprintf "Warning: ~v is not executable file\n" racket-exe))
         (build-path racket-exe)]
        [(eq? racket-exe 'this-version)
         (get-this-racket-exe)]
        [(eq? racket-exe 'auto) ;; means use RACKET-EXE-NAME if in path
         (define exe (find-executable-path RACKET-EXE-NAME))
         (unless exe
           (raise-user-error
            (format "No `~a` command in current executable search path."
                    RACKET-EXE-NAME)))
         (unless (same-path? exe (get-this-racket-exe))
           (eprintf "Warning: found different `~a` command in executable search path\n"
                    RACKET-EXE-NAME)
           (eprintf "  found in search path: ~v\n" (path->string exe))
           (eprintf "  currently executing:  ~v\n" (path->string (get-this-racket-exe))))
         (build-path RACKET-EXE-NAME)]))

(define (same-path? a b)
  (equal? (resolve-path a) (resolve-path b)))

(define RACKET-EXE-NAME
  (case (system-type 'os)
    [(windows) "racket.exe"]
    [else "racket"]))

(define (get-this-racket-exe)
  (unless (find-console-bin-dir)
    (raise-user-error "Cannot find Racket executable directory."))
  (build-path (find-console-bin-dir) RACKET-EXE-NAME))

;; ----------------------------------------
;; Help

(define (cmd:help _args)
  (printf "Usage: ~a <command> <option> ... <arg> ...\n\n"
          (short-program+command-name))
  (printf "Commands:\n")
  (define command-field-width
    (+ 4 (apply max 12 (map string-length (map car subcommand-handlers)))))
  (for ([subcommand (in-list subcommand-handlers)])
    (match-define (list command _ help-text) subcommand)
    (define pad (make-string (- command-field-width (string-length command)) #\space))
    (printf "  ~a~a~a\n" command pad help-text)))

;; ============================================================
;; Main (command dispatch)

(define subcommand-handlers
  `(("help"    ,cmd:help     "show help")
    ("check"   ,cmd:check    "check IRacket configuration")
    ("install" ,cmd:install  "register IRacket kernel with Jupyter")))

(define (call-subcommand handler name args)
  (parameterize ((current-command-name
                  (cond [(current-command-name)
                         => (lambda (prefix) (format "~a ~a" prefix name))]
                        [else #f])))
    (handler args)))

(module+ raco
  (define args (vector->list (current-command-line-arguments)))
  (cond [(and (pair? args) (assoc (car args) subcommand-handlers))
         => (lambda (p) (call-subcommand (cadr p) (car args) (cdr args)))]
        [else (cmd:help args)]))

;; ============================================================
;; raco setup hook

(provide installer)

(define (installer _parent _here _user? _inst?)
  (when _user?
    (match (get-pref 'installed)
      [#f
       (printf "\n")
       (printf "  IRacket must register its kernel with jupyter before it can be used.\n")
       (printf "  Run `raco iracket install` to finish installation.\n")
       (printf "\n")]
      [(list* (? string? data-dir) _) (void)]
      [_ (void)])))
