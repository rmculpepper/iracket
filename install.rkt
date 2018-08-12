#lang racket/base
(require (for-syntax racket/base)
         racket/string
         racket/file
         racket/port
         racket/system
         racket/runtime-path)

;; Script for configuring iracket.

;; TODO:
;; - add c3 support installation
;; - uninstallation?

(define-runtime-path iracket-dir ".")
(define-runtime-path kernel-path "static/kernel.json")
(define-runtime-path-list js-paths (list "static/custom.js" "static/c3.js"))

(define *use-ipython-dir* (make-parameter #f))

;; ----

(define (ipython-exe)
  (or (find-executable-path "ipython")
      (raise-user-error "Cannot find ipython configuration directory; try --ipython-dir")))

(define (ipython-dir)
  (or (*use-ipython-dir*)
      (string-trim
       (with-output-to-string
         (lambda () (system*/exit-code (ipython-exe) "locate"))))))

(define (write-iracket-kernel-json! lang-lines)
  (println lang-lines)
  (define (make-kernel lang-line)
    (define folder (string-replace (string-replace
                                    (string-trim lang-line "#lang ")
                                    "/" "-")
                                   " " "-"))
    (define lang-kernel-dir (build-path (ipython-dir) "kernels" folder))
    (make-directory* lang-kernel-dir)
    (define kernel-json
      (regexp-replace* (regexp-quote "#lang racket")
                       (regexp-replace* (regexp-quote "IRACKET_SRC_DIR")
                                        (file->string kernel-path)
                                        (path->string iracket-dir))
                       lang-line))
    (define dest-file (build-path lang-kernel-dir "kernel.json"))
    (when (file-exists? dest-file)
      (printf "Replacing old ~s\n" (path->string dest-file)))
    (with-output-to-file dest-file #:exists 'truncate/replace
      (lambda () (write-string kernel-json)))
    (printf "Kernel json file copied to ~s\n" (path->string dest-file)))
  (for-each make-kernel lang-lines))

(module* main #f
  (require racket/cmdline)
  (define lang-lines (make-parameter '("#lang racket")))
  (command-line
   #:program "iracket/install.rkt"
   #:once-any
   [("--ipython-dir") use-ipython-dir
    "Write to given ipython configuration directory"
    (*use-ipython-dir* use-ipython-dir)]
   #:multi
   [("-l" "--lang") lang-line
                    "Add #lang lines as additional kernels"
                    (lang-lines (cons lang-line (lang-lines)))]
   #:args ()
   (write-iracket-kernel-json! (lang-lines))))

;; ----------------------------------------

;; raco setup hook; doesn't actually do installation, just prints message
(define (post-installer _parent _here _user? _inst?)
  (printf "\n***\n")
  (printf "*** IRacket must register its kernel with jupyter before it can be used.\n")
  (printf "*** Run `racket -l iracket/install` to finish installation.\n")
  (printf "***\n\n"))
(provide post-installer)
