#lang info

;; ========================================
;; pkg info

(define version "1.3")
(define collection "iracket")
(define deps
  '("base"
    "zeromq-r-lib"
    "sandbox-lib"
    "uuid"
    "sha"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "scribble-doc"))

;; ========================================
;; collect info

(define name "iracket")
(define scribblings '(["iracket.scrbl" ()]))

;; Doesn't actually do installation, just prints message.
(define install-collection "install.rkt")

(define raco-commands
  '(("iracket" (submod iracket/install raco) "manage IRacket (Jupyter support)" #f)))
