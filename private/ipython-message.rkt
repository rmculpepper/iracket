#lang racket/base

(require racket/contract
         racket/string
         libuuid
         json)

(provide message-type/c
         mime-type/c
         (struct-out header)
         (struct-out message)
         make-response-header
         make-response)

;; Message types
(define shell-in-message-type/c
  (symbols 'kernel_info_request
	   'comm_info_request
           'execute_request
           'complete_request
           'is_complete_request
           'object_info_request
           'shutdown_request
           'history_request))

(define shell-out-message-type/c
  (symbols 'kernel_info_reply
	   'comm_info_reply
           'execute_reply
           'complete_reply
           'is_complete_reply
           'object_info_reply
           'shutdown_reply
           'history_reply
           'execute_result))

(define iopub-message-type/c
  (symbols 'display_data
           'execute_input
           'execute_result
           'status
           'clear_output
           'stream))

(define comm-message-type/c
  (symbols 'comm_open
           'comm_msg
           'comm_close))

(define stdin-message-type/c
  (symbols 'input_reply
           'input_request))

(define message-type/c
  (or/c shell-in-message-type/c
        shell-out-message-type/c
        iopub-message-type/c
        comm-message-type/c
        stdin-message-type/c))


;; IPython message header.
(define-struct/contract header
  ([identifiers (listof bytes?)]
   [parent-header any/c] ;; (recursive-contract (or/c false/c header?))]
   [metadata (hash/c string? string?)]
   [message-id string?] ;; uuid-string?
   [session-id string?] ;; uuid-string?
   [username string?]
   [msg-type message-type/c])
  #:transparent)

;; IPython message.
(define-struct/contract message
  ([header header?]
   [content jsexpr?])
  #:transparent)

;; Make a response given a parent header, optionally overriding the
;; message type. If the message type is not given, it is determined (if
;; possible) from the parent message type.
(define (make-response parent content #:msg-type [msg-type #f])
  (define response-header
    (make-response-header (message-header parent) #:msg-type msg-type))
  (make-message response-header content))

(define (reply-type parent-type)
  (case parent-type
    [(kernel_info_request) 'kernel_info_reply]
    [(comm_info_request) 'comm_info_reply]
    [(execute_request) 'execute_reply]
    [(complete_request) 'complete_reply]
    [(is_complete_request) 'is_complete_reply]
    [(object_info_request) 'object_info_reply]
    [(shutdown_request) 'shutdown_reply]
    [(history_request) 'history_reply]
    [else (error (format "No reply for message type: ~a" parent-type))]))

;; Make a response header given a parent header, optionally overriding the
;; message type. If the message type is not given, it is determined (if
;; possible) from the parent message type.
(define (make-response-header parent-header #:msg-type [msg-type #f])
  (make-header
   (header-identifiers parent-header)
   parent-header
   (make-hasheq)
   (uuid-generate)
   (header-session-id parent-header)
   (header-username parent-header)
   (if msg-type msg-type (reply-type (header-msg-type parent-header)))))


;; Mime-types that might show up in output to IPython notebooks.
(define mime-type/c
  (symbols 'application/json
           'application/pdf
           'application/xml
           'image/gif
           'image/jpeg
           'image/png
           'image/bmp
           'image/svg+xml
           'image/tiff
           'text/csv
           'text/html
           'text/markdown
           'text/plain
           'text/rtf
           'text/xml
           'video/avi
           'video/mpeg
           'video/mp4))

