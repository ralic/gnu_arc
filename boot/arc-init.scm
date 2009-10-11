;; --------------------------------------------------------------------
;; string functions
;; --------------------------------------------------------------------
(define (string-index str chr)
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (char=? chr (string-ref str pos)))
       (and (< pos len) pos))))

(define (asc:string-prefix? str prefix)
  (let* ((strl (string-length str))
         (pfxl (string-length prefix)))
    (and (<= pfxl strl)
         (equal? prefix (substring str
                                   0
                                   pfxl)))))


;; --------------------------------------------------------------------
;; warning and error messages
;; --------------------------------------------------------------------
(define (warn . args)
  (let* ((port (current-error-port)))
    (display "WARN: " port)
    (if (not (null? args))
        (begin (display (car args) port)
               (for-each (lambda (x) (display #\space port) (write x port))
                         (cdr args))))
    (newline port) ))

(define (error . args)
  (let* ((port (current-error-port)))
    (display "ERROR: " port)
    (if (not (null? args))
        (begin (display (car args) port)
               (for-each (lambda (x) (display #\space port) (write x port))
                         (cdr args))))
    (newline port) ))


(define (asc:display . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline))
            (else (display (car v))))
          (loop (cdr v))))))


;; --------------------------------------------------------------------
;; arc specific settings and code
;; --------------------------------------------------------------------
;; setup the program arguments
(define %arc:argv% (vector->list *args*))

(load (string-append (arc:home) "/strings.scm"))
(load (string-append (arc:home) "/logical.scm"))

;; load system specific code
(load (string-append (arc:home) "/sys.scm"))

;; eval the loader
(load (string-append (arc:home) "/arc.scm"))
