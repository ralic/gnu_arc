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
  (define cep (current-error-port))
  (if (defined? print-call-stack)
      (print-call-stack cep))
  (perror "WARN")
  (errno 0)
  (display "WARN: " cep)
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\space  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep))

(define (error . args)
  (define cep (current-error-port))
  (if (defined? print-call-stack)
      (print-call-stack cep))
  (perror "ERROR")
  (errno 0)
  (display "ERROR: " cep)
  (if (not (null? args))
      (begin (display (car args) cep)
	     (for-each (lambda (x) (display #\space  cep) (write x cep))
		       (cdr args))))
  (newline cep)
  (force-output cep)
  (abort))


(define (asc:display . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline))
            (else (display (car v))))
          (loop (cdr v))))))


(define asc:error error)
(define asc:warn warn)
(define slib:exit quit)
(define exit quit)


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
