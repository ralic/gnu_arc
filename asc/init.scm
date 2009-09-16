;; Copyright (C) 1991-2002 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111, USA.
;;
;; As a special exception, the Free Software Foundation gives permission
;; for additional uses of the text contained in its release of SCM.
;;
;; The exception is that, if you link the SCM library with other files
;; to produce an executable, this does not by itself cause the
;; resulting executable to be covered by the GNU General Public License.
;; Your use of that executable is in no way restricted on account of
;; linking the SCM library code into it.
;;
;; This exception does not however invalidate any other reasons why
;; the executable file might be covered by the GNU General Public License.
;;
;; This exception applies only to the code released by the
;; Free Software Foundation under the name SCM.  If you copy
;; code from other Free Software Foundation releases into a copy of
;; SCM, as the General Public License permits, the exception does
;; not apply to the code that you add in this way.  To avoid misleading
;; anyone as to the status of such modified files, you must delete
;; this exception notice from them.
;;
;; If you write modifications of your own for SCM, it is your choice
;; whether to permit this exception to apply to your modifications.
;; If you do not wish that, delete this exception notice.

;;;; "Init.scm", Scheme initialization code for SCM.
;;; Author: Aubrey Jaffer.

(define (scheme-implementation-type) 'ASC)
(define (scheme-implementation-version) "0.0.1")
(define (scheme-implementation-home-page) "http://www.eyestep.org/arc")


;; --------------------------------------------------------------------
;; macro support
;; --------------------------------------------------------------------
(cond ((defined? defsyntax)
       (defsyntax define-syntax (the-macro defsyntax)))
      (else
       (define defsyntax define)
       (define the-macro identity)))
(defsyntax sequence (the-macro begin))
(define copy-tree @copy-tree)


;; --------------------------------------------------------------------
;; portable IO stuff 
;; --------------------------------------------------------------------

;;; VMS does something strange when output is sent to both
;;; CURRENT-OUTPUT-PORT and CURRENT-ERROR-PORT.
(case (software-type) ((VMS) (set-current-error-port (current-output-port))))

;;; OPEN_READ, OPEN_WRITE, and OPEN_BOTH are used to request the proper
;;; mode to open files in.  MS-DOS does carriage return - newline
;;; translation if not opened in `b' mode.

(define OPEN_READ (case (software-type)
		    ((MS-DOS WINDOWS ATARIST) 'rb)
		    (else 'r)))
(define OPEN_WRITE (case (software-type)
		     ((MS-DOS WINDOWS) 'wbc)
		     ((ATARIST) 'wb)
		     (else 'w)))
(define OPEN_BOTH (case (software-type)
		    ((MS-DOS WINDOWS) 'r+bc)
		    ((ATARIST) 'r+b)
		    (else 'r+)))
(define ((make-moder str) mode)
  (if (symbol? mode)
      (string->symbol (string-append (symbol->string mode) str))
      (string-append mode str)))
(define _IONBF (make-moder "0"))
(define _TRACKED (make-moder "?"))
(define _EXCLUSIVE (make-moder "x"))

(define could-not-open #f)

(define (open-output-file str)
  (or (open-file str OPEN_WRITE)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-OUTPUT-FILE couldn't open file " str)))
(define (open-input-file str)
  (or (open-file str OPEN_READ)
      (and (procedure? could-not-open) (could-not-open) #f)
      (error "OPEN-INPUT-FILE couldn't open file " str)))




(define close-input-port close-port)
(define close-output-port close-port)

(define (call-with-open-ports . ports)
  (define proc (car ports))
  (cond ((procedure? proc) (set! ports (cdr ports)))
	(else (set! ports (reverse ports))
	      (set! proc (car ports))
	      (set! ports (reverse (cdr ports)))))
  (let ((ans (apply proc ports)))
    (for-each close-port ports)
    ans))

(define (call-with-input-file str proc)
  (call-with-open-ports (open-input-file str) proc))

(define (call-with-output-file str proc)
  (call-with-open-ports (open-output-file str) proc))

(define (with-input-from-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-input-port port)))
		thunk
		(lambda () (set! port (set-current-input-port port)))))

(define (with-output-to-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-output-port port)))
		thunk
		(lambda () (set! port (set-current-output-port port)))))

(define (with-error-to-port port thunk)
  (dynamic-wind (lambda () (set! port (set-current-error-port port)))
		thunk
		(lambda () (set! port (set-current-error-port port)))))

(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

;; --------------------------------------------------------------------
;; evaluation
;; --------------------------------------------------------------------
(define eval
  (let ((@eval @eval)
        (@copy-tree @copy-tree))
    (lambda (x) (@eval (@copy-tree x)))))

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
	     (for-each (lambda (x) (display #\  cep) (write x cep))
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
	     (for-each (lambda (x) (display #\  cep) (write x cep))
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
(define set-errno errno)
(define slib:exit quit)
(define exit quit)

(define load try-load)



;; --------------------------------------------------------------------
;; get the next option
;;
;; returns #f if no further arguments available; returns #\? if an unknown
;; options is parsed; returns #\: if the options is known, but a value is
;; missing; else returns car of the corresponding table entry.  If no
;; further options are detected and pure value is found, it is returned as
;; string.
;;
;; if cadddr of a table entry is #t, the option requires an value
;; (separated by ws), if it is #f is doesn't
;;
;;
;; the table
;; '((argument "-a" "--argument" #t)
;;   (scheme "-s" "--scheme" #t))
;; 
(define *asc:optind* 0)
(define *asc:optarg* #f)
(define *asc:optopt* #f)

(define (asc:getopt argv table)
  (if (>= *asc:optind* (length argv))
      #f
      (let* ((arg (list-tail argv *asc:optind*))
             (cmpf (cond
                    ((asc:string-prefix? (car arg) "--") caddr)
                    ((asc:string-prefix? (car arg) "-") cadr)
                    (else #f))) )
        (set! *asc:optind* (+ *asc:optind* 1))
        (set! *asc:optarg* '())
        (set! *asc:optopt* (car arg))
        (if cmpf
            (let loop ((t table))
              (if (null? t)
                  #\?
                  (if (string=? (car arg) (apply cmpf (list (car t))))
                      (if (cadddr (car t))
                          (if (and (cdr t)
                                   (not (asc:string-prefix? (cadr arg) "-")))
                              (begin
                                (set! *asc:optarg* (cadr arg))
                                (set! *asc:optind* (+ *asc:optind* 1))
                                (car (car t)))
                              #\:)
                          (car (car t)))
                      (loop (cdr t)))))
            (car arg)) )))


(define (asc:display-help)
  (asc:display
   "asc - an interpreter for Scheme code" #\nl
   "usage:" #\nl
   "  asc options" #\nl
   #\nl
   "Options:" #\nl
   " -h, --help           display this help and exit" #\nl
   " -v, --version        display version information and exit" #\nl
   " -s, --script SCRIPT  load Scheme source code from FILE, and exit" #\nl
   #\nl))

(define (asc:display-version)
  (asc:display
   "asc 0.0.1" #\nl
   "Copyright (c) 2002 by Gregor Klinke" #\nl
   "Asc may be distributed under the terms of the GNU General Public Licence;" #\nl
   "certain other uses are permitted as well.  For details, see the file" #\nl
   "'COPYING', which is included in the Guile distribution." #\nl
   "There is no warranty, to the extent permitted by law." #\nl
   #\nl))
   
(define %asc:exit-after-eval% #f)
(define %asc:script-name% #f)

;; scan the options and apply them
(define %asc:argv% (program-arguments))
(define asc:opts '((script "-s" "--script" #t)
                   (help "-h" "--help" #f)
                   (version "-V" "--version" #f)
                   (optend "-" "--" #f)))
(let loop ((opt (asc:getopt %asc:argv% asc:opts)))
  (if (not opt)
      #f
      (let ((done #f))
        (case opt
          ((help) (begin
                    (asc:display-help)
                    (quit)))
          ((version) (begin
                       (asc:display-version)
                       (quit)))
          ((script) (begin
                      (set! %asc:script-name% *asc:optarg*)
                      (set! %asc:exit-after-eval% #t)))
          ((optend) (begin
                      (set! program-arguments 
                            (lambda () 
                              (list-tail %asc:argv% *asc:optind*)))
                      (set! done #t)))
          ((#\?) (begin
                   (warn "bad option: " *asc:optopt* #\nl)
                   (quit)))
          ((#\:) (begin
                   (warn "missing arg " *asc:optopt* #\nl)
                   (quit))))
        (if (not done)
            (loop (asc:getopt %asc:argv% asc:opts)))) ))


(if %asc:script-name%
    (load %asc:script-name%))


(if %asc:exit-after-eval%
    (quit))