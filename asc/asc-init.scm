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
;; arc specific settings and code
;; --------------------------------------------------------------------
(define %arc:scheme-impl% 'asc)

(define %arc:home% (or (sys:getenv "ARC_HOME")
                       (arc-home)))
(define (arc:home)
  (if (not %arc:home%)
      (begin 
        (set! %arc:home% 
              (case (software-type)
                ((PLAN9 UNIX CYGWIN VMS MWC COHERENT) "/usr/share/arc")
                ((AMIGA ATARIST ACORN) "/arc")
                ((WINDOWS) "c:\Programme\arc")
                ((MS-DOS OS/2) "c:\\arc")
                ((THINK_C) "sys:arc")
                ((MACOS) "arc")
                ((BEOS) "/boot/home/config/arc")
                (else "/usr/local/share/arc")))
        %arc:home%)
      %arc:home%))

;; setup the program arguments
(define %arc:argv% (list-tail (program-arguments) 1))

(load (string-append (arc:home) "/strings.scm"))
(load (string-append (arc:home) "/logical.scm"))

;; eval the loader
(load (string-append (arc:home) "/arc.scm"))

