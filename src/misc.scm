;;  This file is part of the arc package
;;  Copyright (C) 2002, 2003 by Gregor Klinke
;;
;;  This library is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU Lesser General Public License as published
;;  by the Free Software Foundation; either version 2.1 of the License, or
;;  (at your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public
;;  License along with this library; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; $Id: misc.scm,v 1.3 2003/04/15 00:04:20 eyestep Exp $

(define (arc:reduce fn base-value lst)
  (if (null? lst)
      base-value
      (fn (car lst)
          (arc:reduce fn base-value (cdr lst)))))

(define (arc:list-copy lst)
  (arc:reduce cons '() lst))

(define (arc:make-list n . init)
  (if (pair? init)
      (set! init (car init)))
  (let loop ((answer '())
             (n n))
    (if (<= n 0)
        answer
        (loop (cons init answer) (- n 1)))))

(define (arc:sublist x b e)
  (let loop ((res ())
             (xx x)
             (cc 0))
    (if (or (null? xx) (>= cc e))
        res
        (loop (if (>= cc b)
                  (append res (list (car xx)))
                  res)
              (cdr xx) (+ cc 1)))))

;; indicates wether a list contains only strings or is empty.
(define (arc:string-list? l)
  (if (not (list? l))
      #f 
      (if (null? l)
          #t
          (let loop ((lx l))
            (if (null? lx)
                #t
                (if (not (string? (car lx)))
                    #f
                    (loop (cdr lx))))))))


(define (arc:string-list->string l)
  (let loop ((res "")
             (lx l))
    (if (null? lx)
        res
        (loop (string-append res (if (string? (car lx))
                                     (string-append " " (car lx)) ""))
              (cdr lx)))))

(define (arc:string-list->string* l tmpl)
  (let loop ((res "")
             (lx l))
    (if (null? lx)
        res
        (loop (string-append res (if (string? (car lx))
                                     (string-append " " tmpl (car lx)) ""))
              (cdr lx)))))

;; indicates wether a list is an alist or not.
(define (arc:alist? l)
  (if (not (list? l))
      #f 
      (if (null? l)
          #t
          (let loop ((lx l))
            (if (null? lx)
                #t
                (if (not (list? (car lx)))
                    #f
                    (loop (cdr lx))))))))

(define (arc:display . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline))
            (else (display (car v))))
          (loop (cdr v))))))

(define (arc:msg . values)
  (display "arc: ")
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline))
            (else (display (car v))))
          (loop (cdr v)))))
  (newline))

(define (arc:log level . values)
  (case level
    ((verbose) (if %arc:verbose% (apply arc:msg values)))
    ((debug) (if %arc:debug% (apply arc:msg values)))
    ((info) (apply arc:msg values))
    ((error) (apply arc:msg values))
    ((fatal) (begin
               (apply arc:msg values)
               (quit)))))

;; looks up a entry from an alist, and returns it's value.  if no such
;; entry is found, returns default
(define (arc:aval key alist default)
  (let ((va (assoc key alist)))
    (if va 
        (cadr va)
        default)))

(define (arc:aval->mlist alist)
  (let loop ((res ())
             (x alist))
    (if (null? x)
        res
        (loop (append res (list (caar x) (cadar x)))
              (cdr x)))))

(define (arc:pathlist-sep)
  (case (car %arc:sysnm%)
    ((linux bsd beos maxosx) ":")
    ((win32 os2) ";")
    (else ":")))


;; reads a complete line from a port
(define (arc:readline port)
  (let* ((res ())
         (cc (read-char port)))
    (if (eof-object? cc)
        cc
        (begin
          (do ((c cc (read-char port)))
              ((or (eof-object? c)
                   (equal? c #\nl)) #t)
            (set! res (cons c res)) )
          (list->string (reverse res))))))

(define (arc:num/str nr)
  (if (number? nr)
      (number->string nr)
      nr))


(define (arc:list->alist . vals)
  (let loop ((res ())
             (v vals))
    (if (or (null? v)
            (null? (cdr v)))
        res
        (loop (append res (list (list (car v) (cadr v))))
              (cddr v)))))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
