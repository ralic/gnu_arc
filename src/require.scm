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

;; ----------------------------------------------------------------------
;; require/provide
;; ----------------------------------------------------------------------
(define arc:provide-list ())

(define (arc:load script)
  (let ((p (arc:string->path script)))
    (if (arc:path-absolute? p)
        (load script)
        (load (arc:path->string (arc:path-append (arc:string->path ".") p))))))
  
(define (arc:provide tag)
  (let ((t (member tag arc:provide-list)))
    (if t
        #t
        (set! arc:provide-list (append arc:provide-list (list tag))))))

(define (arc:require tag . fname)
  (if (member tag arc:provide-list)
      #t
      (let ((fn (if (> (length fname) 0)
                    (if (equal? (string-ref (car fname) 0) #\/)
                        (car fname)
                        (arc:--look-for-a-file (car fname)))
                    (arc:--look-for-a-file (symbol->string tag))) ))
        (if fn
            (begin
              (if %arc:debug%
                  (begin (display "load: ") (display fn) (newline)))
              (arc:load fn))
            (begin
              (display "required file not found: '")
              (display (symbol->string tag))
              (display "'")
              (newline)
              (quit))) )))

(define (arc:--look-for-a-file fn)
  (let loop ((pp %arc:arc-incl-path%))
    (if (null? pp)
        #f
        (let ((t (string-append (car pp) "/" fn ".scm")))
          (if (arc:sys 'file-exists? t)
              t 
              (loop (cdr pp)))))))



;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
