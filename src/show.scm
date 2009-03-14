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

;; $Id: show.scm,v 1.2 2009/03/14 23:40:19 eyestep Exp $

(define (arc:display-script script)
  (let ((current-stmt arc:current-stmt))
    ;; the load command load and evaluates the script
    (set! %arc:--script-in-loading--% script)
    
    (arc:load-arcfile script)
    
    (arc:show-display-info)
    (arc:context-drop)
    ))

(define (arc:show-display-info)
  (let loop ((mq %arc:contexts%))
    (if (null? mq)
        #t
        (begin
          (if (arc:context? (car mq))
              (begin
                (arc:display "CONTEXT NAME: " 
                             (vector-ref (car mq) %ARC:ID-SLOT%) #\nl)
                (arc:show-context-stmts
                 (vector-ref (car mq) %ARC:STMT-SLOT%)) 
                )
              (arc:display "??" #\nl))
          (loop (cdr mq))))))

(define (arc:show-context-stmts stmt-alist)
  ;; iterate to get the max length of the stmt-ids
  (let* ((stmt-id-length 
          (let loop ((len 0)
                     (ta stmt-alist))
            (if (null? ta)
                len
                (loop (if (equal? (arc:stmt-scope (car ta)) 'public)
                          (let ((l (string-length (arc:stmt-display-name (car ta)))))
                            (if (> l len) l len))
                          len)
                      (cdr ta)))) ))
    (let loop ((ta stmt-alist))
      (if (null? ta)
          #t
          (let* ((stmt (car ta))
                 (stmt-name (arc:stmt-display-name stmt))
                 (stmt-name-length (string-length stmt-name))
                 (info (arc:stmt-info (car ta))))
            (if (equal? (arc:stmt-scope stmt) 'public)
                (begin
                  (arc:display "  " stmt-name
                               (arc:stmt-id-spacer stmt-name-length
                                                   stmt-id-length) 
                               " : " )
                  (if info
                      (arc:show-info-wrapped info (+ 2 stmt-id-length 3)))
                                             
                  (arc:display #\nl)))
            (loop (cdr ta)))))) )
(define (arc:stmt-id-spacer sym-len max-len)
  (if (> sym-len max-len)
      ""
      (make-string (- max-len sym-len) #\space)))

(define (arc:stmt-os-name stmt)
  (let ((os (arc:stmt-os stmt)))
    (if (and os
             (not (equal? os 'all)))
        (string-append " [" 
                       (cond
                        ( (list? os) (arc:string-list->string*
                                      (map (lambda (s) (symbol->string s)))
                                      " ") )
                        ( (symbol? os) (symbol->string os)) )
                       (else (arc:throw 'assert "Bad OS"))
                        "]" )
        "")))
    
(define (arc:stmt-display-name stmt)
  (string-append (symbol->string (arc:stmt-id stmt))
                 (arc:stmt-os-name stmt)) )

(define (arc:show-info-wrapped txt col)
  (let ((dist (- 78 col))
        (spc (make-string col #\space))
        (txtl (string-length txt)))
    (let loop ((first #f)
               (ofs 0))
      (if (>= ofs txtl)
        #t
        (let ((len (if (< (+ ofs dist) txtl)
                       dist
                       (- txtl ofs))))
          (if first 
              (arc:display #\nl (make-string col #\space)))
          (arc:display (substring txt ofs (+ ofs len)))
          (loop #t (+ ofs len)))))) )
