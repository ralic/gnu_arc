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

;; $Id: oop.scm,v 1.3 2003/04/13 23:43:20 eyestep Exp $

(arc:provide 'oop)

;; this file provides a simple object and class system for arc.

(define (arc:make-class cls-name
                        super-class
                        vars
                        the-methods)
  (letrec ((isa super-class)
           (name cls-name)
           (slot-templ (append (map (lambda (p) 
                                      (cons (car p) (cdr p)) )
                                    vars)
                               (if (not (null? super-class))
                                   (super-class 'slots)
                                   ())))
           (methods the-methods)
           (self (lambda (msg . prms)
                   (case msg
                     ((super) isa)
                     ((self) self)
                     ((methods) methods)
                     ((slots) slot-templ)
                     ((alloc) (arc:alloc-instance self
                                                  slot-templ))
                     ((name) name)
                     ((instance?) #f)
                     (else (begin
                             (display "unknown class method ")
                             (display msg) (newline)
                             #f))))))
    self))

(define (arc:alloc-instance class slot-templ)
  (letrec ((isa class)
           (slots (map (lambda (p)
                         (cons (car p) (cdr p)) )
                       slot-templ))
           (self (lambda (msg . prms)
                   (case msg
                     ((class) isa)
                     ((self) self)
                     ((set!) (let* ((attr (car prms))
                                    (val (cadr prms))
                                    
                                    (aslot (assoc attr slots)))
                               (if aslot
                                   (set-cdr! aslot (list val))
                                   (begin
                                     (display "unknown slot ")
                                     (display attr) (newline)
                                     #f))))
                     ((get) (let* ((attr (car prms))
                                   (aslot (assoc attr slots)))
                              (if aslot
                                  (cadr aslot)
                                  (begin
                                    (display "unknown slot ")
                                    (display attr) (newline)
                                    #f))))
                     ((instance?) #t)
                     (else (let loop ((cls isa))
                             (if (null? cls)
                                 (begin
                                   (display "unknown method ")
                                   (display msg) (newline)
                                   #f)
                                 (let* ((methl (cls 'methods))
                                        (m (assoc msg (or methl ()))))
                                   (if (and m
                                            (procedure? (cadr m)))
                                       (apply (cadr m) (cons self prms))
                                       (loop (cls 'super)))))))))))
    self))

(define <arc:object>
  (arc:make-class '<arc:object>
                  ()          ; super class
                  '()         ; slots
                  `((isa? ,(lambda (self a-class)
                             (and (arc:class? a-class)
                                  (eq? (self 'class) a-class))))
                    (kind-of? ,(lambda (self a-class)
                                 (and (arc:class? a-class)
                                      (let loop ((cls (self 'class)))
                                        (if (null? cls)
                                            #f
                                            (or (eq? cls a-class)
                                                (loop (cls 'super))))))))
                    )))

(define (arc:class? obj)
  (and (procedure? obj)
       (not (obj 'instance?))))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
