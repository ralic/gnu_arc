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

;; $Id: oop.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $

(arc:provide 'oop)

;; a class definition:
;(define <arc:my-class>
;  (list '<my-class>
;        '((name "my-class"))
;        super-class
;        `((say ,(lambda (self a b) (display (+ a b)) (newline)))
;          (time ,(lambda (self) (display "today") (newline))) )
;        ))

(define (arc:make-instance cls)
  (letrec ((class cls)
           ;; make a copy of the slots
           (slots (map (lambda (p) 
                         (cons (car p) (cdr p)) )
                       (cadr cls)))
           ;; make ptr to self and implement the method eval
           (self (lambda (msg . prms)
                   (case msg
                     ((class) class)
                     ((self) self)
                     ((set!) (let* ((attr (car prms))
                                    (val (cadr prms))

                                    (aslot (assoc attr slots)))
                               (if aslot
                                   (set-cdr! aslot (list val))
                                   (begin
                                     (arc:msg "unknown slot " attr)
                                     #f))))
                     ((get) (let* ((attr (car prms))
                                   (aslot (assoc attr slots)))
                              (if aslot
                                  (cadr aslot)
                                  (begin
                                    (arc:msg "unknown slot " attr)
                                    #f))))
                     (else 
                      (let loop ((cl class))
                        (if (null? cl)
                            (begin
                              (arc:msg "unknown method " msg)
                              #f)
                            (let ((m (assoc msg (cadddr cl))))
                              (if (and m
                                       (procedure? (cadr m)))
                                  (apply (cadr m) (cons self prms))
                                  (loop (caddr class)))) ))
                      )))))
    self))


(define <arc:object>
  (list '<arc:object>                   ; name of the class
        '()                             ; slots
        
        '()                              ; superclass
        
        ;; methods
        '()
        ))

; to get a copy of an class
;(define m (arc:make-instance <arc:my-class>))
