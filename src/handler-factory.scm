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

;; $Id: handler-factory.scm,v 1.2 2003/04/13 23:42:42 eyestep Exp $

(define arc:handlers ())

;; this function returns an handler class for a given task specialized for
;; a system.  Class handlers have to register themselves by registering
;; with the (arc:register-handler) function
(define (arc:handler-factory system task)
  (let* ((sysnm (if (list? system)
                    (car system)
                    system))
         (aval (assoc task arc:handlers))
         (sys (if aval
                  (let ((sysa (assoc sysnm (cadr aval))))
                    (if sysa
                        (arc:load-handler-class (cdr sysa))
                        #f))
                  #f)))
    (or sys
        (begin
          (arc:log 'error "no handler for task '" 
                   task
                   "' on system '"
                   (car %arc:sysnm%) "' found")
          ;; return an empty handler
          <arc:object>))))

(define (arc:load-handler-class proc)
  (apply proc ()))

(define (arc:register-handler system task handler-class)
  (let ((aval (assoc task arc:handlers)))
    (if aval
        (let ((sysa (assoc system (cdr aval))))
          (if sysa
              (set-cdr! sysa handler-class)
              (set-cdr! aval
                        (append (cdr aval) (list (cons system
                                                       handler-class))))))
        (set! arc:handlers
              (append arc:handlers (list (list task
                                               (list (cons system
                                                           handler-class))))))
        )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
