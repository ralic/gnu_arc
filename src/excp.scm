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

;; $Id: excp.scm,v 1.1 2003/04/19 01:03:35 eyestep Exp $


(define arc:top-exception-handler 
  (lambda (type arg)
    (arc:display (arc:to-str "unhandled ("
                             type ") " arg) #\nl)))

(define (arc:throw type arg) 
  (arc:top-exception-handler type arg))

(define (arc:try catch-clause body)
  (let* ((result #f)
         (error-type #f)
         (error-arg #f)
         (old-handler arc:top-exception-handler)
         (success (call-with-current-continuation
                   (lambda (cont)
                     (set! arc:top-exception-handler
                           (lambda (type arg)
                             (set! error-type type)
                             (set! error-arg arg)
                             (cont #f)))
                     (set! result (apply body '()))
                     #t))))
    (set! arc:top-exception-handler old-handler)
    (if success
        result
        (apply catch-clause (list error-type error-arg)))))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
