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

;; $Id: arcconf.scm,v 1.1 2003/04/12 23:50:49 eyestep Exp $

(define %arc:env% ())

;; loads and evaluates a arcconfig script 

;; @todo : this is too simple.  need a syntax check on the config file
;; read!
(define (arc:load-arcconfig script) 
  (arc:log 'debug "load config " script)
  (let* ((*in* (open-input-file script))
         (conf (read *in*)))
    (close-port *in*)
    (if conf
        (set! %arc:env% (cons conf %arc:env%))
        #f)))

(define (arc:env-get key)
  (let loop ((ef %arc:env%))
    (if (null? ef)
        #f
        (let ((aslot (assoc key (car ef))))
          (if aslot
              (cadr aslot)
              (loop (cdr ef)))))))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
