;;  This file is part of the arc package
;;  Copyright (C) 2002, 2003, 2009 by Gregor Klinke
;;
;;  This library is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define %arc:env% '())
(define %arc:volatile-env% '())

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

(define (arc:push-environment alist)
  (set! %arc:env% (cons alist %arc:env%)))

(define (arc:pop-environment alist)
  (if (not (null? %arc:env%))
      (set! %arc:env% (cdr %arc:env%))))


(define (arc:env-get key)
  (let loop ((ef (cons %arc:volatile-env% %arc:env%)))
    (if (null? ef)
        #f
        (let ((aslot (assoc key (car ef))))
          (if aslot
              (cadr aslot)
              (loop (cdr ef)))))))

(define (arc:env-set! key value)
  (let ((aslot (assoc key %arc:volatile-env%)))
    (if aslot
        (set-cdr! aslot (list value))
        (set! %arc:volatile-env%
              (append %arc:volatile-env%
                      (list (list key value)))))))

(define (arc:env-unset! key)
  (let ((aslot (assoc key %arc:volatile-env%)))
    (if aslot
        (begin
          (set-cdr! aslot #f)
          (set-car! aslot #f)))
    #f))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
