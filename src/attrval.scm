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

;;  Make a attrval
(define (arc:attrval)
  (vector ':attrval #f ()))

(define (arc:attrval? av)
  (and (vector? av)
       (eq? (vector-ref av 0) ':attrval)))

;; Set the default slot id.
(define (arc:attrval-default-id! av id)
  (if (arc:attrval? av)
      (vector-set! av 1 id)))

;; Return the default slot id.
(define (arc:attrval-default-id av)
  (if (arc:attrval? av)
      (vector-ref av 1)))
  
;; Sets a value for an attribute 'id'
(define (arc:attrval-set! av id value)
  (if (arc:attrval? av)
      (let ((mv (member id (vector-ref av 2))))
        (if mv
            (set-car! (cdr mv) value)
            (vector-set! av 2 (append (vector-ref av 2) (list id value)))))))

;; Looks up a attribute for 'id'
(define (arc:attrval-ref av id)
  (if (arc:attrval? av)
      (let ((mv (member id (vector-ref av 2))))
        (if mv
            (cadr mv)
            #f))
      #f))


;; Looks up the default attribute (if any), or #f. 
(define (arc:attrval-default av)
  (if (arc:attrval? av)
      (let* ((def (vector-ref av 1))
             (mv (if def
                     (member def (vector-ref av 2))
                     #f)))
        (if mv
            (cadr mv)
            #f))
      #f))

;; Sets the value for the default attribute.
(define (arc:attrval-default! av value)
  (if (arc:attrval? av)
      (let* ((def (vector-ref av 1)))
        (if def
            (arc:attrval-set! av def value)))))



(define (arc:attrval-concat av av2)
  (if (arc:attrval? av)
      (if (arc:attrval? av2)
          (let ((rv (arc:attrval)))
            (arc:attrval-default-id! rv #f)
            (let loop ((v (vector-ref av 2)))
              (if (null? v)
                  'done
                  (begin
                    (arc:attrval-set! rv (car v) (cadr v))
                    (loop (cddr v)))))
            (let loop2 ((v (vector-ref av2 2)))
              (if (null? v)
                  'done
                  (begin
                    (arc:-comb-attr-val rv (car v) (cadr v))
                    (loop2 (cddr v)))))
            rv)
          (if (null? av2)
              av
              #f))
      #f))

(define (arc:-comb-attr-val rv key value)
  (let ((x (arc:attrval-ref rv key)))
    (if x
        (cond
         ((string? x) (arc:msg "can't add value for attrval key " 
                               key " (string)"))
         ((integer? x) (arc:msg "can't add value for attrval key " 
                                key " (integer)"))
         ((char? x) (arc:msg "can't add value for attrval key " 
                             key " (char)"))
         ((boolean? x) (arc:msg "can't add value for attrval key " 
                                key " (boolean)"))
         ((procedure? x) (arc:msg "can't add value for attrval key " 
                                  key " (procedure)"))
         ((list? x) (if (list? value)
                        (arc:attrval-set! rv key (append x value))
                        (arc:attrval-set! rv key (append x (list value)))))
         ((vector? x) (arc:msg "can't add value for attrval key "
                               key " (vector)"))
         ((pair? x) (arc:msg "can't add value for attrval key " 
                             key " (pair)"))
         (else (arc:msg "unknown type for attrval key " key )))
        (arc:attrval-set! rv key value))))


(define (arc:concat . keys)
  (let loop ((cv #f)
             (k keys))
    (if (null? k)
        cv
        (loop (cond
               ((not cv) (car k))
               ((and (arc:attrval? cv)
                     (arc:attrval? (car k))) (arc:attrval-concat cv (car k)))
               ((and (list? cv) (list? (car k))) (append cv (car k)))
               (else (begin
                       (arc:msg "Can't concat values '" cv "' and '" 
                                (car k) "'")
                       cv)))
              (cdr k)))))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
