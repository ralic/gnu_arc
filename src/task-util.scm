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

(define (arc:missing-keyword-option task keyw)
  (arc:msg "missing value for keyword '" keyw "' in task '"
           task "'"))

(define (arc:unknown-keyword task keyw)
  (arc:msg "unknown keyword: '" keyw "' in task: '" task "'"))

(define (arc:bad-keyword-option task desc)
  (arc:msg "bad keyword option in task '"
           task "' (" desc ")"))

(define (arc:exclusive-keyword task set-keyw)
  (arc:msg "exclusive keyword used in task '"
           task "'; keyword used already '" set-keyw "'"))



(define (arc:keyword? key)
  (and (symbol? key)
       (let ((ks (symbol->string key)))
         (eq? (string-ref ks (- (string-length ks) 1)) #\:))))
(define (arc:keyword->symbol keyw)
  (if (symbol? keyw)
      (let ((ks (symbol->string keyw)))
        (string->symbol (substring ks 0 (- (string-length ks) 1))))
      (error "not-a-keyword")))

(define (arc:get-keywords expr)
  (let loop ((res ())
             (k expr))
    (if (null? k)
        res
        (if (arc:keyword? (car k))
            (loop (append res (list (arc:keyword->symbol (car k))
                                    (arc:eval-arc (cadr k))))
                  
                  (if (not (null? (cdr k) ))
                      (cddr k)
                      ()))
            res))))

;; returns the keywords of a argument list seperated from the body in a
;; cons.  The car of the pair is the paramterlist, the cdr the body
(define (arc:get-keywords-and-body expr)
  (let loop ((res ())
             (k expr))
    (if (null? k)
        (cons res k)
        (if (arc:keyword? (car k))
            (loop (append res (list (arc:keyword->symbol (car k))
                                    (arc:eval-arc (cadr k))))
                  
                  (if (not (null? (cdr k) ))
                      (cddr k)
                      ()))
            (cons res k)))))

;; ----------------------------------------------------------------------
;; utility functions for task specifications
;; ----------------------------------------------------------------------
;; compiles a key list given to a generic task and translates it into an
;; a-list.  during compilation it is checked if the keyword parameters
;; match the required types, if required keys are specified etc.
;;
;; the table given to this function is an alist specifing the allowed
;; keywords, their values' type and if they are optional or required.
;; the format for each aframe is:
;;
;; (KEYWORD TYPE REQ-STATEMENT)
;;
;; whereby REQ-STATEMENT is either one of the symbol: optional or required,
;; or a list of the form: (REQ-KEYW KEYW2)
;; The following REQ-KEYW are known:
;; 
;; opt-xor - only one of the keywords (the current keyword or KEYW2) can be
;; be specified (exclusive or)
;;
;; req-xor - exactly one of the keywords (the current keyword or KEYW2) has
;; to be specified (required exclusive)
;;
;; req-or - at least one of the keywords (the current keyword or KEYW2) has
;; to be specified (required inclusive or)
;;
;; examples:
;;'((:info string optional)
;;  (:dir string required))
;;
;; in the following case either ':set or ':pattern has to be given
;;'((:set string (req-xor :pattern))
;;  (:pattern string (req-xor :set)))
;;
;; in the following case at least one of ':set and ':pattern has to be
;; given
;;'((:set string (req-or :pattern))
;;  (:pattern string (req-or :set)))
;;
;; in the following case at most one of ':set and ':pattern may be given
;;'((:set string (opt-xor :pattern))
;;  (:pattern string (opt-xor :set)))

(define (arc:compile-key-list table keys task-name)
  (let ((rx (let loop ((res ())
                       (key keys))
              (if (null? key)
                  res
                  (loop (let* ((nextval (if (not (null? (cdr key)))
                                            (cadr key)
                                            #f))
                               (keycheck (arc:keytable-valid-keypair? 
                                          table key nextval)))
                          (case keycheck
                            ((:ok) (append res (list (list (car key) 
                                                           nextval))))
                            (else (begin
                                    (arc:msg task-name ": " 
                                             keycheck " (was: " nextval ")")
                                    res))))
                        (if (not (null? (cdr key) ))
                            (cddr key) 
                            ()))))))
    (let loop2 ((retv rx)
                (ta table))
      (if (null? ta)
          retv
          (loop2 (cond 
                  ((eq? (caddar ta) 'optional) retv)
                  ((eq? (caddar ta) 'required) 
                   (if (assoc (caar ta) rx)
                       retv
                       ;; the required value has not been defined!
                       (begin
                         (arc:msg task-name ": required keyword '"
                                  (symbol->string (caar ta))
                                  "' not specified")
                         #f)))
                  ((list? (caddar ta))
                   (case (car (caddar ta))
                     ((req-or)
                      (if (and (not (assoc (caar ta) rx))
                               (not (assoc (cadr (caddar ta)) rx)))
                          ;; the required value has not been defined!
                          (begin
                            (arc:msg task-name ": neither keyword '"
                                     (symbol->string (caar ta))
                                     "' nor '"
                                     (symbol->string (cadr (caddar ta)))
                                     "' specified")
                            #f)
                          retv))
                     ((opt-xor)
                      (if (and (assoc (caar ta) rx)
                               (assoc (cadr (caddar ta)) rx))
                          (begin
                            (arc:msg task-name ": optional exclusive keywords '"
                                     (symbol->string (caar ta))
                                     "' and '"
                                     (symbol->string (cadr (caddar ta)))
                                     "' specified")
                            #f)
                          retv))
                     ((req-xor)
                      (if (and (assoc (caar ta) rx)
                               (assoc (cadr (caddar ta)) rx))
                          (begin
                            (arc:msg task-name ": required exclusive keywords '"
                                     (symbol->string (caar ta))
                                     "' and '"
                                     (symbol->string (cadr (caddar ta)))
                                     "' specified")
                            #f)
                          (if (and (not (assoc (caar ta) rx))
                                   (not (assoc (cadr (caddar ta)) rx)))
                              (begin
                                (arc:msg task-name ": neither keyword '"
                                         (symbol->string (caar ta))
                                         "' nor '"
                                         (symbol->string (cadr (caddar ta)))
                                         "' specified")
                                #f)
                              retv)))
                     (else (begin
                             (arc:msg task-name ": unknown table entry: '"
                                      (car (caddar ta)) "'")
                             retv))))
                  (else (begin
                          (arc:msg task-name ": unknown table entry: '"
                                   (car (caddar ta)) "'")
                          retv)))
                 (cdr ta))))
    ))


(define (arc:keytable-valid-keypair? table key nextval)
  (let ((aval (assoc (car key) table))
        (keynm (symbol->string (car key))))
    (if (null? nextval)
        ':ok
        ;;(string-append "missing value for keyword '" keynm "'")
        (if (not aval)
            (string-append "unknown keyword '" keynm "'")
            (if (list? (cadr aval))
                (let loop ((t (cadr aval)))
                  (if (null? t)
                      (string-append "bad value type for keyword '"
                                     keynm
                                     "' " (arc:string-list->string (cadr aval)) " expected " (cadr aval))
                      (if (arc:nextval-type? (car t) keynm nextval)
                          ':ok
                          (loop (cdr t)))))
                (arc:nextval-type? (cadr aval) keynm nextval))))))

(define (arc:nextval-type? type keynm nextval)
  (case type
    ((string) (arc:test-nextval-type string? "string" keynm nextval))
    ((char) (arc:test-nextval-type char? "char" keynm nextval))
    ((list) (arc:test-nextval-type list? "list" keynm nextval))
    ((integer) (arc:test-nextval-type integer? "integer" keynm nextval))
    ((proc) (arc:test-nextval-type procedure? "procedure" keynm nextval))
    ((symbol) (arc:test-nextval-type symbol? "symbol" keynm nextval))
    ((boolean) (arc:test-nextval-type boolean? "boolean" keynm nextval))
    ((stmt) (arc:test-nextval-type arc:find-stmt "statement" keynm nextval))
    ((strlist) (arc:test-nextval-type arc:string-list? "string-list" keynm nextval))
    ((strlist*) (arc:test-nextval-type arc:string-list? "string-list" keynm nextval))
    ((alist) (arc:test-nextval-type arc:alist? "alist" keynm nextval))
    ((dependencies) (arc:test-nextval-type arc:deps-alist? "dependencies" 
                                           keynm nextval))
    ((attrval) (arc:test-nextval-type arc:attrval? "attrvalue" keynm nextval))
    (else (string-append "bad keyword type '" (symbol->string type) "'"))))

(define (arc:test-nextval-type proc typenm keynm nextval)
  (if (not (apply proc (list nextval)))
      (string-append "bad value type for keyword '"
                     keynm 
                     "', " typenm " expected")
      ':ok))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
