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

;; $Id: task-version.scm,v 1.1 2003/04/15 00:03:27 eyestep Exp $

(arc:provide 'task-version)

(arc:log 'debug "loading 'version' task")

;; Takes a text file, extracts a build number from it, increases it by a
;; defined step and write the file back.  This task can be used to generate
;; an automatic build number counter.  The count part in the text file is
;; marked up by @
;;
;; Keywords:
;; src: STRING require
;; the version file
;;
;; seperator: CHAR
;; the seperator to use instead of @
;;
;; step: INTEGER
;; the step to count for, defaults to 1
;;
;; tofile: STRING
;; where to write the result, defaults to src:
;;
;; clean-copy-tofile: STRING
;; if set write a copy of the counted data to STRING, but removes the
;; seperator chars
;;
;; fill-to-length: INTEGER
;; left fill the generated build number at least to this length, use
;; the character of fill-char: for this purpose
;;
;; fill-char: CHAR
;; left-fill with this char, defaults to #\0.  Don't use number digits
;; here, since they modify the number as number (so no further increase
;; would be possible)
;;
;; RETURNS
;; <unspecified>

(define arc:version-keywords '((src            string required)
                               (tofile         string optional)
                               (clean-copy-tofile string optional)
                               (seperator      char optional)
                               (fill-to-length integer optional)
                               (fill-char      char optional)
                               (step           integer optional)))
(define (arc:version props body)
  (let* ((src (arc:aval 'src props #f))
         (tofile (arc:aval 'tofile props src))
         (clean-copy-tofile (arc:aval 'clean-copy-tofile props #f))
         (seperator (arc:aval 'seperator props #\@))
         (step (arc:aval 'step props 1))
         (fill-to-length (arc:aval 'fill-to-length props 4))
         (fill-char (arc:aval 'fill-char props #\0))
         (s-apt #f))
    
    (arc:log 'verbose "version: use version file '" src "' ...")
    (set! s-apt (arc:-version-file-scan src seperator))
    (set! s-apt (arc:-version-file-increase-counts s-apt step 
                                                   fill-to-length
                                                   fill-char))
    (arc:-version-file-write s-apt tofile seperator)

    (if clean-copy-tofile
        (arc:-version-file-write s-apt clean-copy-tofile #f))
    
    (arc:log 'debug "version: ok")
    '<unspecified>
    ))


(define (arc:-version-file-scan file boundary)
  (let* ((*in* (open-input-file file))
         (state 'expect-at)
         (buffer '())
         (struct '()))
    (if (not boundary)
        (set! boundary #\@))
    (do ((c (read-char *in*) (read-char *in*)))
        ((eof-object? c) 'done)
      (case state
        ((expect-at) (if (equal? c boundary)
                         (begin
                           ;; save the yet read stuff in to the struct
                           (set! struct 
                                 (append struct 
                                         (list (list->string (reverse buffer)))))
                           (set! state 'scan-count)
                           (set! buffer '()))
                         (set! buffer (cons c buffer))))
        ((scan-count) (if (equal? c boundary)
                          (begin
                            (set! struct 
                                  (append struct 
                                          (list (cons ':count (list->string (reverse buffer))))))
                            (set! state 'expect-at)
                            (set! buffer '()))
                          (set! buffer (cons c buffer))))
        ))
    (if (> (length buffer) 0)
        (set! struct 
              (append struct 
                      (list (list->string (reverse buffer))))))

    (close-port *in*)
    struct))

(define (arc:-version-file-increase-counts apt step
                                           left-fill-to-length fill-char)
  (map (lambda (x)
         (cond
          ((string? x) x)
          ((and (pair? x) 
                (eq? (car x) ':count))
           (cons ':count (arc:-version-file-inc-count (cdr x)
                                                      step 
                                                      left-fill-to-length
                                                      fill-char)))
          (else x)))
       apt))

(arc:register-task 'version arc:version arc:version-keywords)




(define (arc:-version-file-inc-count count-repr step
                                     left-fill-to-length fill-char)
  (if (not fill-char)
      (set! fill-char #\0))

  ;; check if the count contains only numbers
  (let ((num (string->number count-repr)))
    (if num
        (let* ((newnum (+ num step))
               (new-repr (number->string newnum)))
          (if (and left-fill-to-length
                   (> left-fill-to-length 0))
              (let* ((nrl (string-length new-repr)))
                (if (< nrl left-fill-to-length)
                    (string-append (make-string (- left-fill-to-length nrl)
                                                fill-char)
                                   new-repr)
                    new-repr))
              new-repr))
        count-repr)) )

(define (arc:-version-file-write apt file boundary)
  (let* ((*out* (open-output-file file)))
    (for-each (lambda (x)
                (cond
                  ((string? x) (display x *out*))
                  ((and (pair? x)
                        (eq? (car x) ':count))
                   (begin
                     (if boundary
                         (display (string boundary) *out*))
                     (display (cdr x) *out*)
                     (if boundary
                         (display (string boundary) *out*))))
                  (else 'ignore)))
              apt)
    (close-port *out*)))
      

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
