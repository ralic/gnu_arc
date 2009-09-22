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

;; some functions to handle implicit knowledge of the processor.

;; ----------------------------------------------------------------------
;; cleanup knowledge.
;;
;; tasks can register their stuff, which has to be cleaned up later,
;; (e.g. temporary directories, etc.)
;; ----------------------------------------------------------------------

;; this list contains pairs of paths and methods to be removed on a clean
;; run (see next description)
(define %arc:build-resources% '())

;; registers a new path on the list of cleanable elements.  Method
;; describes how to interpret the path information.  It could be on of the
;; symbols:
;;
;; 'in-place      the path information is a absolute path, remove only this
;;                exact path
;; 'recursive     remove every occurance of this (sub-)path, while 
;;                traversing the sub directory tree
;;
;; Or it could be a procedure taking one two arguments (the path and the 
;; working directory, from where the clean-process should be started)
(define (arc:register-built-resource path method)
  (set! %arc:build-resources% 
        (append  %arc:build-resources% (list (cons path method)))))



;; starts a clean run with "root" as root directory
(define (arc:clean-implicit root)
  (let* ((olddir (sys:getcwd)))
    (sys:change-dir root)
    (let loop ((ce %arc:build-resources%))
      (if (null? ce)
          #t
          (begin
            (cond 
             ((and (symbol? (cdar ce))
                   (equal? (cdar ce) 'recursive))
              (arc:-clean-recursive-clean root (caar ce)) )
             ((and (symbol? (cdar ce))
                   (equal? (cdar ce) 'in-place))
              (arc:-clean-inplace-clean root (caar ce)))
             ((procedure? (cdar ce))
              (arc:-clean-apply-clean root (caar ce) (cdar ce)))
             (else #t))
            (loop (cdr ce)))))
    (sys:change-dir root)))
                 
(define (arc:-clean-recursive-clean root path)
  (let* ((rp (arc:string->path path))
         (dl '()) )
    (arc:traverse-dir 
     root
     (lambda (kind fn)
       (case kind
         ((:dir) (set! dl (append dl (list (arc:path->string
                                            (arc:path-append fn rp))))))
         ((:file) 'ignore))))
    (let loop ((d dl))
      (if (null? d)
          'done
          (begin
            (arc:-clean-recursive-clean-res (car d))
            (loop (cdr d)))))
    (arc:-clean-recursive-clean-res path)))

(define (arc:-clean-recursive-clean-res path)
  (if (sys:file-directory? path)
      (sys:remove-dir path)
      (if (sys:file-exists? path)
          (sys:remove-file path))))
  

(define (arc:-clean-inplace-clean root path)
  (if (sys:file-directory? path)
      (sys:remove-dir path)
      (if (sys:file-exists? path)
          (sys:remove-file path))))

(define (arc:-clean-apply-clean root path method)
  (apply method (list path root)))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
