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

(define (sys:mkdirs path)
  (let ((dirnm (arc:string->path path)))
    (let loop ((pc 0))
      (if (>= pc (arc:path-length dirnm))
          #f
          (let ((aps (arc:path->string (arc:path-subpath dirnm 0
                                                         (+ pc 1)))))
            (if (not (sys:file-exists? aps))
                (if (not (sys:mkdir aps))
                    (begin
                      (arc:log 'error 
                               "failed to create directory '" aps "'")
                      #f)
                    (begin
                      (arc:log 'debug "directory '" aps "' created")
                      (loop (+ pc 1))))
                (loop (+ pc 1))))))))


(define (sys:remove-dir path)
  (arc:traverse-dir path (lambda (kind fn)
                           (case kind
                             ((:dir) (begin
                                       (arc:log 'debug "remove dir '" 
                                                (arc:path->string fn) 
                                                "'")
                                       (sys:rmdir (arc:path->string fn))))
                             ((:file) (sys:remove-file (arc:path->string fn))))))
  (sys:rmdir path))


(define (sys:make-symlink from to)
  (if (sys:file-exists? to)
      (sys:remove-file to))
  (sys:symlink from to)
  #t)
          

(define (sys:change-dir path)
  (arc:log 'debug "change to path '" path "'")
  (sys:chdir path))
           

(define (sys:copy-dir dir todir)
  (if (sys:file-exists? dir)
      (let* ((dp (arc:string->path todir))
             (srcp (arc:string->path dir))
             (srcp-brl (arc:path-length
                        (arc:path-without-last-comp srcp))))
        (arc:traverse-dir 
         dir
         (lambda (kind fn)
           (let* ((pl (arc:path-length fn))
                  (sp (arc:path-subpath fn srcp-brl pl))
                  (tp (arc:path-append dp sp)))
             (case kind
               ((:dir) (sys:mkdirs (arc:path->string tp)))
               ((:file) 
                (begin
                  (sys:mkdirs (arc:path->string (arc:path-without-last-comp tp)))
                  (sys:copy-file (arc:path->string fn)
                                 (arc:path->string tp))))))))
        #t)
      #f))

(define (sys:execute* cmd args)
  (let ((a (if (list? args) 
               (cons cmd args)
               (cons cmd (vector->list args)) )))
    (sys:system "/bin/sh" (list->vector a))))

(define (sys:execute cmd args)
  (let ((a (if (list? args) 
               (list->vector args)
               args)))
    (sys:system cmd a)))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
