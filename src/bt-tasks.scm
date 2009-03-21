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

;; read the handler declarations
(for-each 
 (lambda (pn)
   (let* ((pth (arc:path-append (arc:string->path pn) "meta-inf"))
          (dir (if (arc:sys 'file-exists? (arc:path->string pth))
                   (arc:sys 'opendir (arc:path->string pth))
                   #f)))
     (if dir
         (begin
           (do ((fn (arc:sys 'readdir dir) (arc:sys 'readdir dir)))
               ((not fn) #t)
             (if (arc:string-suffix? fn ".decl")
                 (begin
                   (arc:log 'verbose "load '" (arc:path-append pth fn) "'")
                   (arc:load (arc:path->string (arc:path-append pth fn))))))
           (arc:sys 'closedir dir)))))
 %arc:arc-incl-path%)

;; load all tasks in the subdirectory "tasks"
(for-each 
 (lambda (pn)
   (let* ((pth (arc:path-append (arc:string->path pn) "tasks"))
          (dir (if (arc:sys 'file-exists? (arc:path->string pth))
                   (arc:sys 'opendir (arc:path->string pth))
                   #f)))
     (if dir
         (begin
           (do ((fn (arc:sys 'readdir dir) (arc:sys 'readdir dir)))
               ((not fn) #t)
             (if (and (arc:string-prefix? fn "task-")
                      (arc:string-suffix? fn ".scm"))
                 (arc:require (string->symbol 
                               (arc:path->string 
                                (arc:path-without-last-ext 
                                 (arc:string->path fn))))
                              (arc:path->string (arc:path-append pth fn)))))
           (arc:sys 'closedir dir)))))
 %arc:arc-incl-path%)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
