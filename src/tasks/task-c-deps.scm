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

;; $Id: task-c-deps.scm,v 1.3 2003/04/22 23:40:24 eyestep Exp $

(arc:provide 'task-c-deps)

(arc:require 'task-c-compile "tasks/task-c-compile")

(arc:log 'debug "loading 'c-deps' task")

(define arc:c-deps-keywords '((sources strlist required)
                              (outdir string optional)
                              (flags strlist optional)
                              (includes strlist optional)
                              (objext string optional)))

(define (arc:c-deps props body)
  (let* ((<backend> ((arc:handler-factory %arc:sysnm% 'task-c-deps) 'alloc))
         (objext (arc:aval 'objext props (<backend> 'objfile-ext)))
         (outdir (arc:aval 'outdir props "."))
         (cincs (arc:string-list->string* (arc:aval 'includes props ()) 
                                          " -I"))
         (cflags (arc:string-list->string* (arc:aval 'flags props ())
                                           " "))
         (sources (arc:-prepare-c-source-list (arc:aval 'sources props ()))))
    (map
     (lambda (src)
       (let ((target (<backend> 'make-objfile-name src outdir objext)))
         (arc:log 'debug "dependency for: " target)

         (arc:deps-get-deps src target 
                            (lambda (sfile target) 
                              (<backend> 'makedeps 
                                         sfile target
                                         cflags
                                         cincs)))))
     
     sources)))

(arc:register-task 'c-deps arc:c-deps arc:c-deps-keywords)





;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
