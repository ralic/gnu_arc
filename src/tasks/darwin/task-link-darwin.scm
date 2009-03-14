;;  This file is part of the arc package
;;  Copyright (C) 2009 by Gregor Klinke
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

;; $Id: task-link-darwin.scm,v 1.1 2009/03/14 23:40:26 eyestep Exp $


(arc:provide 'task-link-darwin)

(arc:log 'debug "loading 'link' task [darwin]")

(arc:require 'task-link-generic "generic/task-link-generic")

;; backend functionality for linking object files on darwin systems.
(define <arc:link-darwin>
  (arc:make-class 
   '<arc:link-darwin>                   ; name of the class
   <arc:link-generic>                   ; superclass
   
   '()                                  ; slots
   
   ;; methods
   `((link-cmd ,(lambda (self) "gcc"))
     (outfile-flag ,(lambda (self) "-o"))
     (shared-flag ,(lambda (self) ""))
     (static-flag ,(lambda (self) ""))
     (nostdlib-flag ,(lambda (self) "-nostdlib"))
     
     ;; the default application extension (on windows: .exe)
     (app-ext ,(lambda (self) ""))

     (rpath-option ,(lambda (self rpath)
                      (cond
                       ((string? rpath)
                        (string-append "-Wl,-rpath," 
                                       (arc:path->string
                                        (arc:path-absolutize
                                         (arc:string->path rpath)))))
                       ((list? rpath)
                        (arc:string-list->string*
                         (arc:reduce (lambda (x lst)
                                       (cons (arc:path->string
                                              (arc:path-absolutize
                                               (arc:string->path x)))
                                             lst))
                                     '()
                                     rpath)
                         " -Wl,-rpath,"))
                       (else "")) ))
     )))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

