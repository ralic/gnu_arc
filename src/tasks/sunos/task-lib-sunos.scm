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

;; $Id: task-lib-sunos.scm,v 1.1 2003/04/24 23:59:35 eyestep Exp $

(arc:provide 'task-lib-sunos)

(arc:log 'debug "loading 'lib' task [sunos]")

(arc:require 'task-lib-generic "generic/task-lib-generic")

(define (arc:<lib-sunos>-make-shared-names self outdir libnm 
                                             vermajor verminor verrelease)
  (let* ((fvp (if vermajor 
                  (string-append 
                   "." (arc:num/str vermajor)
                   (if verminor
                       (string-append 
                        "." (arc:num/str verminor)
                        (if verrelease
                            (string-append
                             "." (arc:num/str verrelease))
                            ""))
                       ""))
                  ""))
         (svp (if vermajor 
                  (string-append "." (arc:num/str vermajor))
                  ""))
         (linknm (string-append "lib" libnm "."
                                (self 'suffix-shared)))
         (realnm (string-append linknm fvp))
         (soname (string-append linknm svp))
         )
    (if outdir
        (let ((rpath (arc:string->path outdir)))
          (list (arc:path->string (arc:path-append rpath realnm))
                (arc:path->string (arc:path-append rpath soname))
                (arc:path->string (arc:path-append rpath linknm))
                soname))
        (list realnm soname linknm soname)) ))

(define <arc:lib-sunos>
  (arc:make-class 
   '<arc:lib-sunos>                     ; name of the class
   <arc:lib-generic>                    ; superclass
   '()                                  ; slots
   `((ar-cmd ,(lambda (self) "ar"))
     (replace-create-flag ,(lambda (self) "rc"))
     (ranlib-cmd ,(lambda (self) "ranlib"))
     (ranlib-needed? ,(lambda (self) #t))
     (suffix-shared ,(lambda (self) "so"))
     (suffix-static ,(lambda (self) "a"))
     
     (ld-cmd ,(lambda (self) "gcc"))
     (ld-shared-flag ,(lambda (self) "-shared"))
     (ld-extra-flags ,(lambda (self) ""));;-Wl,--export-dynamic"))
     (ld-outfile-flag ,(lambda (self) "-o"))
     (ld-soname-flag ,(lambda (self soname)
                        (string-append "-Wl,-h," soname)))
     (ld-rpath-option ,(lambda (self rpath)
                         (string-append "-Wl,-rpath," 
                                        (arc:path->string
                                         (arc:path-absolutize
                                          (arc:string->path rpath))))))
     
     (make-shared-names ,arc:<lib-sunos>-make-shared-names)
     
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
