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

(arc:provide 'task-lib-beos)

(arc:log 'debug "loading 'lib' task [beos]")

(arc:require 'task-lib-generic "generic/task-lib-generic")

(define <arc:lib-beos>
  (arc:make-class 
   '<arc:lib-beos>                      ; name of the class
   <arc:lib-generic>                    ; superclass
   '()                                  ; slots
   `((ar-cmd ,(lambda (self) "ar"))
     (replace-create-flag ,(lambda (self) "rc"))
     (ranlib-cmd ,(lambda (self) "ranlib"))
     (ranlib-needed? ,(lambda (self) #t))
     (suffix-shared ,(lambda (self) "so"))
     (suffix-static ,(lambda (self) "a"))
     
     (ld-cmd ,(lambda (self) "ld"))
     (ld-shared-flag ,(lambda (self) "-shared"))
     (ld-extra-flags ,(lambda (self) "--export-dynamic"))
     (ld-outfile-flag ,(lambda (self) "-o"))
     (ld-soname-flag ,(lambda (self soname)
                        (string-append "-soname=" soname)))
     (ld-rpath-option ,(lambda (self rpath)
                         (string-append "-rpath=" 
                                        (arc:path->string
                                         (arc:path-absolutize
                                          (arc:string->path rpath))))))
     )))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
