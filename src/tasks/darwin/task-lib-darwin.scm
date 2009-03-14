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

;; $Id: task-lib-darwin.scm,v 1.1 2009/03/14 23:40:21 eyestep Exp $

(arc:provide 'task-lib-darwin)

(arc:log 'debug "loading 'lib' task [darwin]")

(arc:require 'task-lib-generic "generic/task-lib-generic")

(define <arc:lib-darwin>
  (arc:make-class 
   '<arc:lib-darwin>                    ; name of the class
   <arc:lib-generic>                    ; superclass
   '()                                  ; slots
   `((ar-cmd              ,(lambda (self) "ar"))
     (replace-create-flag ,(lambda (self) "rc"))
     (ranlib-cmd          ,(lambda (self) "ranlib"))
     (ranlib-needed?      ,(lambda (self) #f))
     (suffix-shared       ,(lambda (self) "dylib"))
     (suffix-static       ,(lambda (self) "a"))
     
     (ld-cmd              ,(lambda (self) "gcc"))
     (ld-shared-flag      ,(lambda (self) "-dynamiclib"))
     (ld-extra-flags      ,(lambda (self) ""))
     (ld-outfile-flag     ,(lambda (self) "-o"))
     (ld-soname-flag      ,(lambda (self soname) ""))
     (ld-rpath-option     ,(lambda (self rpath) " "))
     
    )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
