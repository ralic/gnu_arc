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

(arc:provide 'task-link-linux)

(arc:log 'debug "loading 'link' task [linux]")

(arc:require 'task-link-generic "generic/task-link-generic")


;; backend functionality for linking object files on linux systems.
(define <arc:link-linux>
  (arc:make-class 
   '<arc:link-linux>                    ; name of the class
   <arc:link-generic>                   ; superclass
   
   '()                                  ; slots
   
   ;; methods
   `(
     )))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

