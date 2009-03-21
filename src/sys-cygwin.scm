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

(define <arc:sys-cygwin>
  (arc:make-class 
   '<arc:sys-cygwin>                    ; name of the class
   <arc:sys-unix>                       ; superclass
   '()                                  ; slots
   
   ;; methods
   `((file-executable? 
      ,(lambda (self fn)
         (if (file-exists? fn)
             (or (arc:string-suffix? fn ".com")
                 (arc:string-suffix? fn ".exe")
                 (arc:string-suffix? fn ".bat")
                 (arc:string-suffix? fn ".cmd")
                 (> (arc:logand (vector-ref (stat fn) 2) #o111) 0))
             #f)))
     )))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
