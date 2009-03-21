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

(define <arc:sys-unix>
  (arc:make-class 
   '<arc:sys-unix>                      ; name of the class
   <arc:sys-generic>                    ; superclass
   '()                                  ; slots
   
   ;; methods
   `((copy-file 
      ,(lambda (self file tofile)
         (case %arc:scheme-impl%
           ((guile) (if (file-exists? file)
                        (copy-file file tofile)
                        #f))
           ((scm asc ksi)
            (let ((cpcmd (string-append "cp -f " file " " tofile)))
              (= (system cpcmd) 0)))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     )))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
