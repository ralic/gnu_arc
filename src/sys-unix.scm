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

;; $Id: sys-unix.scm,v 1.1 2003/04/19 01:03:35 eyestep Exp $

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
