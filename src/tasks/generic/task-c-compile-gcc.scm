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

;; $Id: task-c-compile-gcc.scm,v 1.1 2003/04/13 23:47:34 eyestep Exp $


(arc:provide 'task-c-compile-gcc)

(arc:log 'debug "loading 'c-compile' task [gcc]")

(arc:require 'task-c-compile-generic "generic/task-c-compile-generic")


;; backend functionality for compilation and c file dependency control.
(define <arc:c-compile-gcc>
  (arc:make-class 
   '<arc:c-compile-gcc>                 ; name of the class
   <arc:c-compile-generic>              ; superclass
   '()                                  ; slots
   
   ;; methods
   `(
     ;; compiler command
     (compiler-cmd ,(lambda (self) "gcc"))
     
     ;; makedeps command
     (makedeps-cmd ,(lambda (self) "gcc"))

     ;; default definitions
     (default-defs ,(lambda (self) ""))
     
     ;; default includes
     (default-incls ,(lambda (self) ""))
     
     ;; default flags
     (default-flags ,(lambda (self) ""))
     
     ;; flag for compiling dependencies
     (deps-flag ,(lambda (self) "-M"))
     
     ;; returns the compiler flag for optimization
     (opt-level-flag ,(lambda (self level)
                        (case level
                          ((high) "-O3")
                          ((medium) "-O2")
                          ((low) "")
                          (else ""))))
     (ansi-flag ,(lambda (self) "-ansi"))
     (debug-flag ,(lambda (self) "-g"))
     (signed-char-flag ,(lambda (self) "-signed-char"))
     (unsigned-char-flag ,(lambda (self) "-unsigned-char"))
     
     ;; most definitly, this must be overriden for various platforms
     (shared-obj-flag ,(lambda (self) "-fpic -DPIC"))
     
     (warn-level-flag 
      ,(lambda (self level)
         (case level
           ((high) "-Wall")
           ((medium) (string-append "-Wqual "
                                    "-Wmissing-prototypes "
                                    "-Wimplicit "
                                    "-Winline "
                                    "-Wredundant-decls "
                                    "-Wformat "
                                    "-Wenum-clash "
                                    "-Wuninitialized"))
           ((low) "-Wcast-qual -Wmissing-prototypes")
           (else ""))))
     
     (outfile-flag ,(lambda (self) "-o"))
     (compile-only-flag ,(lambda (self) "-c"))
     
     (objfile-ext ,(lambda (self) "o"))
     (shared-objfile-ext ,(lambda (self) "lo"))
     
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

