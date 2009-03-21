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

(arc:provide 'task-c-compile-generic)

(arc:log 'debug "loading 'c-compile' task [generic]")


;; backend functionality for compilation and c file dependency control.
(define <arc:c-compile-generic>
  (arc:make-class 
   '<arc:c-compile-generic>             ; name of the class
   <arc:object>                         ; superclass
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

     ;; does this platform needs a separate compilation for shared objects?
     (need-shared-build ,(lambda (self) #t))
     ;; if so, use the following extra flags for compilation
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
     
     
     (make-objfile-name 
      ,(lambda (self filename outdir objext)
         (let* ((pn (arc:string->path filename))
                (on* (arc:path-replace-last-ext 
                      pn 
                      (or objext 
                          (self 'objfile-ext)))))
           (arc:path->string
            (if (and outdir (not (null? outdir)))
                (arc:path-append (arc:string->path outdir)
                                 (arc:path-last-comp on*))
                on*)))))
     
     
     (compile-file 
      ,(lambda (self sfile ofile cincs cflags)
         (arc:log 'debug "compile file " sfile " to " ofile)
         (let ((cmd-str (string-append 
                         (self 'compiler-cmd) " "
                         (self 'default-defs) " "
                         (self 'default-incls) " "
                         cincs " "                      ; custom includes
                         (self 'default-flags) " "
                         cflags " "                     ; custom cflags
                         (self 'compile-only-flag) " "  ; compile only
                         (self 'outfile-flag) " " 
                         ofile " "                      ; obj file
                         sfile     ; the source file
                         )))
           (arc:display cmd-str #\nl)
           (if (not (equal? (arc:sys 'system cmd-str) 0))
               (if (not %arc:keep-going-on-errors%)
                   (quit))))))
     
     
     (makedeps 
      ,(lambda (self sfile ofile cflags cincs)
         (let* ((bn (arc:path-last-comp (arc:string->path sfile)))
                (tdf (arc:path->string
                      (arc:path-append (arc:deps-directory)
                                       (arc:path-replace-last-ext bn "d"))))
                (dcmd (string-append (self 'makedeps-cmd) " "
                                     (self 'deps-flag) " "
                                     cflags " "
                                     cincs " "
                                     sfile " "
                                     "> " tdf)) )
           (arc:display dcmd #\nl)
           (if (equal? (arc:sys 'system dcmd) 0)
               (let* ((deps (arc:parse-make-deps-file tdf)))
                 ;; set the correct object target
                 (arc:deps-set-target! deps ofile)
                 (arc:sys 'remove-file tdf)
                 deps)
               #f))))
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

