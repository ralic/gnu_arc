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

;; $Id: task-lib-cygwin.scm,v 1.1 2003/04/23 18:46:42 eyestep Exp $

(arc:provide 'task-lib-cygwin)

(arc:log 'debug "loading 'lib' task [cygwin]")


;; generate the correct file names for this platform
(define (arc:<lib-cygwin>-make-static-name self outdir libnm)
  (let* ((bn (string-append "lib" libnm "." (self 'suffix-static))))
    (if outdir
        (arc:path->string (arc:path-append (arc:string->path outdir) bn))
        bn)) )


;; compile a static library
(define (arc:<lib-cygwin>-make-static-lib self libnm objs)
  (if (arc:sys 'file-exists? libnm)
      (arc:sys 'remove-file libnm))
  
  (let ((arcmd (string-append (self 'ar-cmd) " "
                              (self 'replace-create-flag) " "
                              libnm " "
                              (arc:string-list->string* objs " ")))
        (ranlibcmd (string-append (self 'ranlib-cmd) " " libnm)) )
    
    (arc:display arcmd #\nl)
    
    (if (not (equal? (arc:sys 'system arcmd) 0))
        (arc:msg "failed to create library " libnm #\nl)

        (if (self 'ranlib-needed?)
            (begin
              (arc:display ranlibcmd #\nl)
              (if (not (equal? (arc:sys 'system arcmd) 0))
                  (arc:msg "failed to run ranlib on " libnm #\nl)))))
    libnm))




;; build a shared library.  This requires the object files to be combiled
;; properly
(define (arc:<lib-cygwin>-make-share-lib self libnm soname 
                                          objs libdirs deplibs
                                          rpath)
  (if (arc:sys 'file-exists? libnm)
      (arc:sys 'remove-file libnm))

  (let* ((libnmp (arc:string->path libnm))
         (def (arc:path->string
               (arc:path-replace-last-ext
                libnmp
                "def")))
         (dllname (arc:path->string
                   (arc:path-last-comp
                    libnmp)))
         (import-libname (arc:path->string
                          (arc:path-replace-last-ext
                           libnmp "a")))
;         (import-libname (arc:path->string
;                          (arc:path-append-ext
;                           (arc:path-append
;                            (arc:path-without-last-comp libnmp)
;                            (string-append
;                             (arc:path->string
;                              (arc:path-without-last-ext 
;                               (arc:path-last-comp libnmp)))
;                             "_loader"))
;                           "a")))
         (dll1 (string-append 
                "dlltool --export-all "
                "--output-def " def
                " "
                (arc:string-list->string* objs " ")))
         (dll2 (string-append
                "dllwrap -o " libnm " "
                "--dllname " dllname " "
                "--def " def " "
                (arc:string-list->string* objs " ") " "
                (arc:string-list->string* deplibs " ")))
         (dll3 (string-append
                "dlltool --def " def " "
                "--dllname " dllname " "
                "--output-lib " import-libname))
         )
    (arc:display dll1 #\nl)
    
    (if (not (equal? (arc:sys 'system dll1) 0))
        (arc:throw 'exec 
                   (string-append "failed to create library " libnm
                                  " (dlltool)"))
        (begin
          (arc:display dll2 #\nl)
          (if (not (equal? (arc:sys 'system dll2) 0))
              (arc:throw 'exec 
                         (string-append "failed to create library " libnm
                                        " (dllwrap)"))
              (begin
                (arc:display dll3 #\nl)
                (if (not (equal? (arc:sys 'system dll3) 0))
                    (arc:throw 'exec 
                               (string-append "failed to create library " libnm
                                              " (loader)"))
                    libnm)))))
    ))


;; now concat the class
(define <arc:lib-cygwin>
  (arc:make-class 
   '<arc:lib-cygwin>                   ; name of the class
   <arc:object>                         ; superclass
   '()                                  ; slots
   
        
   ;; methods
   `((ar-cmd ,(lambda (self) "ar"))
     (replace-create-flag ,(lambda (self) "rc"))
     (ranlib-cmd ,(lambda (self) "ranlib"))
     (ranlib-needed? ,(lambda (self) #t))
     (suffix-shared ,(lambda (self) "dll"))
     (suffix-static ,(lambda (self) "a"))

     (ld-cmd ,(lambda (self) "dlltool"))
     
     ;; make a name for a static library 
     ;; #1: the outdir
     ;; #2: the libname
     (make-static-name ,arc:<lib-cygwin>-make-static-name)

     ;; returns a list with four different name forms of a shared
     ;; libraries.  car: the library's real name, incl. full version info;
     ;; cadr: the library's soname; caddr: the library's linker name;
     ;; cadddr: the internal soname (without path information)
     (make-shared-names 
      ,(lambda (self outdir libnm vermajor verminor verrelease)
         (let* ((name (string-append "lib" libnm "." (self 'suffix-shared)))
                (full (arc:path->string (arc:path-append 
                                         (arc:string->path outdir)
                                         name))))
           (list full full full name))))
     
     ;; create static library
     ;; #1: the (absolute) libraryname 
     ;; #2: the list of object files
     (make-static-lib ,arc:<lib-cygwin>-make-static-lib)
     
     ;; make a shared library
     ;; #1: the (absolute) library's real name
     ;; #2: the soname (internal, without paths)
     ;; #3: the list of (shared) objects
     ;; #4: a list of library to look into for 
     ;;     dependency libs
     ;; #5: a list of libraries the shared library depends 
     ;;     on (or () if not needed)
     ;; #6: rpath
     (make-shared-lib ,arc:<lib-cygwin>-make-share-lib)
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
