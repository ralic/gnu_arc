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
  (if (sys:file-exists? libnm)
      (sys:remove-file libnm))
  
  (let ((ar-cmd (self 'ar-cmd))
        (ar-args (append (list (self 'replace-create-flag) libnm) objs))
        (ranlib-cmd (self 'ranlib-cmd))
        (ranlib-args (list libnm)) )
    
    (arc:display ar-cmd " " (arc:string-list->string* ar-args " ") #\nl)
    
    (if (not (equal? (sys:execute ar-cmd ar-args) 0))
        (arc:msg "failed to create library " libnm #\nl)

        (if (self 'ranlib-needed?)
            (begin
              (arc:display ranlib-cmd " " 
                           (arc:string-list->string* ranlib-args " ") #\nl)
              (if (not (equal? (sys:execute ranlib-cmd ranlib-args) 0))
                  (arc:msg "failed to run ranlib on " libnm #\nl)))))
    libnm))




;; build a shared library.  This requires the object files to be combiled
;; properly
(define (arc:<lib-cygwin>-make-share-lib self libnm soname 
                                          objs libdirs deplibs
                                          rpath)
  (if (sys:file-exists? libnm)
      (sys:remove-file libnm))

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
         (dll1-cmd "dlltool")
         (dll1-args (append (list "--export-all" "--output-def" def) objs))
         (dll2-cmd "dllwrap")
         (dll2-args (append (append (list "-o " libnm
                                          "--dllname" dllname
                                          "--def" def)
                                    objs)
                            deplibs))
         (dll3-cmd "dlltool")
         (dll3-args (list "--def" def
                          "--dllname" dllname
                          "--output-lib" import-libname))
         )
    (arc:display dll1-cmd " " (arc:string-list->string* dll1-args " ") #\nl)
    
    (if (not (equal? (sys:execute dll1-cmd dll1-args) 0))
        (arc:throw 'exec 
                   (string-append "failed to create library " libnm
                                  " (dlltool)"))
        (begin
          (arc:display dll2-cmd " "
                       (arc:string-list->string* dll2-args " ") #\nl)
          (if (not (equal? (sys:execute dll2-cmd dll2-args) 0))
              (arc:throw 'exec 
                         (string-append "failed to create library " libnm
                                        " (dllwrap)"))
              (begin
                (arc:display dll3 " " 
                             (arc:string-list->string* dll3-args " ") #\nl)
                (if (not (equal? (sys:execute dll3-cmd dll3-args) 0))
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
     ;;     on (or '() if not needed)
     ;; #6: rpath
     (make-shared-lib ,arc:<lib-cygwin>-make-share-lib)
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
