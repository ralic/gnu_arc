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

(arc:provide 'task-lib-generic)

(arc:log 'debug "loading 'lib' task [generic]")


;; generate the correct file names for this platform
(define (arc:<lib-generic>-make-static-name self outdir libnm)
  (let* ((bn (string-append "lib" libnm "." (self 'suffix-static))))
    (if outdir
        (arc:path->string (arc:path-append (arc:string->path outdir) bn))
        bn)) )


;; compile a static library
(define (arc:<lib-generic>-make-static-lib self libnm objs)
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


;; build a shared library.  This requires the object files to be compiled
;; properly
(define (arc:<lib-generic>-make-share-lib self libnm soname 
                                          objs libdirs deplibs
                                          rpath)
  (if (arc:sys 'file-exists? libnm)
      (arc:sys 'remove-file libnm))
  
  (let ((ldmd (string-append
               (self 'ld-cmd) " "
               (self 'ld-shared-flag) " "
               (self 'ld-extra-flags) " "
               (self 'ld-soname-flag soname) " "
               (if rpath
                   (string-append (self 'ld-rpath-option rpath) " ")
                   "")
               (if (and deplibs
                        (not (null? deplibs)))
                   (string-append (arc:string-list->string* libdirs " -L") " ")
                   "")
               ;; objects
               (arc:string-list->string* objs " ") " "
               ;; deplibs
               (if (and deplibs
                        (not (null? deplibs)))
                   (string-append (arc:string-list->string* deplibs " -l") " ")
                   "")
               (self 'ld-outfile-flag) " " libnm)) )

    (arc:display ldmd #\nl)
    
    (if (not (equal? (arc:sys 'system ldmd) 0))
        (begin
          (arc:msg "failed to create library " libnm #\nl)
          #f)
        libnm) ))


;; now concat the class
(define <arc:lib-generic>
  (arc:make-class 
   '<arc:lib-generic>                   ; name of the class
   <arc:object>                         ; superclass
   '()                                  ; slots
   
        
   ;; methods
   `((ar-cmd ,(lambda (self) "ar"))
     (replace-create-flag ,(lambda (self) "rc"))
     (ranlib-cmd ,(lambda (self) "ranlib"))
     (ranlib-needed? ,(lambda (self) #t))
     (suffix-shared ,(lambda (self) "so"))
     (suffix-static ,(lambda (self) "a"))

     (ld-cmd ,(lambda (self) "gcc"))
     (ld-shared-flag ,(lambda (self) "-shared"))
     (ld-extra-flags ,(lambda (self) "-Wl,--export-dynamic"))
     (ld-outfile-flag ,(lambda (self) "-o"))
     (ld-soname-flag ,(lambda (self soname) ""))
     (ld-rpath-option ,(lambda (self rpath)
                         (string-append "-Wl,-rpath=" 
                                        (arc:path->string
                                         (arc:path-absolutize
                                          (arc:string->path rpath))))))
     
     ;; make a name for a static library 
     ;; #1: the outdir
     ;; #2: the libname
     (make-static-name ,arc:<lib-generic>-make-static-name)

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
     (make-static-lib ,arc:<lib-generic>-make-static-lib)
     
     ;; make a shared library
     ;; #1: the (absolute) library's real name
     ;; #2: the soname (internal, without paths)
     ;; #3: the list of (shared) objects
     ;; #4: a list of library to look into for 
     ;;     dependency libs
     ;; #5: a list of libraries the shared library depends 
     ;;     on (or '() if not needed)
     ;; #6: rpath
     (make-shared-lib ,arc:<lib-generic>-make-share-lib)
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
