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

;; $Id: task-lib-linux.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $

(arc:provide 'task-lib-linux)

(arc:log 'debug "loading 'lib-linux' task")


;; generate the correct file names for this platform
(define (arc:<linux-lib>-make-static-name self outdir libnm)
  (let* ((bn (string-append "lib" libnm "." (self 'suffix-static))))
    (if outdir
        (arc:path->string (arc:path-append (arc:string->path outdir) bn))
        bn)) )

(define (arc:<linux-lib>-make-shared-name self outdir libnm 
                                          vercur verrev verage)
  (let* ((vp (if (and vercur verrev verage)
                 (string-append "." (arc:num/str vercur) 
                                "." (arc:num/str verrev) 
                                "." (arc:num/str verage))
                 ""))
         (bn (string-append "lib" libnm "." 
                            (self 'suffix-shared)
                            vp)) )
    (if outdir
        (arc:path->string (arc:path-append (arc:string->path outdir) bn))
        bn)))


;; compile a static library
(define (arc:<linux-lib>-make-static-lib self libnm objs)
  (if (arc:sys.file-exists? libnm)
      (arc:sys.remove-file libnm))
  
  (let ((arcmd (string-append (self 'ar-command) " "
                              (self 'replace-create-flag) " "
                              libnm " "
                              (arc:string-list->string* objs " ")))
        (ranlibcmd (string-append (self 'ranlib-command) " " libnm)) )
    
    (arc:display arcmd #\nl)
    
    (if (not (equal? (arc:sys.system arcmd) 0))
        (arc:msg "failed to create library " libnm #\nl)

        (if (self 'ranlib-needed?)
            (begin
              (arc:display ranlibcmd #\nl)
              (if (not (equal? (arc:sys.system arcmd) 0))
                  (arc:msg "failed to run ranlib on " libnm #\nl)))))
    libnm))


;; build a shared library.  This requires the object files to be combiled
;; properly
(define (arc:<linux-lib>-make-share-lib self libnm objs libdirs deplibs)
  (if (arc:sys.file-exists? libnm)
      (arc:sys.remove-file libnm))
  
  (let ((ldmd (string-append 
               "gcc -shared -Wl,--export-dynamic "
               (if (and deplibs
                        (not (null? deplibs)))
                   (string-append (arc:string-list->string* libdirs "-L") " ")
                   "")
               ;; objects
               (arc:string-list->string* objs " ") " "
               ;; deplibs
               (if (and deplibs
                        (not (null? deplibs)))
                   (string-append (arc:string-list->string* deplibs "-l") " ")
                   "")
               "-o " libnm)) )

    (arc:display ldmd #\nl)
    
    (if (not (equal? (arc:sys.system ldmd) 0))
        (begin
          (arc:msg "failed to create library " libnm #\nl)
          #f)
        libnm) ))


;; now concat the class
(define <arc:linux-lib>
  (list '<arc:linux-lib>                ; name of the class
        '((os linux))                   ; slots

        <arc:object>                    ; superclass
        
        ;; methods
        `((os ,(lambda (self) linux))
          (ar-command ,(lambda (self) "ar"))
          (replace-create-flag ,(lambda (self) "rc"))
          (ranlib-command ,(lambda (self) "ranlib"))
          (ranlib-needed? ,(lambda (self) #t))
          (suffix-shared ,(lambda (self) "so"))
          (suffix-static ,(lambda (self) "a"))
                     
          ;; make a name for a static library 
          ;; #1: the outdir
          ;; #2: the libname
          (make-static-name ,arc:<linux-lib>-make-static-name)
          
          ;; make a name for a shared library
          ;; #1: the outdir
          ;; #2: the libname
          ;; #3: the version current
          ;; #4: the version revision
          ;; #5: the version age
          (make-shared-name ,arc:<linux-lib>-make-shared-name)
          
          ;; make a name for a shared library without version
          ;; information
          ;; #1: the outdir
          ;; #2: the libname
          (make-shared-name-no-version 
           ,(lambda (self outdir libnm)
              (arc:<linux-lib>-make-shared-name self
                                                outdir libnm
                                                #f #f #f)))
          
          ;; create static library
          ;; #1: the (absolute) libraryname 
          ;; #2: the list of object files
          (make-static-lib ,arc:<linux-lib>-make-static-lib)
          
          ;; make a shared library
          ;; #1: the (absolute) library name
          ;; #2: the list of (shared) objects
          ;; #3: a list of library to look into for 
          ;;     dependency libs
          ;; #4: a list of libraries the shared library depends 
          ;;     on (or () if not needed)
          (make-shared-lib ,arc:<linux-lib>-make-share-lib)
          )))
