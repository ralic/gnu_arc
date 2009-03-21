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

(arc:provide 'task-link-generic)

(arc:log 'debug "loading 'link' task [generic]")


;; backend functionality for linking object files on linux systems.
(define <arc:link-generic>
  (arc:make-class 
   '<arc:link-generic>                  ; name of the class
   <arc:object>                         ; superclass

   '()                                  ; slots
   
   ;; methods
   `((link-cmd ,(lambda (self) "gcc"))
     (outfile-flag ,(lambda (self) "-o"))
     (shared-flag ,(lambda (self) ""))
     (static-flag ,(lambda (self) "-static"))
     (nostdlib-flag ,(lambda (self) "-nostdlib"))
     
     ;; the default application extension (on windows: .exe)
     (app-ext ,(lambda (self) ""))

     (rpath-option ,(lambda (self rpath)
                      (cond
                       ((string? rpath)
                        (string-append "-Wl,-rpath," 
                                       (arc:path->string
                                        (arc:path-absolutize
                                         (arc:string->path rpath)))))
                       ((list? rpath)
                        (arc:string-list->string*
                         (arc:reduce (lambda (x lst)
                                       (cons (arc:path->string
                                              (arc:path-absolutize
                                               (arc:string->path x)))
                                             lst))
                                     '()
                                     rpath)
                         " -Wl,-rpath,"))
                       (else "")) ))
     
     ;; make the name for an application
     (make-app-name 
      ,(lambda (self outdir appnm appext*)
         (let* ((appext (or appext*
                            (self 'app-ext)))
                (od (if outdir 
                        (arc:string->path outdir) 
                        ()))
                (ap (if (and appext
                             (> (string-length appext) 0))
                        (arc:path-replace-last-ext (arc:string->path appnm) 
                                                   appext)
                        (arc:path-without-last-ext (arc:string->path appnm)))) )
           (arc:path->string (arc:path-append od ap)))))
     
     ;; link a set of object files
     (link-app
      ,(lambda (self outdir appnm appext 
                     libdirs autolibdirs shared nostdlib files autolibs libs
                     rpath)
         (let* ((fullnm (self 'make-app-name outdir appnm appext))
                (linkcmd (string-append 
                          (self 'link-cmd) " "
                          (if libdirs
                              (string-append (arc:string-list->string* libdirs
                                                                       " -L")
                                             " ")
                              "")
                          (if autolibdirs
                              (string-append (arc:string-list->string* 
                                              autolibdirs " -L")
                                             " ")
                              "")
                          (if shared
                              (string-append (self 'shared-flag) " ")
                              (string-append (self 'static-flag) " "))
                          (if (and shared
                                   rpath)
                              (string-append (self 'rpath-option rpath) " ")
                              "")
                          (if nostdlib
                              (string-append (self 'nostdlib-flag) " ")
                              "")
                          (self 'outfile-flag) " " fullnm " "
                          (if files
                              (string-append (arc:string-list->string* files
                                                                       " ")
                                             " ")
                              "")
                          (if autolibs
                              (string-append (arc:string-list->string* autolibs
                                                                       " -l")
                                             " ")
                              "")
                          (if libs
                              (string-append (arc:string-list->string* 
                                              libs " -l")
                                             " ")
                              "") )))
           (arc:log 'debug "linking " fullnm " ...")

           (arc:display linkcmd #\nl)

           (if (not (= (arc:sys 'system linkcmd) 0))
               (arc:log 'info "linking '" fullnm "' failed"))
      
           fullnm)) )
     
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

