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

(arc:provide 'task-link-cygwin)

(arc:log 'debug "loading 'link' task [cygwin]")

(arc:require 'task-link-generic "generic/task-link-generic")

;; backend functionality for linking object files on linux systems.
(define <arc:link-cygwin>
  (arc:make-class 
   '<arc:link-cygwin>                   ; name of the class
   <arc:link-generic>                   ; superclass

   '()                                  ; slots
   
   ;; methods
   `((link-cmd ,(lambda (self) "gcc"))
     (outfile-flag ,(lambda (self) '("-o")))
     (shared-flag ,(lambda (self) '()))
     (static-flag ,(lambda (self) '("-static")))
     (nostdlib-flag ,(lambda (self) '("-nostdlib")))
     
     ;; the default application extension (on windows: .exe)
     (app-ext ,(lambda (self) ".exe"))

     (rpath-option ,(lambda (self rpath)
                      (cond
                       ((string? rpath)
                        (string-append "-Wl,-rpath," 
                                       (arc:path->string
                                        (arc:path-absolutize
                                         (arc:string->path rpath)))))
                       ((list? rpath)
                        (arc:reduce (lambda (x lst)
                                      (cons (string-append "-Wl,-rpath," 
                                                           (arc:path->string
                                                            (arc:path-absolutize
                                                             (arc:string->path x))))
                                            lst))
                                     '()
                                     rpath))
                       (else "")) ))

     ;; link a set of object files
     (link-app
      ,(lambda (self outdir appnm appext 
                     libdirs autolibdirs shared nostdlib files autolibs libs
                     rpath frameworks)
         (let* ((fullnm (self 'make-app-name outdir appnm appext))
                (link-cmd "gcc")
                (link-args (hea:list-appends 
                            (if libdirs
                                (arc:annotate-list libdirs "-L")
                                '())
                            (if autolibdirs
                                (arc:annotate-list autolibdirs "-L")
                                '())
;;;                          (if shared
;;;                              (self 'shared-flag)
;;;                              (self 'static-flag))
;;;                          (if (and shared rpath)
;;;                              (self 'rpath-option rpath)
;;;                              '())
                            (if nostdlib
                                (list (self 'nostdlib-flag))
                                '())
                            (self 'outfile-flag) fullnm
                            (if files
                                files
                                '())
                            (if autolibs
                                (arc:annotate-list autolibs "-l")
                                '())
                            (if libs
                                (arc:annotate-list libs "-l")
                                '())
                            )) )
           (arc:log 'debug "linking " fullnm " ...")
           
           (arc:display-command link-cmd link-args)
           
           (if (not (equal? (sys:execute link-cmd link-args) 0))
               (arc:log 'info "linking '" fullnm "' failed"))
           
           fullnm)) )
     )))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:

