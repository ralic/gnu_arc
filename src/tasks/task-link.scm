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

;; $Id: task-link.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $

(arc:provide 'task-link)


;; gcc specific settings
(define %arc:link-command% "gcc")
(define %arc:link-outfile-flag% "-o")
(define %arc:link-shared-flag% "")
(define %arc:link-static-flag% "-static")
(define %arc:link-nostdlib-flag% "-nostdlib")

(arc:log 'debug "loading 'link' task")

;; Creates an application/executable by linking object files and libraries.
;; Only compatible object files may be linked.
;;
;; :files STRING-LIST
;; a list of object files to link into the executable
;;
;; :libs STRING-LIST
;; A list of libraries to link with the executable.  The entries has to
;; give the "base library name" only, so for e.g.  for "libAbcX.so" give
;; "AbcX".  This task looks in the places given by ":libdirs" for libraries
;; and shared objects
;;
;; :shared? BOOLEAN
;; if #t, use shared libraries for the executable.  If #f, try to avoid
;; linking against shared libraries, but build a static application.
;;
;; :appnm STRING
;; the executable name to use.  This should not include probably necessary
;; name extensions, used to indicate executable files on some platforms.
;; 
;; :appext STRING
;; the extension to use for the application.  This is only used if an
;; unusal executable extension should be used.

;; :outdir STRING
;; the directory to build the executable in
;;
;; :libdirs STRING-LIST
;; A list of directories where to look for depending shared
;; objects/libraries.  Give only the directory itself (without any
;; additional option, e.g. without "-L" for gcc)
;;
;; :nostdlib BOOLEAN
;; if #t, don't use the standard system libraries and startup files when
;; linking.  Only the files you specify will be passed to the linker.
;; Default is #f
;;
;; RETURNS
;; the name of the build application (string)

(define arc:link-keywords '((files (strlist attrval) required)
                            (libs strlist optional)
                            (shared? boolean optional)
                            (appnm string required)
                            (appext string optional)
                            (outdir string optional)
                            (nostdlib? boolean optional)
                            (libdirs strlist optional)) )
(define (arc:link props body)
  (let* ((shared (arc:aval 'shared? props #t))
         (files* (arc:aval 'files props ()))
         (files (if (arc:attrval? files*)
                    (if shared
                        (arc:attrval-ref files* 'shared-objs)
                        (arc:attrval-ref files* 'objs))
                    files*))
         (autolibs (if (arc:attrval? files*)
                       (arc:attrval-ref files* 'dep-libs)
                       #f))
         (autolibdirs (if (arc:attrval? files*)
                          (arc:attrval-ref files* 'dep-lib-dirs)
                          #f))
         (libs (arc:aval 'libs props ())) 
         (appext (arc:aval 'appext props (arc:default-appl-extension)))
         (appnm (arc:aval 'appnm props ""))
         (outdir (arc:aval 'outdir props #f))
         (nostdlib (arc:aval 'nostdlib? props #f))
         (libdirs (arc:aval 'libdirs props ())) 
         )
    (if (= (string-length appnm) 0)
        (arc:log 'fatal "bad or empty application name"))
    
    (if (and (= (length files) 0)
             (= (length libs) 0))
        (arc:log 'info "no object files/libs for executable!"))
    
    (let* ((fullnm (arc:-link-make-app-name outdir appnm appext))
           (linkcmd (string-append 
                     %arc:link-command% " "
                     (if libdirs
                         (string-append (arc:string-list->string* libdirs "-L")
                                        " ")
                         "")
                     (if autolibdirs
                         (string-append (arc:string-list->string* autolibdirs 
                                                                  "-L")
                                        " ")
                         "")
                     (if shared
                         (string-append %arc:link-shared-flag% " ")
                         (string-append %arc:link-static-flag% " "))
                     (if nostdlib
                         (string-append %arc:link-nostdlib-flag% " ")
                         "")
                     %arc:link-outfile-flag% " " fullnm " "
                     (if files
                         (string-append (arc:string-list->string files) " ")
                         "")
                     (if autolibs
                         (string-append (arc:string-list->string* autolibs 
                                                                  "-l")
                                        " ")
                         "")
                     (if libs
                         (string-append (arc:string-list->string* libs "-l")
                                        " ")
                         "") )))

      (arc:log 'debug "linking " fullnm " ...")

      (arc:display linkcmd #\nl)

      (if (not (= (arc:sys.system linkcmd) 0))
          (arc:log 'info "linking '" fullnm "' failed"))
      
      fullnm) ))


(arc:register-task 'link arc:link arc:link-keywords)


(define (arc:-link-make-app-name outdir appnm appext)
  (let* ((od (if outdir 
                 (arc:string->path outdir) 
                 ()))
         (ap (if (and appext
                      (> (string-length appext) 0))
                 (arc:path-replace-last-ext (arc:string->path appnm) appext)
                 (arc:path-without-last-ext (arc:string->path appnm)))) )
    (arc:path->string (arc:path-append od ap))))


(define (arc:default-appl-extension)
  (case (car %arc:sysnm%)
    ((win32) "exe")
    (else "")))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
