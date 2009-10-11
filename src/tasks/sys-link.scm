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

(arc:provide 'sys-link)

(define-generic (arc:link:link-cmd))

(define-generic (arc:link:outfile-flag))
(define-generic (arc:link:shared-flag))
(define-generic (arc:link:static-flag))
(define-generic (arc:link:nostdlib-flag))
     
;; the default application extension (on windows: .exe)
(define-generic (arc:link:app-ext))

(define-generic (arc:link:rpath-option))
(define-generic (arc:link:format-rpath-option rpath))
     
;; make the name for an application
(define-generic (arc:link:make-app-name outdir appnm appext*))
     
(define-generic (arc:link:format-framework-options frameworks))

;; link a set of object files
(define-generic (arc:link:link-app outdir appnm appext 
                                   libdirs autolibdirs shared nostdlib files
                                   autolibs libs rpath frameworks))

(define-generic (arc:link:needs-relink? fullnm files))


;;--------------------------------------------------------------------------------

(define-class <arc:link-generic> (<class>) ())

(define-method (initialise <arc:link-generic> args)
  (call-next-method)
  self)

(define-method (arc:link:link-cmd <arc:link-generic>)
  (call-next-method)
  "gcc")

(define-method (arc:link:outfile-flag <arc:link-generic>)
  (call-next-method)
  "-o")

(define-method (arc:link:shared-flag <arc:link-generic>)
  (call-next-method)
  "")

(define-method (arc:link:static-flag <arc:link-generic>)
  (call-next-method)
  "-static")

(define-method (arc:link:nostdlib-flag <arc:link-generic>)
  (call-next-method)
  "-nostdlib")

     
     ;; the default application extension (on windows: .exe)
(define-method (arc:link:app-ext <arc:link-generic>)
  (call-next-method)
  "")

(define-method (arc:link:rpath-option <arc:link-generic>)
  (call-next-method)
  "-Wl,-rpath,")

(define-method (arc:link:format-rpath-option <arc:link-generic> rpath)
  (call-next-method)
  (let* ((ropt (arc:link:rpath-option self)))
    (cond
     ((string? rpath) (string-append ropt
                                     (arc:path->string
                                      (arc:path-absolutize
                                       (arc:string->path rpath)))))
     ((list? rpath) (arc:annotate-list 
                     (arc:reduce (lambda (x lst)
                                   (cons (arc:path->string
                                          (arc:path-absolutize
                                           (arc:string->path x)))
                                         lst))
                                 '()
                                 rpath) ropt) )
     (else '() ))) )


     ;; make the name for an application
(define-method (arc:link:make-app-name <arc:link-generic> outdir appnm appext*)
  (call-next-method)
  (let* ((appext (or appext*
                     (arc:link:app-ext self)) )
         (od (if outdir 
                 (arc:string->path outdir) 
                 '()))
         (ap (if (and appext
                      (> (string-length appext) 0))
                 (arc:path-replace-last-ext (arc:string->path appnm) 
                                            appext)
                 (arc:path-without-last-ext (arc:string->path appnm)))) )
    (arc:path->string (arc:path-append od ap))))
     

(define-method (arc:link:format-framework-options <arc:link-generic> frameworks)
  (call-next-method)
  (arc:log 'warn "Framework settings are ignored on platforms other than Darwin/OS X")
  '())


(define-method (arc:link:needs-relink? <arc:link-generic> fullnm files)
  (arc:is-due? fullnm files 'list))

;; link a set of object files
(define-method (arc:link:link-app <arc:link-generic> outdir appnm appext 
                                  libdirs autolibdirs shared nostdlib files autolibs libs
                                  rpath frameworks)
  (call-next-method)
  (let* ((fullnm (arc:link:make-app-name self outdir appnm appext))
         (link-cmd (arc:link:link-cmd self))
         (link-args (arc:list-appends
                     (if libdirs
                         (arc:annotate-list libdirs "-L")
                         '())
                     (if autolibdirs
                         (arc:annotate-list autolibdirs "-L")
                         '())
                     (if shared
                         (arc:link:shared-flag self)
                         (arc:link:static-flag self))
                     (if (and shared
                              rpath)
                         (arc:link:format-rpath-option self rpath)
                         '())
                     (if nostdlib
                         (arc:link:nostdlib-flag self)
                         '())
                     (arc:link:outfile-flag self) fullnm
                     (if files
                         files
                         '())
                     (if autolibs
                         (arc:annotate-list autolibs "-l")
                         '())
                     (if libs
                         (arc:annotate-list libs "-l")
                         '())
                     (if frameworks
                         (arc:link:format-framework-options self frameworks)
                         '()) 
                     )))

    (if (arc:link:needs-relink? self fullnm files)
        (begin
          (arc:log 'debug "linking " fullnm " ...")
    
          (arc:display-command link-cmd link-args)
    
          (if (not (equal? (sys:execute* link-cmd link-args) 0))
              (arc:log 'info "linking '" fullnm "' failed")) ))
    
    fullnm))


;;--------------------------------------------------------------------------------

;; backend functionality for linking object files on darwin systems.
(define-class <arc:link-darwin> (<arc:link-generic>) ())

(define-method (initialise <arc:link-darwin> args)
  (call-next-method)
  self)

(define-method (arc:link:static-flag <arc:link-darwin>)
  (call-next-method)
  "")

(define-method (arc:link:format-framework-options <arc:link-darwin> frameworks)
  (call-next-method)
  (arc:reduce (lambda (x lst)
                (cons "-framework" (cons x lst)))
              '()
              frameworks))


;;--------------------------------------------------------------------------------

;; backend functionality for linking object files on cygwin.
(define-class <arc:link-cygwin> (<arc:link-generic>) ())

(define-method (initialise <arc:link-cygwin> args)
  (call-next-method)
  self)

;; the default application extension (on windows: .exe)
(define-method (arc:link:app-ext <arc:link-cygwin>)
  (call-next-method)
  ".exe")


;;--------------------------------------------------------------------------------

;; backend functionality for linking object files on sunos.
(define-class <arc:link-sunos> (<arc:link-generic>) ())

(define-method (initialise <arc:link-sunos> args)
  (call-next-method)
  self)

(define-method (arc:link:shared-flag <arc:link-sunos>)
  (call-next-method)
  "-Wl,-dy")

(define-method (arc:link:static-flag <arc:link-sunos>)
  (call-next-method)
  "-Wl,-dn")

(define-method (arc:link:rpath-option <arc:link-sunos>)
  (call-next-method)
  "-Wl,-R")


;;--------------------------------------------------------------------------------

(define (arc:link-backend system)
  (let* ( (sysnm (if (list? system)
                     (car system)
                     system)) )
    (case sysnm
      ((darwin)      (make-object <arc:link-darwin>  '()))
      ((cygwin)      (make-object <arc:link-cygwin>  '()))
      ((sunos)       (make-object <arc:link-sunos>   '()))
      ((bsd linux beos) 
                     (make-object <arc:link-generic> '()))
      (else          (make-object <arc:link-generic> '())) )) )

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
