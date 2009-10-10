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

(arc:provide 'sys-lib)


;;--------------------------------------------------------------------------------

(define-generic (arc:ld:ar-cmd))
(define-generic (arc:ld:replace-create-flag))
(define-generic (arc:ld:ranlib-cmd))
(define-generic (arc:ld:ranlib-needed?))
(define-generic (arc:ld:suffix-shared))
(define-generic (arc:ld:suffix-static))

(define-generic (arc:ld:ld-cmd))
(define-generic (arc:ld:ld-shared-flag))
(define-generic (arc:ld:ld-extra-flags))
(define-generic (arc:ld:ld-outfile-flag))
(define-generic (arc:ld:ld-soname-flag soname))
(define-generic (arc:ld:ld-rpath-option rpath))
     
;; make a name for a static library 
(define-generic (arc:ld:make-static-name outdir libnm))

;; returns a list with four different name forms of a shared
;; libraries.  car: the library's real name, incl. full version info;
;; cadr: the library's soname; caddr: the library's linker name;
;; cadddr: the internal soname (without path information)
(define-generic (arc:ld:make-shared-names outdir libnm vermajor verminor verrelease))
     
;; create static library
;; libnm: the (absolute) libraryname 
;; objs: the list of object files
(define-generic (arc:ld:make-static-lib libnm objs))
     
;; make a shared library
;; libnm: the (absolute) library's real name
;; soname: the soname (internal, without paths)
;; objs: the list of (shared) objects
;; libdirs: a list of library to look into for 
;;          dependency libs
;; deplibs: a list of libraries the shared library depends 
;;          on (or '() if not needed)
;; rpath: rpath
(define-generic (arc:ld:make-shared-lib libnm soname objs libdirs deplibs rpath))




;;--------------------------------------------------------------------------------

(define-class <arc:lib-generic> (<class>) ())

(define-method (initialise <arc:lib-generic> args)
  (call-next-method)
  self)
        
(define-method (arc:ld:ar-cmd <arc:lib-generic>)
  (call-next-method)
  "ar")

(define-method (arc:ld:replace-create-flag <arc:lib-generic>)
  (call-next-method)
  '("rc"))

(define-method (arc:ld:ranlib-cmd <arc:lib-generic>)
  (call-next-method)
  "ranlib")

(define-method (arc:ld:ranlib-needed? <arc:lib-generic>)
  (call-next-method)
  #t)

(define-method (arc:ld:suffix-shared <arc:lib-generic>)
  (call-next-method)
  "so")

(define-method (arc:ld:suffix-static <arc:lib-generic>)
  (call-next-method)
  "a")

(define-method (arc:ld:ld-cmd <arc:lib-generic>)
  (call-next-method)
  "gcc")

(define-method (arc:ld:ld-shared-flag <arc:lib-generic>)
  (call-next-method)
  '("-shared"))

(define-method (arc:ld:ld-extra-flags <arc:lib-generic>)
  (call-next-method)
  '())

(define-method (arc:ld:ld-outfile-flag <arc:lib-generic>)
  (call-next-method)
  "-o")

(define-method (arc:ld:ld-soname-flag <arc:lib-generic> soname)
  (call-next-method)
  '())

(define-method (arc:ld:ld-rpath-option <arc:lib-generic> rpath)
  (call-next-method)
  (string-append "-Wl,-rpath=" 
                 (arc:path->string
                  (arc:path-absolutize
                   (arc:string->path rpath)))))


(define-method (arc:ld:make-static-name <arc:lib-generic> outdir libnm)
  (call-next-method)
  (let* ((bn (string-append "lib" libnm "." (arc:ld:suffix-static self))))
    (if outdir
        (arc:path->string (arc:path-append (arc:string->path outdir) bn))
        bn)) )


;; returns a list with four different name forms of a shared
;; libraries.  car: the library's real name, incl. full version info;
;; cadr: the library's soname; caddr: the library's linker name;
;; cadddr: the internal soname (without path information)
(define-method (arc:ld:make-shared-names <arc:lib-generic> outdir libnm 
                                         vermajor verminor verrelease)
  (call-next-method)
  (let* ((name (string-append "lib" libnm "." (arc:ld:suffix-shared self)))
         (full (arc:path->string (arc:path-append 
                                  (arc:string->path outdir)
                                  name))))
    (list full full full name)))


;; create static library
(define-method (arc:ld:make-static-lib <arc:lib-generic> libnm objs)
  (call-next-method)

  (if (sys:file-exists? libnm)
      (sys:remove-file libnm))
  
  (let ((ar-cmd (arc:ld:ar-cmd self))
        (ar-args (arc:list-appends (arc:ld:replace-create-flag self)
                                   libnm
                                   objs))
        (ranlib-cmd (arc:ld:ranlib-cmd self))
        (ranlib-args (list libnm)) )
    
    (arc:display-command ar-cmd ar-args)
    
    (if (not (equal? (sys:execute ar-cmd ar-args) 0))
        (arc:msg "failed to create library " libnm 'nl)
        
        (if (arc:ld:ranlib-needed? self)
            (begin
              (arc:display-command ranlib-cmd ranlib-args)
              (if (not (equal? (sys:execute ranlib-cmd ranlib-args) 0))
                  (arc:msg "failed to run ranlib on " libnm 'nl)))))
    libnm))


;; build a shared library.  This requires the object files to be compiled
;; properly
(define-method (arc:ld:make-shared-lib <arc:lib-generic> libnm soname 
                                       objs libdirs deplibs
                                       rpath)
  (call-next-method)

  (if (sys:file-exists? libnm)
      (sys:remove-file libnm))
  
  (let ((ld-cmd (arc:ld:ld-cmd self))
        (ld-args (arc:list-appends
                  (arc:ld:ld-cmd self) 
                  (arc:ld:ld-shared-flag self)
                  (arc:ld:ld-extra-flags self)
                  (arc:ld:ld-soname-flag soname self)
                  (if rpath
                      (arc:ld:ld-rpath-option self rpath)
                      '())
                  (if (and deplibs
                           (not (null? deplibs)))
                      (arc:annotate-list libdirs "-L")
                      '())

                  ;; objects
                  objs

                  ;; deplibs
                  (if (and deplibs
                           (not (null? deplibs)))
                      (arc:annotate-list deplibs "-l")
                      '())
                  (arc:ld:ld-outfile-flag self)
                  libnm)) )

    (arc:display-command ld-cmd ld-args)
    
    (if (not (equal? (sys:execute ld-cmd ld-args) 0))
        (begin
          (arc:msg "failed to create library " libnm 'nl)
          #f)
        libnm) ))


;;--------------------------------------------------------------------------------

(define-class <arc:lib-cygwin> (<arc:lib-generic>) ())

(define-method (initialise <arc:lib-cygwin> args)
  (call-next-method)
  self)


(define-method (arc:ld:suffix-shared <arc:lib-cygwin>)
  (call-next-method)
  "dll")


(define-method (arc:ld:ld-cmd <arc:lib-cygwin>)
  (call-next-method)
  "dlltool")


(define-method (arc:ld:make-shared-lib <arc:lib-cygwin> libnm soname 
                                       objs libdirs deplibs
                                       rpath)
  (call-next-method)

  (if (sys:file-exists? libnm)
      (sys:remove-file libnm))

  (let* ((libnmp (arc:string->path libnm))
         (def (arc:path->string (arc:path-replace-last-ext libnmp "def")))
         (dllname (arc:path->string (arc:path-last-comp libnmp)))
         (import-libname (arc:path->string
                          (arc:path-replace-last-ext
                           libnmp (arc:ld:suffix-static self)) ))
;;;         (import-libname (arc:path->string
;;;                          (arc:path-append-ext
;;;                           (arc:path-append
;;;                            (arc:path-without-last-comp libnmp)
;;;                            (string-append
;;;                             (arc:path->string
;;;                              (arc:path-without-last-ext 
;;;                               (arc:path-last-comp libnmp)))
;;;                             "_loader"))
;;;                           (arc:ld:suffix-static self))))
         (dll1-cmd "dlltool")
         (dll1-args (arc:list-appends "--export-all" "--output-def" def objs))
         (dll2-cmd "dllwrap")
         (dll2-args (arc:list-appends "-o " libnm
                                      "--dllname" dllname
                                      "--def" def
                                      objs
                                      deplibs))
         (dll3-cmd "dlltool")
         (dll3-args (arc:list-appends "--def" def
                                      "--dllname" dllname
                                      "--output-lib" import-libname))
         )
    (arc:display-command dll1-cmd dll1-args)
    
    (if (not (equal? (sys:execute dll1-cmd dll1-args) 0))
        (arc:throw 'exec 
                   (string-append "failed to create library " libnm
                                  " (dlltool)"))
        (begin
          (arc:display-command dll2-cmd dll2-args)
          (if (not (equal? (sys:execute dll2-cmd dll2-args) 0))
              (arc:throw 'exec 
                         (string-append "failed to create library " libnm
                                        " (dllwrap)"))
              (begin
                (arc:display-command dll3-cmd dll3-args)
                (if (not (equal? (sys:execute dll3-cmd dll3-args) 0))
                    (arc:throw 'exec 
                               (string-append "failed to create library " libnm
                                              " (loader)"))
                    libnm)))))
    ))

     
;;--------------------------------------------------------------------------------

(define-class <arc:lib-base-elf> (<arc:lib-generic>) ())

(define-method (initialise <arc:lib-base-elf> args)
  (call-next-method)
  self)


(define-method (arc:ld:make-shared-names <arc:lib-base-elf> outdir libnm 
                                         vermajor verminor verrelease)
  (call-next-method)

  (let* ((fvp (if vermajor 
                  (string-append 
                   "." (arc:num/str vermajor)
                   (if verminor
                       (string-append 
                        "." (arc:num/str verminor)
                        (if verrelease
                            (string-append
                             "." (arc:num/str verrelease))
                            ""))
                       ""))
                  ""))
         (svp (if vermajor 
                  (string-append "." (arc:num/str vermajor))
                  ""))
         (linknm (string-append "lib" libnm "."
                                (arc:ld:suffix-shared self)))
         (realnm (string-append linknm fvp))
         (soname (string-append linknm svp))
         )
    (if outdir
        (let ((rpath (arc:string->path outdir)))
          (list (arc:path->string (arc:path-append rpath realnm))
                (arc:path->string (arc:path-append rpath soname))
                (arc:path->string (arc:path-append rpath linknm))
                soname))
        (list realnm soname linknm soname)) ))


;;--------------------------------------------------------------------------------

(define-class <arc:lib-linux> (<arc:lib-base-elf>) ())

(define-method (initialise <arc:lib-linux> args)
  (call-next-method)
  self)

(define-method (arc:ld:ld-extra-flags <arc:lib-linux>)
  (call-next-method)
  '("-Wl,--export-dynamic"))

(define-method (arc:ld:ld-soname-flag <arc:lib-linux> soname)
  (call-next-method)
  (string-append "-Wl,-soname," soname))

(define-method (arc:ld:ld-rpath-option <arc:lib-linux> rpath)
  (call-next-method)
  (string-append "-Wl,-rpath," 
                 (arc:path->string
                  (arc:path-absolutize
                   (arc:string->path rpath)))))


;;--------------------------------------------------------------------------------

(define-class <arc:lib-beos> (<arc:lib-generic>) ())

(define-method (initialise <arc:lib-beos> args)
  (call-next-method)
  self)


(define-method (arc:ld:ld-cmd <arc:lib-beos>)
  (call-next-method)
  "ld")


(define-method (arc:ld:ld-shared-flag <arc:lib-beos>)
  (call-next-method)
  "-shared")


(define-method (arc:ld:ld-extra-flags <arc:lib-beos>)
  (call-next-method)
  '("--export-dynamic"))

(define-method (arc:ld:ld-soname-flag <arc:lib-beos> soname)
  (call-next-method)
  (string-append "-soname=" soname))

(define-method (arc:ld:ld-rpath-option <arc:lib-beos> rpath)
  (call-next-method)
  (string-append "-rpath=" 
                 (arc:path->string
                  (arc:path-absolutize
                   (arc:string->path rpath)))))


;;--------------------------------------------------------------------------------

(define-class <arc:lib-sunos> (<arc:lib-base-elf>) ())

(define-method (initialise <arc:lib-sunos> args)
  (call-next-method)
  self)

(define-method (arc:ld:ld-cmd <arc:lib-sunos>)
  (call-next-method)
  "gcc")

(define-method (arc:ld:ld-soname-flag <arc:lib-sunos> soname)
  (call-next-method)
  (string-append "-Wl,-h," soname))

(define-method (arc:ld:ld-rpath-option <arc:lib-sunos> rpath)
  (call-next-method)
  (string-append "-Wl,-rpath," 
                 (arc:path->string
                  (arc:path-absolutize
                   (arc:string->path rpath)))))


;;--------------------------------------------------------------------------------

(define-class <arc:lib-darwin> (<arc:lib-generic>) ())

(define-method (initialise <arc:lib-darwin> args)
  (call-next-method)
  self)

(define-method (arc:ld:ranlib-needed? <arc:lib-darwin>)
  (call-next-method)
  #f)

(define-method (arc:ld:suffix-shared <arc:lib-darwin>)
  (call-next-method)
  "dylib")

(define-method (arc:ld:ld-cmd <arc:lib-darwin>)
  (call-next-method)
  "gcc")

(define-method (arc:ld:ld-shared-flag <arc:lib-darwin>)
  (call-next-method)
  "-dynamiclib")

(define-method (arc:ld:ld-soname-flag <arc:lib-darwin> soname)
  (call-next-method)
  '())

(define-method (arc:ld:ld-rpath-option <arc:lib-darwin> rpath)
  (call-next-method)
  '())


;;--------------------------------------------------------------------------------

(define (arc:lib-backend system)
  (let* ( (sysnm (if (list? system)
                     (car system)
                     system)) )
    (case sysnm
      ((darwin)      (make-object <arc:lib-darwin>  '()))
      ((cygwin)      (make-object <arc:lib-cygwin>  '()))
      ((sunos)       (make-object <arc:lib-sunos>   '()))
      ((bsd linux)   (make-object <arc:lib-linux>   '()))
      ((beos)        (make-object <arc:lib-beos>    '()))
      (else          (make-object <arc:lib-generic> '())) )) )

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
