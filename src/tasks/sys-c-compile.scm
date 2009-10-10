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

(arc:provide 'sys-c-compile)

;;--------------------------------------------------------------------------------

(define-generic (arc:cc:compiler-cmd))
(define-generic (arc:cc:makedeps-cmd))
(define-generic (arc:cc:default-defs))
(define-generic (arc:cc:default-incls))
(define-generic (arc:cc:default-flags))
(define-generic (arc:cc:deps-flag))
(define-generic (arc:cc:incl-flag))
(define-generic (arc:cc:opt-level-flag level))
(define-generic (arc:cc:ansi-flag))
(define-generic (arc:cc:debug-flag))
(define-generic (arc:cc:signed-char-flag))
(define-generic (arc:cc:unsigned-char-flag))
(define-generic (arc:cc:need-shared-build))
(define-generic (arc:cc:shared-obj-flag))
(define-generic (arc:cc:warn-level-flag level))
(define-generic (arc:cc:outfile-flag))
(define-generic (arc:cc:compile-only-flag))
(define-generic (arc:cc:objfile-ext))
(define-generic (arc:cc:shared-objfile-ext))
(define-generic (arc:cc:make-objfile-name filename outdir objext))
(define-generic (arc:cc:compile-file sfile ofile cincs cflags))
(define-generic (arc:cc:makedeps sfile ofile cflags cincs))


;;--------------------------------------------------------------------------------

;; backend functionality for compilation and c file dependency control.
(define-class <arc:c-compile-generic> (<class>) ())

(define-method (initialise <arc:c-compile-generic> args) 
  (call-next-method)
  self)

(define-method (arc:cc:compiler-cmd <arc:c-compile-generic>)
  (call-next-method)
  "gcc")

(define-method (arc:cc:makedeps-cmd <arc:c-compile-generic>)
  (call-next-method)
  "gcc")

(define-method (arc:cc:default-defs <arc:c-compile-generic>)
  (call-next-method)
  '())

(define-method (arc:cc:default-incls <arc:c-compile-generic>)
  (call-next-method)
  '())

(define-method (arc:cc:default-flags <arc:c-compile-generic>)
  (call-next-method)
  '())

;; flag for compiling dependencies
(define-method (arc:cc:deps-flag <arc:c-compile-generic>)
  (call-next-method)
  "-M")

(define-method (arc:cc:incl-flag <arc:c-compile-generic>)
  (call-next-method)
  "-I")

;; returns the compiler flag for optimization
(define-method (arc:cc:opt-level-flag <arc:c-compile-generic> level) 
  (call-next-method)
  (case level
    ((high)   '("-O3"))
    ((medium) '("-O2"))
    ((low)    '())
    (else     '()) ))

(define-method (arc:cc:ansi-flag <arc:c-compile-generic>)
  (call-next-method)
  '("-ansi"))

(define-method (arc:cc:debug-flag <arc:c-compile-generic>)
  (call-next-method)
  "-g")

(define-method (arc:cc:signed-char-flag <arc:c-compile-generic>)
  (call-next-method)
  '("-signed-char"))

(define-method (arc:cc:unsigned-char-flag <arc:c-compile-generic>)
  (call-next-method)
  '("-unsigned-char"))

;; does this platform needs a separate compilation for shared objects?
(define-method (arc:cc:need-shared-build <arc:c-compile-generic>)
  (call-next-method)
  #t)

;; if so, use the following extra flags for compilation; most definitly,
;; this must be overriden for various platforms
(define-method (arc:cc:shared-obj-flag <arc:c-compile-generic>)
  (call-next-method)
  '("-fpic" "-DPIC"))

(define-method (arc:cc:warn-level-flag <arc:c-compile-generic> level)
  (call-next-method)
  (case level
    ((high)   '("-Wall"))
    ((medium) (list "-Wqual"
                    "-Wmissing-prototypes"
                    "-Wimplicit"
                    "-Winline"
                    "-Wredundant-decls"
                    "-Wformat"
                    "-Wenum-clash"
                    "-Wuninitialized"))
    ((low)    (list "-Wcast-qual" "-Wmissing-prototypes"))
    (else     '()) ))

(define-method (arc:cc:outfile-flag <arc:c-compile-generic>)
  (call-next-method)
  "-o")

(define-method (arc:cc:compile-only-flag <arc:c-compile-generic>)
  (call-next-method)
  "-c")

(define-method (arc:cc:objfile-ext <arc:c-compile-generic>)
  (call-next-method)
  "o")

(define-method (arc:cc:shared-objfile-ext <arc:c-compile-generic>)
  (call-next-method)
  "lo")

(define-method (arc:cc:make-objfile-name <arc:c-compile-generic> filename outdir objext)
  (call-next-method)
  (let* ((pn (arc:string->path filename))
         (on* (arc:path-replace-last-ext 
               pn 
               (or objext 
                   (arc:cc:objfile-ext self)))) )
    (arc:path->string
     (if (and outdir (not (null? outdir)))
         (arc:path-append (arc:string->path outdir)
                          (arc:path-last-comp on*))
         on*))))

(define-method (arc:cc:compile-file <arc:c-compile-generic> sfile ofile cincs cflags)
  (call-next-method)
  (arc:log 'debug "compile file " sfile " to " ofile)
  (let ((cmd-cmd (arc:cc:compiler-cmd self))
        (cmd-args (arc:list-appends 
                   (arc:cc:default-defs self)
                   (arc:cc:default-incls self)
                   (arc:annotate-list cincs (arc:cc:incl-flag self)) ; custom includes
                   (arc:cc:default-flags self)
                   cflags                         ; custom cflags
                   (arc:cc:compile-only-flag self) ; compile only
                   (arc:cc:outfile-flag self)  
                   ofile                          ; obj file
                   sfile                          ; the source file
                   )))
    (arc:display-command cmd-cmd cmd-args)
    (if (not (equal? (sys:execute* cmd-cmd cmd-args) 0))
        (if (not %arc:keep-going-on-errors%)
            (quit -1)))))

(define-method (arc:cc:makedeps <arc:c-compile-generic> sfile ofile cflags cincs)
  (call-next-method)
  (let* ((bn (arc:path-last-comp (arc:string->path sfile)))
         (tdf (arc:path->string
               (arc:path-append (arc:deps-directory)
                                (arc:path-replace-last-ext bn "d"))))
         (md-cmd (arc:cc:makedeps-cmd self))
         (md-args (arc:list-appends
                   (arc:cc:deps-flag self)
                   cflags
                   cincs
                   sfile
                   ">" tdf)) )
    (arc:display-command md-cmd md-args)
    
    (if (equal? (sys:execute* md-cmd md-args) 0)
        (let* ((deps (arc:parse-make-deps-file tdf)))
          ;; set the correct object target
          (arc:deps-set-target! deps ofile)
          (sys:remove-file tdf)
          deps)
        #f)))


;;--------------------------------------------------------------------------------

;; backend functionality for compilation and c file dependency control.
(define-class <arc:c-compile-gcc> (<arc:c-compile-generic>) ())

(define-method (initialise <arc:c-compile-gcc> args) 
  (call-next-method)
  self)

;; flag for compiling dependencies; only produce dependencies for non
;; system files
(define-method (arc:cc:deps-flag <arc:c-compile-gcc>)
  (call-next-method)
  "-MM")

;;--------------------------------------------------------------------------------

;; backend functionality for compilation and c file dependency control on the
;; beos system.  
(define-class <arc:c-compile-beos> (<arc:c-compile-gcc>) ())

(define-method (initialise <arc:c-compile-beos> args) 
  (call-next-method)
  self)

(define-method (arc:cc:shared-obj-flag <arc:c-compile-beos>)
  (call-next-method)
  "-fpic")

;;--------------------------------------------------------------------------------

;; backend functionality for compilation and c file dependency control on
;; darwin systems (incl. Mac OSX).  Since on Darwin/MacOSX the default
;; compiler is the gcc compiler, this class assumes gcc compilation.

(define-class <arc:c-compile-darwin> (<arc:c-compile-gcc>) ())

(define-method (initialise <arc:c-compile-darwin> args) 
  (call-next-method)
  self)

(define-method (arc:cc:shared-obj-flag <arc:c-compile-darwin>)
  (call-next-method)
  '("-fPIC" "-dynamic"))

;;--------------------------------------------------------------------------------

;; backend for cygwin
(define-class <arc:c-compile-cygwin> (<arc:c-compile-gcc>) ())

(define-method (initialise <arc:c-compile-cygwin> args) 
  (call-next-method)
  self)

;; cygwin does not separate between shared or not shared builds
(define-method (arc:cc:need-shared-build <arc:c-compile-cygwin>)
  (call-next-method)
  #f)

;;--------------------------------------------------------------------------------

;; backend functionality for compilation and c file dependency control on
;; linux systems.  Since linux system comes by default with the gcc
;; compiler, this class assumes gcc compilation.  C-file dependencies are
;; generated by gcc too, so there's no need for a special class here.
(define-class <arc:c-compile-linux> (<arc:c-compile-gcc>) ())

(define-method (initialise <arc:c-compile-linux> args) 
  (call-next-method)
  self)

(define-method (arc:cc:shared-obj-flag <arc:c-compile-linux>)
  (call-next-method)
  '("-fpic" "-DPIC" "-dynamic"))

;;--------------------------------------------------------------------------------

(define (arc:c-compile-backend system)
  (let* ( (sysnm (if (list? system)
                     (car system)
                     system)) )
    (case sysnm
      ((bsd)         (make-object <arc:c-compile-gcc> '()))
      ((linux sunos) (make-object <arc:c-compile-linux> '()))
      ((darwin)      (make-object <arc:c-compile-darwin> '()))
      ((beos)        (make-object <arc:c-compile-beos> '()))
      ((cygwin)      (make-object <arc:c-compile-cygwin> '()))
      (else          (make-object <arc:c-compile-gcc> '())) )) )
    

(define (arc:c-deps-backend system)
  (arc:c-compile-backend system))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
