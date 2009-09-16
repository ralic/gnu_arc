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

(arc:provide 'task-orange-compile)
;;(arc:require 'task-orange-deps)

(arc:log 'debug "loading 'orange-compile' task")

;; compiles a set of orange files into object files.
;;
;; keywords:
;;
;;
;; the return value is a list of all object files controlled by this task
(define arc:orange-compile-keywords '((sources strlist required)
                                      (debug? boolean optional)
                                      (flags strlist optional)
                                      (includes strlist optional)
                                      (catalog string optional)
                                      (outdir string optional)
                                      (objext string optional)
                                      (sobjext string optional)
                                      (shared? boolean optional)
                                      (static? boolean optional)
                                      (depends dependencies optional)))

(define (arc:orange-compile props body)
  (let* ((outdir (arc:aval 'outdir props #f))
         (catalog (arc:aval 'catalog props #f))
         (oflags (string-append 
                  (arc:string-list->string* (arc:aval 'flags props '()) " ")
                  " "
                  (if catalog
                      (string-append "-C" catalog " ")
                      "")))
         (sources (arc:aval 'sources props '()))
         (depends (arc:aval 'depends props #f)) 
         (av (arc:attrval)) )
    
    (arc:log 'debug "compile ...")
    
    (for-each
     (lambda (fn)
       (let* ((cn (arc:make-orange-cfile fn outdir "c"))
              (cincls (arc:string-list->string* 
                       (arc:aval 'includes props '()) " -I")))
         
         (let ((vv (arc:attrval-ref av 'c-source) ))
           (if vv
               (arc:attrval-set! av 'c-source (append vv (list cn)))
               (arc:attrval-set! av 'c-source (list cn))))
         (if (arc:deps-orange-needs-recompile? depends 
                                               fn
                                               cn
                                               catalog
                                               "c"
                                               outdir)
             (begin
               (arc:log 'verbose "compile '" fn "' into '" cn "'")
               (arc:orange-compile-file
                fn                      ; source
                cn                      ; cfile
                cincls                  ; cincls
                oflags                  ; orange flags
                outdir                  ; out directory
                ))))
       )
     sources)
    av))

(define (arc:deps-orange-needs-recompile? depends sfile cfile
                                          catalog objext outdir)
  (let ((deps (if depends
                  (let ((va (assoc cfile depends)))
                    (if va (list va) #f))
                  (arc:call-task 'orange-deps
                                 (list 'sources (list sfile)
                                       'outdir (if outdir 
                                                   outdir
                                                   '())
                                       'catalog catalog )
                                 #f) )))
    (if (not (list? deps))
        ;; for some reason we didn't got a dependecy list. assume recompile
        #t
        ;; otherwise check if the object file needs recompilation.  this is
        ;; done generic.  probably once replace the modification time
        ;; method by a md5sum based method?
        (arc:mtime-file-changed? (car deps) cfile))))


(define (arc:orange-compile-file sfile cfile incls
                                 orange-flags outdir)
  (let* ((orange-cmd (string-append
                      (arc:orange-command) " -x "
                      orange-flags " "
                      (if outdir
                          (string-append "-d " outdir " ")
                          "")
                      incls " "
                      sfile)))
    (arc:display orange-cmd #\nl)
    (if (not (equal? (arc:sys 'system orange-cmd) 0))
        (if (not %arc:keep-going-on-errors%)
            (quit)))
    ))


(define (arc:make-orange-cfile filename outdir cext)
  (let* ((pn (arc:string->path filename))
         (on* (arc:path-replace-last-ext 
               pn cext )))
    (arc:path->string
     (if (and outdir (not (null? outdir)))
         (arc:path-append (arc:string->path outdir)
                          (arc:path-last-comp on*))
         on*))))


(define (arc:orange-command)
  (let ((cmd (arc:env-get 'orange2c-command)))
    (if cmd
        cmd
        (let ((orange-prefix (arc:env-get 'orange-prefix)))
          (if orange-prefix
              (arc:path->string
               (arc:path-append
                (arc:path-append (arc:string->path orange-prefix)
                                 "bin")
                "orange2c"))
              ;; if this fails, assume orange2c is in the path -- it should
              ;; be anyway ...
              "orange2c")))))

  
         
(arc:register-task 'orange-compile arc:orange-compile 
                   arc:orange-compile-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
