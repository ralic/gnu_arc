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

;; $Id: task-bison.scm,v 1.2 2003/04/22 23:40:43 eyestep Exp $

(arc:provide 'task-bison)

(arc:log 'debug "loading 'bison' task")

;; compiles a bison grammar file file into a c source file.
;;
;; keywords:
;;
;; source: STRING
;; the bison source file
;;
;; prefix: STRING
;; provides a prefix which should be used instead of the default 'yy'.
;; This is necessary if you need more than one parser in the same
;; application.  The output file is renamed from parse.tab.c to source.c,
;; if no tofile: value is given
;;
;; create-header?: BOOLEAN
;; if create-header? is #t, this task creates two files actually, the
;; c-file and an c header file.  Default is #t, since it is nearly always
;; necessary to refer to the bison created token types/constants from
;; outside.
;;
;; to-c-file: STRING
;; gives the name of the c-file to create
;;
;; to-h-file: STRING
;; gives the name of the h-file to create; only used if create-header? is
;; #t.  Defaults to tofile-c, where the .c suffix is replaced by 
;; a .h-suffix.  This is only to use, when to-c-file is used too.
;;
;; outdir: STRING
;; generates the output file(s) in this dir, instead of the current one.  
;; this has no effect when to-c-file:/to-h-file: is used.  By default the
;; generated files are named from the input file (source:), where the 
;; .y-suffix is replaced with .c and .h suffices.
;;
;; no-lines?: BOOLEAN
;; creates #line statements in the generated C code
;;
;; the return value is the generate c file

(define arc:bison-keywords '((source string required)
                             (prefix string optional)
                             (create-header? boolean optional)
                             (no-lines? boolean optional)
                             (to-c-file string (opt-xor outdir))
                             (to-h-file string optional)
                             (outdir string (opt-xor tofile))) )
  
(define (arc:bison props body)
  (let* ((outdir (arc:aval 'outdir props #f))
         (to-c-file (arc:aval 'to-c-file props #f))
         (to-h-file (arc:aval 'to-h-file props #f))
         (ch? (arc:aval 'create-header? props #t))
         (no-lines? (arc:aval 'no-lines? props #f))
         (prefix (arc:aval 'prefix props #f))
         (source (arc:aval 'source props #f))
         (av (arc:attrval)) )

    (arc:log 'debug "bison ...")

    (let* ((coutfile (if to-c-file 
                         #f
                         (let* ((x (arc:path-replace-last-ext 
                                    (arc:string->path source) "c"))
                                (y (if outdir
                                       (arc:path->string
                                        (arc:path-append
                                         (arc:string->path outdir)
                                         (arc:path-last-comp x)))
                                       (arc:path->string x))))
                           y)))
           (houtfile (if to-h-file 
                         #f
                         (let* ((x (arc:path-replace-last-ext 
                                    (arc:string->path source) "h"))
                                (y (if outdir
                                       (arc:path->string
                                        (arc:path-append
                                         (arc:string->path outdir)
                                         (arc:path-last-comp x)))
                                       (arc:path->string x))))
                           y)))
           (bisoncmd (string-append "bison "
                                    (if no-lines?
                                        "-l "
                                        "")
                                    (if ch?
                                        (string-append "--defines="
                                                       houtfile " ")
                                        "")
                                    (if to-c-file
                                        (string-append "-o "
                                                       to-c-file
                                                       " ")
                                        (if coutfile
                                            (string-append "-o "
                                                           coutfile
                                                           " ")
                                            ""))
                                    (if prefix
                                        (string-append "-p" prefix " ")
                                        "")
                                    source
                                    )) )
      
      
      (if (arc:deps-bison-needs-recompile? source 
                                           (or to-c-file coutfile))
          (begin
            (arc:display bisoncmd #\nl)
            (if (not (= (system bisoncmd) 0))
                (arc:log 'error "bison: failed to translate file '" 
                         source "'")
                (begin
                  (arc:attrval-set! av 'c-source (or to-c-file coutfile))
                  (arc:attrval-set! av 'c-header (or to-h-file houtfile)) ))))
      )
    av))

(define (arc:deps-bison-needs-recompile? sfile ofile)
  (let ((deps (arc:deps-get-deps sfile ofile 
                                 (lambda (src dest)
                                   (arc:deps-set-deps! (arc:make-deps dest)
                                                       src)))))
    (if (not (list? deps))
        ;; for some reason we didn't got a dependecy list. assume recompile
        #t
        ;; otherwise check if the object file needs recompilation.  this is
        ;; done generic.  probably once replace the modification time
        ;; method by a md5sum based method?
        (arc:mtime-file-changed? deps ofile))))


(arc:register-task 'bison arc:bison arc:bison-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
