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

;; $Id: task-flex.scm,v 1.1 2003/04/17 00:02:45 eyestep Exp $

(arc:provide 'task-flex)
(arc:require 'task-c-deps)

(arc:log 'debug "loading 'flex' task")

;; compiles a flex description file into a c source file.
;;
;; the majority of settings, e.g. which c compiler to use, which default
;; flags to specify, etc. should be set by a system configuration and read
;; in by the master arc-script (if available)
;;
;; keywords:
;;
;; source: STRING
;; the flex source file
;;
;; prefix: STRING
;; provides a prefix which should be used instead of the default 'yy'.
;; This is necessary if you need more than one lexer in the same
;; application.  The output file is renamed from lex.yy.c to source.c,
;; if no tofile: value is given
;;
;; tofile: STRING
;; copies the generated c code on file STRING
;;
;; outdir: STRING
;; generates the output file in this dir, instead of the current one.  
;; this has no effect when tofile: is used.
;;
;; case-insensitive?: BOOLEAN
;; creates a case insensitive scanner
;;
;; the return value is the generate c file
(define arc:flex-keywords '((source string required)
                            (prefix string optional)
                            (case-insensitive? boolean optional)
                            (tofile string (opt-xor outdir))
                            (outdir string (opt-xor tofile))) )
  
(define (arc:flex props body)
  (let* ((outdir (arc:aval 'outdir props #f))
         (tofile (arc:aval 'tofile props #f))
         (ci? (arc:aval 'case-insensitive? props #f))
         (prefix (arc:aval 'prefix props #f))
         (source (arc:aval 'source props #f))
         (av (arc:attrval)) )

    (arc:log 'debug "flex ...")

    (let* ((outfile (if tofile 
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
           (flexcmd (string-append "flex "
                                   (if ci?
                                       "-I "
                                       "")
                                   (if prefix
                                       (string-append "-P" prefix " ")
                                       "")
                                   (if (or tofile outfile)
                                       "-t "
                                       "")
                                   source
                                   (if tofile
                                       (string-append " > " tofile)
                                       (if outfile
                                           (string-append " > " outfile)
                                           ""))
                                   )))
      
      
      (if (arc:deps-flex-needs-recompile? source 
                                          (or tofile outfile))
          (begin
            (arc:display flexcmd #\nl)
            (if (not (= (system flexcmd) 0))
                (arc:log 'error "flex: failed to translate file '" 
                         source "'")
                (arc:attrval-set! av 'c-source (or tofile outfile)))))
      )
    av))

(define (arc:deps-flex-needs-recompile? sfile ofile)
  (let ((deps (arc:deps-get-deps sfile 
                                 (lambda (src)
                                   (list ofile 
                                         (list (cons src 0)))))))
    (if (not (list? deps))
        ;; for some reason we didn't got a dependecy list. assume recompile
        #t
        ;; otherwise check if the object file needs recompilation.  this is
        ;; done generic.  probably once replace the modification time
        ;; method by a md5sum based method?
        (arc:mtime-file-changed? deps ofile))))


(arc:register-task 'flex arc:flex arc:flex-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End: