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

(arc:provide 'task-flex)
(arc:require 'task-c-deps)

(arc:log 'debug "loading 'flex' task")

;; compiles a flex description file into a c source file.
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
           (flex-cmd "flex")
           (flex-args (arc:list-appends (if ci?
                                            "-I"
                                            '())
                                        (if prefix
                                            (string-append "-P" prefix)
                                            '())
                                        (if (or tofile outfile)
                                            "-t"
                                            '())
                                        source
                                        (if tofile
                                            (string-append ">" tofile)
                                            (if outfile
                                                (string-append ">" outfile)
                                                '()))
                                        )))
      
      (if (arc:deps-flex-needs-recompile? source 
                                          (or tofile outfile))
          (begin
            (arc:display-command flex-cmd flex-args)
            (if (not (= (sys:execute* flex-cmd flex-args) 0))
                (arc:log 'error "flex: failed to translate file '" 
                         source "'")
                (arc:attrval-set! av 'c-source (or tofile outfile)))))
      )
    av))

(define (arc:deps-flex-needs-recompile? sfile ofile)
  (arc:is-due? ofile sfile 'single))


(arc:register-task 'flex arc:flex arc:flex-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
