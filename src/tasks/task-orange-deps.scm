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

(arc:provide 'task-orange-deps)

(arc:require 'task-orange-compile "tasks/task-orange-compile")


(arc:log 'debug "loading 'orange-deps' task")

(define arc:orange-deps-keywords '((sources strlist required)
                                   (outdir string optional)
                                   (flags strlist optional)
                                   (catalog string optional)
                                   (cext string optional)))

(define (arc:orange-deps props body)
  (let* ((cext "c")
         (outdir (arc:aval 'outdir props "."))
         (catalog (arc:aval 'catalog props #f))
         (flags (arc:string-list->string* (arc:aval 'flags 
						    props '())
					  " "))
         (sources (arc:aval 'sources props '())) )
    (map
     (lambda (src)
       (let ((target (arc:make-orange-cfile src outdir cext)))
         (arc:log 'debug "dependency for: " target)
         (arc:deps-get-deps src target
                            (lambda (sfile tfile) 
                              (arc:make-orange-deps sfile tfile
                                                    flags
                                                    catalog)))))
     sources)))


(define (arc:make-orange-deps sfile cfile flags catalog)
  (let* ((bn (arc:path-last-comp (arc:string->path sfile)))
         (tdf (arc:path->string
               (arc:path-append (arc:arc-tmp-directory)
                                (arc:path-replace-last-ext bn "dep"))))
         (deps-cmd (arc:orange-o2cdep))
         (deps-args (arc:list-appends "-A"
                                      (if catalog
                                          (list "-C" catalog)
                                          '())
                                      "-d" (arc:path->string (arc:arc-tmp-directory))
                                      flags
                                      "-O" cfile
                                      sfile)))

    (arc:display-command deps-cmd deps-args)

    (if (equal? (sys:execute deps-cmd deps-args) 0)
        (let ((deps (arc:load-deps-file tdf)))
          (arc:deps-set-target! deps cfile)
          (sys:remove-file tdf)
          deps)
        #f)))


(define (arc:orange-o2cdep)
  (let ((cmd (arc:env-get 'o2cdep-command)))
    (if cmd
        cmd
        (let ((orange-prefix (arc:env-get 'orange-prefix)))
          (if orange-prefix
              (arc:path->string
               (arc:path-append
                (arc:path-append (arc:string->path orange-prefix)
                                 "bin")
                "o2cdep"))
              ;; if this fails, assume o2cdep is in the path -- it should
              ;; be anyway ...
              "o2cdep")))))

(arc:register-task 'orange-deps arc:orange-deps arc:orange-deps-keywords)



;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
