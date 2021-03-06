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

(arc:provide 'task-fileset)


(arc:log 'debug "loading 'fileset' task")


;; the fileset task compiles a list of files, either by applying file
;; patterns to a directory, or by combining explicitly set files
;;
;; TODO: the only format excepted by the :pattern is "*.xxx" and "*", non
;; recursive in the current working directly only.
;;
;; RETURNS
;; a list of filenames (strings)


;; determines a list of files.  The fileset actions has to main modes: One
;; lists a explicit list of files, the other finds matching files from a
;; directory.  The result of this actions is always a fileset
;;
;; keywords:
;; :dir STRING
;; lists the files relative to a directory
;;
;; :pattern STRING
;; specifies a pattern files should match. required exclusive with
;; :files
;;
;; match a path against a pattern; both values are "paths" as returned by
;; arc:string->path valid patterns:
;;
;; *.h                  all .h files in current dir X
;; **/*.h               all .h files recursive from current dir
;; src/*.h              all .h files in subdir src/
;; src/**/*.h           all .h files recursive from subdir src/
;;
;; :exclude STRING-LIST | STRING
;; specifies a list of path which are excluded from the result list.
;;
;; :files STRING-LIST
;; a list of files included in this fileset.  required exclusive with 
;; :pattern
;;
;; the keywords :includes and :pattern are required exclusive

(define arc:fileset-keywords '((dir string optional)
                               (pattern '(string
                                          strlist) (req-xor files))
                               (files '(strlist
                                        string) (req-xor pattern))
                               (exclude '(string strlist) optional)) )

(define (arc:fileset props body)
  (let* ((dir* (arc:aval 'dir props #f))
         (dir (if dir* (arc:string->path dir*) #f))
         (pattern (arc:aval 'pattern props #f))
         (files (arc:aval 'files props #f))
         (exclude (let ((v (arc:aval 'exclude props #f)))
                    (cond
                     ((list? v) v)
                     ((string? v) (list v))
                     (else '()))) ) )
    
    (cond 
     ((string? pattern)
      (arc:substract-files-list (arc:scan-dir-with-pattern 
                                 (if dir
                                     (if (arc:path-absolute? dir)
                                         dir
                                         (arc:path-append (arc:path-cwd) dir))
                                     (arc:path-cwd))
                                 pattern)
                                exclude))
     ((list? pattern)
      (let loop ((retv '())
                 (p pattern))
        (if (null? p)
            (arc:substract-files-list retv exclude)
            (loop (append retv (arc:scan-dir-with-pattern 
                                (if dir
                                    (if (arc:path-absolute? dir)
                                        dir
                                        (arc:path-append (arc:path-cwd)
                                                         dir))
                                    (arc:path-cwd))
                                (car p)))
                  (cdr p)))))
     
     ((list? files) 
      (arc:substract-files-list files exclude))

     ((string? files) 
      (arc:substract-files-list (list files) exclude))

     (else '()))))


(arc:register-task 'fileset arc:fileset arc:fileset-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
