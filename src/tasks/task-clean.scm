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

(arc:provide 'task-clean)


(arc:log 'debug "loading 'clean' task")

;; cleans a project area.  "Cleaning" means, removing all implicitly or
;; explicitly build files and directories.  
;;
;; Keywords:
;; :dir STRING
;; start clean operation from this base directory; by default the clean
;; operation starts always from the directory stated as basedir in the
;; current working context
;;
;; :res STRING-LIST
;; a list of all "resources" (files and directories) to be removed.  
;; directories are always deleted recursively 
;;
;; RETURNS
;; <unspecified>

(define arc:clean-keywords '((dir string optional)
                             (res strlist required)) )

(define (arc:clean props body)
  (let* ((res (arc:aval 'res props '())) 
         (dir (arc:aval 'dir props 
                        (arc:context-basedir (arc:context-current)))) )
    
    (arc:log 'debug "clean ... (root: " dir ")")
    
    (arc:clean-implicit dir)
    
    (let loop ((r res))
      (if (null? r)
          'done
          (begin
            (cond
             ((sys:file-directory? (car r)) (sys:remove-dir (car r)))
             ((sys:file-exists? (car r)) (sys:remove-file (car r)))
             (else 'ignore))
            (loop (cdr r)))))
    ;; do something more?
    )
  '<unspecified>)

(arc:register-task 'clean arc:clean arc:clean-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
