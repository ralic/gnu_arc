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

(arc:provide 'task-mkdir)


(arc:log 'debug "loading 'mkdir' task")


;; creates a directory or a list of directories
;;
;; Keywords:
;; :dir STRING
;; the path of the directory to create.  all directories in the path are 
;; created if missing.
;;
;; RETURNS
;; <unspecified>

(define arc:mkdir-keywords '((dir string required)) )
(define (arc:mkdir props body)
  (let* ((dirnm (or (arc:aval 'dir props #f)
                    (begin
                      (arc:log 'fatal "empty file/directory (arc:mkdir)")
                      #f))))

    (arc:log 'debug "mkdir ... " dirnm)

    (if dirnm
        (sys:mkdirs dirnm))
    dirnm))

(arc:register-task 'mkdir arc:mkdir arc:mkdir-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
