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

;; $Id: task-mkdir.scm,v 1.2 2003/04/19 01:08:38 eyestep Exp $

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
        (arc:sys 'mkdirs dirnm)))
  '<unspecified>)

(arc:register-task 'mkdir arc:mkdir arc:mkdir-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
