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

;; $Id: task-delete.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $


(arc:provide 'task-delete)


(arc:log 'debug "loading 'delete' task")

;; deletes a file or recursively a directory 
;;
;; Keywords:
;; :pathnm STRING
;; the path of the directory or file to remove.
;;
;; :recursive? BOOL
;; if #t, a directory is removed recursively, if #f a directory must be 
;; empty before removed.  the default if #f (not recursive)
;;
;; :ignore-missing? BOOL
;; if #t, arc does not complain about a missing directory/file to delete,
;; otherwise it does.  Default is #f (to complain)
;;
;; RETURNS
;; <unspecified>

(define arc:delete-keywords '((path string required)
                              (recursive? boolean optional)
                              (ignore-missing? boolean optional)) )
(define (arc:delete props body)
  (let* ((pathnm (let ((v (arc:aval 'path props #f)))
                   (if v v
                       (arc:log 'fatal "empty path")))))
    (arc:log 'debug "delete ... " pathnm)
    
    (if (arc:sys.file-exists? pathnm)
        (if (arc:aval 'recursive? props #f)
            (if (arc:sys.remove-dir pathnm)
                (arc:log 'verbose "directory '" pathnm "' deleted"))
            (if (arc:sys.remove-file pathnm)
                (arc:log 'verbose "file '" pathnm "' deleted")))
        (if (arc:aval 'ignore-missing? props #f)
            #f
            (arc:log 'info "directory/file '"
                     pathnm
                     "' to remove does not exist"))) )
  '<unspecified>)

(arc:register-task 'delete arc:delete arc:delete-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
