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

;; $Id: task-file-type.scm,v 1.2 2003/04/19 01:08:38 eyestep Exp $

(arc:provide 'task-file-type)


(arc:log 'debug "loading 'file-type' task")


;; checks the type of a file-system resource and returns its type as a
;; symbol: 'dir, 'file, 'executable, 'unknown (= file not found)
;;
;; Keywords:
;; :src STRING
;; the path of the file or directory to check
;;
;; RETURNS
;; a symbol, indicating the type of the file: 'dir, 'file, 'executable

(define arc:file-type-keywords '((src string required)) )
(define (arc:file-type props body)
  (let* ((fn (arc:aval 'src props #f)) 
         (retv 'unknown))
    (if (not fn)
        (arc:log 'fatal "empty file (arc:touch)"))
    
    (arc:log 'debug "file-type ... " fn)
    
    (if (arc:sys 'file-directory? fn)
        (set! retv 'dir)
        (if (arc:sys 'file-exists? fn)
            (if (arc:sys 'file-executable? fn)
                (set! retv 'executable)
                (set! retv 'file) )
            (set! retv 'unknown)))
    retv))

(arc:register-task 'file-type arc:file-type arc:file-type-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
