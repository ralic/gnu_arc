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

;; $Id: task-touch.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $

(arc:provide 'task-touch)


(arc:log 'debug "loading 'touch' task")


;; Update the access and modification times of each FILE to the current
;; time.  If the file does not exists yet, it is created
;;
;; Keywords:
;; :file STRING
;; the path of the file to touch.  only the real file is touched, no directories in the 
;; path - if missing - are created.
;;
;; RETURNS
;; <unspecified>

(define arc:touch-keywords '((file string required)) )
(define (arc:touch props body)
  (let* ((fn (arc:aval 'file props #f)) )
    (if (not fn)
        (arc:log 'fatal "touch: empty file"))
    
    (arc:log 'debug "touch ... " fn)
    
    (if (arc:sys.file-exists? fn)
        (let ((ct (arc:sys.current-time)))
          (arc:sys.utime fn ct ct))
        (let ((port (open-output-file fn)))
          (close-output-port port)) )
    
    '<unspecified>))

(arc:register-task 'touch arc:touch arc:touch-keywords)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
