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
    
    (if (sys:file-exists? fn)
        (let ((ct (sys:current-time)))
          (sys:utime fn ct ct))
        (let ((port (open-output-file fn)))
          (close-output-port port)) )
    
    '<unspecified>))

(arc:register-task 'touch arc:touch arc:touch-keywords)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
