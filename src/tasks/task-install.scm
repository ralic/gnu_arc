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

;; $Id: task-install.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $


(arc:provide 'task-install)


(arc:log 'debug "loading 'install' task")

;; installs a file or a set of files into it's runtime destination
;;
;; Keywords:
;; :src STRING
;; the path to the file to copy.
;;
;; :dest STRING
;; the destination path
;;
;; :strip? BOOL
;; strip symbol tables
;;
;; :group INTEGER
;; use the group information
;;
;; :owner INTEGER
;; use the owner information
;;
;; :mode (INTEGER | exec | file)
;; the mode to use.  Either 'executable' or 'file'.  The default is 'file
;;
;; RETURNS
;; <unspecified>

(define arc:install-keywords '((src    (string strlist)  required)
                               (dest   string            required)
                               (strip? boolean           optional)
                               (group  integer           optional)
                               (owner  integer           optional)
                               (mode   (integer symbol)  optional)) )
(define (arc:install props body)
  (let* ((src (arc:aval 'src props #f))
         (dest (arc:aval 'dest props #f))
         (strip (arc:aval 'strip? props #t))
         (group (arc:aval 'group props 0))
         (owner (arc:aval 'owner props 0))
         (mode (arc:aval 'mode props #f)))
    
    (arc:log 'debug "install ... " src)
    
    (if (string? src)
        (arc:install-file src dest strip group owner mode))
    (if (list? src)
        (let loop ((fn src))
          (if (null? fn)
              'done
              (begin
                (arc:install-file (car fn) dest strip group owner mode)
                (loop (cdr fn)))))) )
  '<unspecified>)

(arc:register-task 'install arc:install arc:install-keywords)


(define (arc:install-file src dest strip group owner mode)
  (if (arc:sys.file-exists? src)
      (let* ((srcp (arc:string->path src))
             (destp (arc:string->path dest))
             (ndest (arc:path->string (arc:path-append 
                                       destp (arc:path-last-comp srcp)))))
        (arc:sys.mkdirs dest)
        (arc:sys.copy-file src ndest)
        
        ;; TODO: change owner, group flags, strip binary date
        
        (if mode
            (arc:sys.chmod ndest mode)))
      (arc:log 'info "source file not found")))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
