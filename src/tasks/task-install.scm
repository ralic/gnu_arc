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

(define arc:install-keywords '((src    (string list)     required)
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

    (cond
     ((string? src) (arc:install-file src dest strip group owner mode))
     ((list? src)
      (let loop ((fn src))
        (if (null? fn)
            'done
            (begin
              (arc:install-elt (car fn) dest strip group owner mode)
              (loop (cdr fn))))))
     ((arc:attrval? src)
      (arc:install-elt src dest strip group owner mode))
     ))
  '<unspecified>)

(arc:register-task 'install arc:install arc:install-keywords)

(define (arc:install-file src dest strip group owner mode)
  (if (arc:sys 'file-exists? src)
      (let* ((srcp (arc:string->path src))
             (destp (arc:string->path dest))
             (ndest (arc:path->string (arc:path-append 
                                       destp (arc:path-last-comp srcp)))))
        (arc:display "install " src " to " ndest "(" (or mode 'exec) ")" #\nl)
        (arc:sys 'mkdirs dest)
        (arc:sys 'copy-file src ndest)
        
        ;; TODO: change owner, group flags, strip binary date
        
        (if mode
            (arc:sys 'chmod ndest mode))
        ndest)
      (begin
        (arc:log 'info "source file not found: " src)
        #f)))



(define (arc:install-elt src dest strip group owner mode)
  (cond
   ((string? src) (arc:install-file src dest strip group owner mode))
   ((arc:attrval? src) (arc:install-attrval src dest strip group owner mode))

   (else
    (arc:log 'error "install: bad element type in source list: " src))))

(define (arc:install-attrval src dest strip group owner mode)
  ;; check installable information: libraries
  (let ((static-lib (arc:attrval-ref src 'static))
        (shared-lib (arc:attrval-ref src 'shared))
        (shared-lib-soname (arc:attrval-ref src 'shared-soname))
        (shared-lib-linkname (arc:attrval-ref src 'shared-linker-name)))

    (if static-lib
        (arc:install-file static-lib dest strip group owner mode))
    (if shared-lib
        (let* ((real-lib (arc:install-file shared-lib dest strip group 
                                           owner mode)))
          (if (not (equal? shared-lib shared-lib-soname))
              (let ((target (arc:path->string
                             (arc:path-append 
                              (arc:string->path dest)
                              (arc:path-last-comp 
                               (arc:string->path shared-lib-soname))))))
                (arc:sys 'symlink real-lib target)
                (if mode
                    (arc:sys 'chmod target mode))))

          (if (and (not (equal? shared-lib shared-lib-linkname))
                   (not (equal? shared-lib-soname shared-lib-linkname)))
              (let ((target (arc:path->string
                             (arc:path-append 
                              (arc:string->path dest)
                              (arc:path-last-comp 
                               (arc:string->path shared-lib-linkname))))))
                (arc:sys 'symlink real-lib target)
                (if mode
                    (arc:sys 'chmod target mode))))
          ))
    ))
        


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
