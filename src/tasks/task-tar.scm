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

(arc:provide 'task-tar)


(arc:log 'debug "loading 'tar' task")


;; creates a (file) tar archive (using the gnu tar tool).  Returns the name
;; of the file which had been created or #f, if no file had been created
;;
;; Keywords:
;; :tarfile STRING
;; the path of the tar archive.  Must be a single file.  this is
;; required
;;
;; :files STRLIST
;; a list of files (pathes) to add to the archive.  Directories are added 
;; recursively.
;;
;; :deref-symlinks? BOOLEAN
;; add the files a symlink points to?  (-h)
;;
;; :absolute-paths? BOOLEAN
;; don't strip leading '/' from paths
;;
;; :zip-mode (none|compress|gzip|bzip2)
;; use the zip mode during creation of tar archive
;;
;; :force? BOOLEAN
;; if #t removes tar file if it exists, and creates then the new tar file; if #f doesn't 
;; create a tar file, if it exists already (#t is default)
;;
;; RETURNS
;; The name of the archive created or #f if the task failed

(define arc:tar-keywords '((tarfile         string  required)
                           (files           strlist optional)
                           (deref-symlinks? boolean optional)
                           (absolute-paths? boolean optional)
                           (force?          boolean optional)
                           (zip-mode        symbol  optional)) )
(define (arc:tar props body)
  (let* ((tfn (arc:aval 'tarfile props ""))
	 (files (arc:aval 'files props ()))
         (force (arc:aval 'force? props #t))
	 (deref (arc:aval 'deref-symlinks? props #f))
         (absp (arc:aval 'absolute-paths? props #f))
         (really-work #t) )
    
    (if (= (string-length tfn) 0)
	(arc:log 'fatal "invalid tar-file name"))
    
    (if (arc:sys 'file-exists? tfn)
        (if force
            (arc:sys 'remove-file tfn)
            (begin
              (arc:log 'info "don't create '" tfn "', older version exists")
              (set! really-work #f))))

    (if really-work
        (begin
          (arc:log 'debug "create tar '" tfn "'")
          (let ((tarcmd (string-append "tar cl"
                                       (if absp "P" "")
                                       (if deref "h" "")
                                       (case (arc:aval 'zip-mode props #f)
                                         ((gzip) "z")
                                         ((compress) "Z")
                                         ((bzip2) "I")
                                         (else ""))
                                       "f "
                                       tfn " "
                                       (arc:string-list->string* files
                                                                 " "))))
            (arc:display tarcmd #\nl)
            (if (not (equal? (system tarcmd) 0))
                (begin
                  (arc:log 'error "failed to create tar file '" tfn "'")
                 #f)
                tfn)))
        #f) ))


(arc:register-task 'tar arc:tar arc:tar-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
