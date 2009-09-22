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

(arc:provide 'task-untar)


(arc:log 'debug "loading 'untar' task")


(define %arc:untar-tmp-list% ".arc/tar-list")


;; extract a (file) tar archive (using the gnu tar tool).  Returns a list
;; of all files extracted; if no file could be extracted returns the empty
;; list '()
;;
;; Keywords:
;; :tarfile STRING
;; the path of the tar archive.  Must be a single file.  this is
;; required
;;
;; :absolute-paths? BOOLEAN
;; don't strip leading '/' from paths
;;
;; :zip-mode (none|compress|gzip|bzip2)
;; use the zip mode during creation of tar archive
;;
;; :force? BOOLEAN
;; if #t removes old files before extracting new ones; #f keeps the old ones; 
;; default is #t
;;
;; :preserve-perm? BOOLEAN
;; preserves all file system permission on extraction; default is #t
;;
;; :outdir STRING
;; extracts the files into the directory given instead of the current directory
;;
;; RETURNS
;; a list of all extracted files and directories, if the extraction
;; succeeded.  The list is empty if the task failed. (STIRNG-LIST)

(define arc:untar-keywords '((tarfile         string  required)
			     (absolute-paths? boolean optional)
			     (force?          boolean optional)
			     (preserve-perm?  boolean optional)
			     (outdir          string  optional)
			     (zip-mode        symbol  optional)) )

(define (arc:untar props body)
  (let* ((tfn* (arc:aval 'tarfile props ""))
	 (force (arc:aval 'force? props #f))
         (perm (arc:aval 'preserve-perm? props #t))
         (absp (arc:aval 'absolute-paths? props #f))
         (outdir (arc:aval 'outdir props #f))
         (olddir (if outdir
                     (sys:getcwd) 
                     #f))
         (tfn (if outdir
                  (arc:path->string (arc:path-append (arc:string->path olddir) tfn*))
                  tfn*))
         (retv '()) )
    
    (if (= (string-length tfn) 0)
	(arc:log 'fatal "invalid tar-file name"))

    (if (and outdir
             (not (sys:file-directory? outdir)))
        (arc:log 'info "out directory '" outdir "' not found")

        (begin
          (arc:log 'debug "extract tar '" tfn "'")
          (if (not (sys:file-exists? tfn))
              (arc:log 'info "tar file '" tfn "' does not exist")
              (let ((tarcmd (string-append "tar xv"
                                           (if absp "P" "")
                                           (if perm "p" "")
                                           (case (arc:aval ':zip-mode props #f)
                                             ((gzip) "z")
                                             ((compress) "Z")
                                             ((bzip2) "I")
                                             (else ""))
                                           (if (not force) "k" "")
                                           "f "
                                           tfn 
                                           " > " %arc:untar-tmp-list%)) )
                (if outdir
                    (sys:change-dir outdir))

                ;; create the base directory for the tmp directory
                (sys:mkdirs (arc:path->string (arc:path-without-last-comp 
                                               (arc:string->path %arc:untar-tmp-list%))))
                
                (arc:display tarcmd #\nl)
                (if (not (= (system tarcmd) 0))
                    (if (not force)
                        (arc:log 'info 
                                 "some or all files could not "
                                 "be extracted from tar '" tfn "'")
                        (arc:log 'info 
                                 "failed to extract tar file '" 
                                 tfn "'")))
                (if (sys:file-exists? %arc:untar-tmp-list%)
                    (let ((lf (arc:-tar-read-list-of-extracted-files %arc:untar-tmp-list%)))
                      (sys:remove-file %arc:untar-tmp-list%)
                      (set! retv lf)))
                (if outdir
                    (sys:change-dir olddir)) ))))
    retv))

(arc:register-task 'untar arc:untar arc:untar-keywords)



(define (arc:-tar-read-list-of-extracted-files fn)
  (let* ((port (open-input-file fn))
         (cwd (arc:path-cwd))
         (retv '()))
    (do ((ln (arc:readline port) (arc:readline port)))
        ((eof-object? ln) #t)
      (set! retv (append retv 
                         (list (arc:path->string 
                                (arc:path-append cwd
                                                 (arc:string->path ln)))))) )
    (close-input-port port)
    retv))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
