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

;; $Id: task-copy.scm,v 1.2 2003/04/13 23:45:36 eyestep Exp $

(arc:provide 'task-copy)


(arc:log 'debug "loading 'copy' task")

;; copies a file, a set of files or a complete directory (recursively)
;;
;; Keywords:
;; :dir STRING
;; the path of the directory to copy.  The directory is copied recursively.
;;
;; :files STRLIST
;; a list of files to copy
;;
;; :file STRING
;; copy exactly one file
;;
;; :todir STRING
;; the destination directory to copy to.  Works with :dir and :files
;;
;; :tofile STRING
;; the destination to copy the source file to.  Works with :file only
;;
;; :force? BOOLEAN
;; if #t overwrite an existing destination file.
;;
;; :flatten? BOOLEAN
;; if #f copies for :files and :file (:todir) the complete directory hierachy; if #t
;; it "flattens" the hierachy by copying only the files into one directory.  #f is
;; default
;;
;; RETURNS
;; <unspecified>

(define arc:copy-keywords '((dir string optional)
                            (files strlist optional)
                            (file string optional)
                            (todir string optional)
                            (tofile string optional)
                            (flatten? boolean optional)
                            (force? boolean optional)) )

(define (arc:copy props body)
  (let* ((dir (arc:aval 'dir props #f))
         (files (arc:aval 'files props #f))
         (file (arc:aval 'file props #f))
         (todir (arc:aval 'todir props #f))
         (tofile (arc:aval 'tofile props #f))
         (force (arc:aval 'force? props #f))
         (flatten (arc:aval 'flatten? props #f))
         (mode 'unknown) )
    
    (if dir
        (if todir
            (set! mode 'dir-to-dir)
            (if tofile
                (arc:log 'fatal "copy: can't copy dir to file")
                (arc:log 'fatal "copy: no target specifed")))
        (if files
            (if todir
                (set! mode 'files-to-dir)
                (if tofile
                    (arc:log 'fatal "copy: can't copy multiple files to file")
                    (arc:log 'fatal "copy: no target specifed")))
            (if file
                (if todir
                    (set! mode 'file-to-dir)
                    (if tofile
                        (set! mode 'file-to-file)
                        (arc:log 'fatal "copy: no target specifed")))
                (arc:log 'fatal 
                         "copy: neither :dir, :files, nor :file specified"))))
    
    (case mode
      ((dir-to-dir) (arc:-copy-dir-to-dir dir todir force))
      ((files-to-dir) (arc:-copy-files-to-dir files todir force flatten))
      ((file-to-dir) (arc:-copy-file-to-dir file todir force flatten))
      ((file-to-file) (arc:-copy-file-to-file file tofile force))
      (else (arc:log 'error "unknown copy mode"))) )
  '<unspecified>)

(arc:register-task 'copy arc:copy arc:copy-keywords)



(define (arc:-copy-dir-to-dir dir todir force)
  (let ((copy #f))
    (if (not (arc:sys.file-exists? dir))
        (arc:log 'info "copy: source dir '" dir "' not found")
        (if (arc:sys.file-exists? todir)
            (if (not force)
                (arc:log 'info
                         "copy: destination directory exists. don't touch")
                (set! copy #t))
            (set! copy #t)))
    (if copy
        (arc:sys.copy-dir dir todir)) ))

(define (arc:-copy-files-to-dir files todir force flatten)
  (let ((todirp (arc:string->path todir)))
    (if (not (arc:sys.file-directory? todir))
        (arc:log 'info "copy: dest directory doesn't exists / or no directory")
        (let loop ((f files))
          (if (null? f)
              #t
              (begin
                (let* ((fp (if flatten
                               (arc:path-last-comp (arc:string->path (car f)))
                               (arc:path-abbreviate (arc:string->path (car f)))))
                       (tp (arc:path-append todirp fp)) )
                  (if (and (arc:sys.file-exists? (arc:path->string tp))
                           (not force))
                      (arc:log 'info 
                               "copy: file '" (arc:path->string tp) 
                               "' exists. don't touch")
                      (arc:-copy-file (car f) (arc:path->string tp))))
                (loop (cdr f))))))) )

(define (arc:-copy-file-to-dir file todir force flatten)
  (let ((todirp (arc:string->path todir)))
    (if (not (arc:sys.file-directory? todir))
        (arc:log 'info 
                 "copy: destination directory is not an "
                 "directory/doesn't exist")
        (if (not (arc:sys.file-exists? file))
            (arc:log 'info "copy: source file not found: " file)
            (let* ((fp (if flatten
                           (arc:path-last-comp (arc:string->path file))
                           (arc:path-abbreviate (arc:string->path file))) )
                   (tp (arc:path-append (arc:string->path todir) fp)) )
              (if (and (arc:sys.file-exists? (arc:path->string tp)) (not force))
                  (arc:log 'info
                           "copy: file '" (arc:path->string tp) 
                           "' exists. don't touch")
                  (arc:-copy-file file (arc:path->string tp)))) ))))

(define (arc:-copy-file-to-file file tofile force)
  (if (arc:sys.file-directory? tofile)
      (arc:log 'info "copy: destination is a directory, not a file")
      (if (not (arc:sys.file-exists? file))
          (arc:log 'info "copy: source file not found (" file")")
          (if (arc:sys.file-exists? tofile)
              (if (not force)
                  (arc:log 'info "copy: destination file found. don't touch")
                  (arc:-copy-file file tofile))
              (arc:-copy-file file tofile)))) )

(define (arc:-copy-file src dest)
  (let* ((destp (arc:string->path dest))
         (dirs (arc:path-without-last-comp destp)))
    (if (> (arc:path-length dirs) 0)
        (arc:sys.mkdirs (arc:path->string dirs)))
    (arc:sys.copy-file src dest)))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
