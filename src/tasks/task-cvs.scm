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

(arc:provide 'task-cvs)

(arc:log 'debug "loading 'cvs' task")

;; Provides cvs access through arc files 
;;
;; Keywords:
;; func: (checkout | export | update | add | remove | commit | tag)
;; specifies the cvs function to process.  The used set of further
;; parameters depend on the function selected here
;;
;; rev: STRING
;; specifies the revision to work on (checkout, export)
;;
;; local?: BOOLEAN
;; work in the local directory only; by default #f, i.e. work recursively
;; on the subdirectories
;;
;; prune-dir?: BOOLEAN
;; prune empty directories?
;;
;; create-dir?: BOOLEAN
;; create missing directories?
;;
;; files: STRLIST
;; list of files to work on (update, commit, add, delete, tag).
;;
;; module: STRING
;; name of the module to work (checkout, export)
;;
;; dir: STRING
;; the local repository directory (update, add, commit, delete, tag); if
;; not specified use the current working directory
;;
;; compress-level: INTEGER
;; compression level to use (1-9)
;;
;; cvsroot: STRING
;; the cvsroot to use (checkout, commit)
;;
;; todir: STRING
;; (checkout, export) exports/checks out the files to directory DIR,
;; by default use a directory named as the given module
;;
;; RETURNS
;; ???

(define arc:cvs-keywords '((func symbol required)
                           (rev string (opt-xor date))
                           (date string (opt-xor rev))
                           (local? boolean optional)
                           (files strlist optional)
                           (compress-level integer optional)
                           (cvsroot string optional)
                           (module string (opt-xor dir))
                           (dir string (opt-xor module))
                           (todir string optional)
                           (prune-dir? boolean optional)
                           (create-dir? boolean optional)
                           (message string optional)
                           (keymap alist optional)) )
(define (arc:cvs props body)
  (let* ((func (arc:aval 'func props 'none))
         (compress-opt (let ((cl (arc:aval 'compress-level props 7)))
                         (case cl
                           ((0) "")
                           ((1 2 3 4 5 
                               6 7 8 9) (string-append "-z"
                                                       (number->string cl)))
                           (else "-z9"))))
         (cvsroot (arc:aval 'cvsroot props #f))
         (module (arc:aval 'module props #f))
         (dir (arc:aval 'dir props #f))
         (todir (arc:aval 'todir props #f))
         (rev (arc:aval 'rev props #f))
         (date (arc:aval 'date props #f))
         (prune-dir? (arc:aval 'prune-dir? props #f))
         (create-dir? (arc:aval 'create-dir? props #t))
         (local? (arc:aval 'local? props #f))
         (files (arc:aval 'files props '()))
         (message (arc:aval 'message props "arc cvs commit"))
         (keymap (arc:aval 'keymap props arc:cvs-default-keymap))
         )
    
    (case func
      ((checkout) (arc:cvs-handle-checkout compress-opt cvsroot
                                           rev date todir module
                                           prune-dir?))
      ((export) (arc:cvs-handle-export compress-opt cvsroot
                                       rev date todir module))
      ((update) (arc:cvs-handle-update compress-opt rev date dir files 
                                       local? prune-dir? create-dir?))
      
      ((add) (arc:log 'info "cvs: 'add' not implemented yet"))
      ((remove) (arc:log 'info "cvs: 'remove' not implemented yet"))
      ((tag) (arc:log 'info "cvs: 'tag' not implemented yet"))
      ((commit) (arc:log 'info "cvs: 'commit' not implemented yet"))

      (else (arc:log 'fatal "unknown cvs function " func)))
    
    '<unspecified>))

(define (arc:cvs-handle-checkout compress-opt cvsroot
                                 rev date dir module prune-dir?)
  (if (not module)
      (arc:log 'fatal "cvs: 'checkout' needs module"))
  (let* ((tfn (arc:path->string (arc:path-append (arc:arc-tmp-directory)
                                                 "cvs-out")))
         (cvscmd (string-append "cvs "
                                compress-opt " "
                                (if cvsroot
                                    (string-append "-d" cvsroot " ")
                                    "") 
                                "checkout "
                                (if rev
                                    (string-append "-r" rev " ")
                                    (if date
                                        (string-append "-D\"" date "\" ")
                                        ""))
                                (if prune-dir?
                                    "-P "
                                    "")
                                (if dir
                                    (string-append "-d" dir " ")
                                    "")
                                module 
                                " > " tfn)))
    (arc:display cvscmd #\nl)
    
    (if (not (equal? (system cvscmd) 0))
        (begin
          (arc:log 'error "cvs: 'checkout' failed for module '" module "'")
          #f)
        #t)))

(define (arc:cvs-handle-export compress-opt cvsroot
                               rev date dir module)
  (if (not module)
      (arc:log 'fatal "cvs: 'export' needs module"))
  (if (and (not rev)
           (not date))
      (arc:log 'fatal "cvs: 'export' requires either a revision or date"))
  (let* ((tfn (arc:path->string (arc:path-append (arc:arc-tmp-directory)
                                                 "cvs-out")))
         (cvscmd (string-append "cvs "
                                compress-opt " "
                                (if cvsroot
                                    (string-append "-d" cvsroot " ")
                                    "") 
                                "export "
                                (if rev
                                    (string-append "-r" rev " ")
                                    (if date
                                        (string-append "-D\"" date "\" ")
                                        ""))
                                (if dir
                                    (string-append "-d" dir " ")
                                    "")
                                module
                                " > " tfn)))
    (arc:display cvscmd #\nl)
    
    (if (not (equal? (system cvscmd) 0))
        (begin
          (arc:log 'error "cvs: 'export' failed for module '" module "'")
          #f)
        #t)))
        
(define (arc:cvs-handle-update compress-opt rev date dir files 
                               local? prune-dir? create-dir?)
  (let* ((cwd (if dir
                  (sys:getcwd)
                  #f)))
    (if cwd
        (begin
          (arc:log 'verbose "cvs/update: change to dir '" dir "'")
          (sys:change-dir dir)))
    
    (let* ((tfn (arc:path->string (arc:path-append (arc:arc-tmp-directory)
                                                   "cvs-out")))
           (cvscmd (string-append "cvs "
                                  compress-opt " "
                                  "update "
                                  (if local?
                                      "-l "
                                      "")
                                  (if create-dir?
                                      "-d "
                                      "")
                                  (if prune-dir?
                                      "-P "
                                      "")
                                  (if rev
                                      (string-append "-r" rev " ")
                                      (if date
                                          (string-append "-D\"" date "\" ")
                                          ""))
                                  (if files
                                      (arc:reduce string-append "" files)
                                      "")
                                  " > " tfn))
           (retv #t))
      
      (arc:display cvscmd #\nl)
      
      (if (not (equal? (system cvscmd) 0))
          (begin
            (arc:log 'error "cvs: 'update' failed for module '" module "'")
            (set! retv #f)))
      
      (if cwd 
          (sys:change-dir cwd))
      
      ;; @todo write the output for the update command into a temporary file
      ;; and parse the output lines for the "U A R M C ?" characters.  This
      ;; output can be returned as returnvalue from task-cvs.
      
      retv)
    ))



(define arc:cvs-default-keymap
  '(;; image files
    (".jpg"     "-kb")
    (".jpeg"    "-kb")
    (".gif"     "-kb")
    (".png"     "-kb")
    (".tiff"    "-kb")
    (".tif"     "-kb")
    (".xcf"     "-kb")
    (".bmp"     "-kb")

    ;; archives
    (".jar"     "-kb")
    (".tar.gz"  "-kb")
    (".tar.bz2" "-kb")
    (".tar.z"   "-kb")
    (".tgz"     "-kb")
    (".a"       "-kb")
    (".gz"      "-kb")
    (".bz2"     "-kb")
    (".z"       "-kb")
    (".zip"     "-kb")

    ;; sound files
    (".wav"     "-kb")
    (".mp3"     "-kb")
    (".au"      "-kb")

    ;; movie files
    (".mpeg"   "-kb")
    (".mpg"    "-kb")
    (".avi"    "-kb")

    ;; misc binaries
    (".class"   "-kb")
    (".o"       "-kb")
    (".wmf"     "-kb")
    ))

(arc:register-task 'cvs arc:cvs arc:cvs-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
    

