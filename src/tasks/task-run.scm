;;  This file is part of the arc package
;;  Copyright (C) 2009 by Gregor Klinke
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

;; $Id: task-gzip.scm,v 1.2 2003/04/19 01:08:38 eyestep Exp $

(arc:provide 'task-run)


(arc:log 'debug "loading 'run' task")

;; Runs a system tool.
;;
;; Keywords:
;; :cmd STRING
;; the path to the command to run.  Must be a single file.  This is
;; required
;;
;; :args STRING-LIST
;; the arguments to be passed to the command.  If not given treated as
;; empty argument list.
;;
;; :indir STRING
;; specifies the directory which is to be used as current working directory
;; when executing the command.  This may be useful when writing output to
;; (temp) files or using other files.  This is optional and if not given
;; the current working directory is used.
;;
;; :capture? BOOLEAN
;; indicates whether the tools output should be captured an returned as
;; string.  By default the output is send to stdout/stderr as normally.
;;
;; :uptodate? STRING-LiST
;; the arguments are explicit file dependencies on this task.  It will only
;; run if any of the given file names has changed since the last run.  If 
;; not given the task will be run always.
;;
;; RETURNS
;; an attrval with the default value being the return status of the
;; command.  This depends on the nature of the command but normally 0
;; indicates success.  If the property capture?: was set to #t the output
;; on stdout is returned for the id 'stdout and that of stderr on 'stderr.

(define arc:run-keywords '((cmd string required)
                           (args strlist optional)
                           (indir string optional)
                           (capture? boolean optional)
                           (uptodate? strlist optional) ))

(define (arc:-run-run cmd cmd-line capture?)
  (let* ((av (arc:attrval))
         (retv 0)
         (stdout-file #f)
         (stderr-file #f))
    (arc:attrval-default-id! av 'return-value)

    (if capture? 
        (begin
          (set! stdout-file (arc:temp-file-name "capture-out"))
          (set! stderr-file (arc:temp-file-name "capture-err"))
          (set! cmd-line (string-append cmd-line " 1> " stdout-file
                                        " 2> " stderr-file))))

    (arc:display cmd-line #\nl)
    (set! retv (system cmd-line))
    (arc:attrval-default! av retv)

    (if (not (equal? retv 0))
        (arc:log 'error "external tool '" cmd "' failed"))

    (if stdout-file
        (begin
          (arc:attrval-set! av 'stdout (hea:read-string-from-file stdout-file))
          (sys:remove-file stdout-file)))
    (if stderr-file
        (begin
          (arc:attrval-set! av 'stderr (hea:read-string-from-file stderr-file))
          (sys:remove-file stderr-file)))
    av))
  
(define (arc:run props body)
  (let* ((cmd (arc:aval 'cmd props ""))
         (args (arc:aval 'args props (list)))
         (indir (arc:aval 'indir props #f))
         (capture? (arc:aval 'capture? props #f))
         (uptodate? (arc:aval 'uptodate? props #f))

         (full-cmd-line (string-append cmd " " 
                                       (arc:string-list->string* args " "))) )

    (arc:log 'debug "run '" full-cmd-line "'")

    (if (and uptodate?
             (not (arc:deps-run-needs-rerun? full-cmd-line uptodate?)))
        (let* ((av (arc:attrval)))
          (arc:attrval-default-id! av 'return-value)
          (arc:attrval-default! av 0)
          av)

        (if (string? indir)
            (let ((cwd (path:cwd))
                  (retv (arc:attrval)))
              (sys:change-dir indir)
              (set! retv (arc:-run-run cmd full-cmd-line capture?))
              (sys:change-dir cwd)
              retv)
            (arc:-run-run cmd full-cmd-line capture?)) )) )

(define (arc:deps-run-needs-rerun? key sfiles)
  (let* ((depkey (arc:run-command->symbol key))
         (deps0 (arc:make-deps depkey))
         (deps (arc:deps-get-deps depkey depkey
                                  (lambda (src dest)
                                    (for-each (lambda (sf)
                                                (arc:deps-set-deps! deps0 sf))
                                              sfiles)
                                    deps0)))
         (retv #f))
    (arc:log 'debug "use key '" depkey "'")

    (set! retv (if (not (list? deps))
                   #t
                   (let loop ((sf sfiles))
                     (if (null? sf)
                         #f
                         (let* ((dep (car sf))
                                (sf-mtime (sys:mtime dep))
                                (dep-mtime (arc:deps-mtime deps dep)))
                           (or (not dep-mtime)
                               (< dep-mtime sf-mtime)
                               (loop (cdr sf)))) ))) )
    (arc:deps-update-mtimes deps)
    retv))


(define (arc:run-command->symbol key)
  (arc:string-list->string 
   (map (lambda (c)
          (case c
            ((#\space) "%20")
            ((#\/)     "%2f")
            ((#\\)     "%5c")
            ((#\nl)    "%0a")
            ((#\cr)    "%0d")
            ((#\")     "%22")
            ((#\()     "%28")
            ((#\))     "%29")
            ((#\{)     "%7b")
            ((#\})     "%7d")
            ((#\[)     "%5b")
            ((#\])     "%5d")
            ((#\|)     "%5c")
            (else (string c))))
        (string->list key))))

(arc:register-task 'run arc:run arc:run-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
