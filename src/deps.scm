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

;; $Id: deps.scm,v 1.5 2003/04/23 18:48:24 eyestep Exp $


;; ----------------------------------------------------------------------
;; generic dependency support.
;;
;; most languages will use a (language) specific dependecy support, but
;; some functionality is supported by allmost all languages.
;;
;; this functions load and save (arc specific) dependency files, whereas
;; the language specific construction and setup of dependency files is
;; implementented in the appropriate task-*-deps.scm file
;;
;; the format of the dependency list: it returns a list with two elements,
;; the first is the target (in the makefile notation), and the second is an
;; list of cons'.  Each cons in the list have in its car the dependencie's
;; name and in the cdr the modification time
;;
;; example:
;; ("target.o" (("target.c" . 0) ("../include/types.h" . 0) 
;;    ("../include/gc.h" . 0)))
;; ----------------------------------------------------------------------

;; the directory where to put dependency files
(define %arc:deps-directory% ".arc/deps")

(arc:register-built-resource ".arc" 'recursive)


(define %arc:deps-mode% 'database)

(define (arc:arc-tmp-directory)
  (let ((p (arc:path-append 
            (arc:string->path 
             (if (arc:context-script-home (arc:context-current))
                 (arc:context-script-home (arc:context-current))
                 ()))
            ".arc")))
    (if (arc:sys 'file-exists? (arc:path->string p))
        p
        (begin
          (arc:sys 'mkdirs (arc:path->string p))
          p))))

;; returns the dependency directory as a path object
(define (arc:deps-directory)
  (let ((p (arc:path-append 
            (arc:string->path 
             (if (arc:context-script-home (arc:context-current))
                 (arc:context-script-home (arc:context-current))
                 ()))
            (arc:string->path %arc:deps-directory%))))
    (if (arc:sys 'file-exists? (arc:path->string p))
        p
        (begin
          (arc:sys 'mkdirs (arc:path->string p))
          p))))

(define (arc:deps? deps)
  (and deps
       (list? deps)))

;; creates a new dependency object for the target file ofile
(define (arc:make-deps ofile)
  (list ofile '()))

;; sets a dependency for file dfile to a dependency object.  Returns the
;; updated deps object
(define (arc:deps-set-deps! deps dfile)
  (let* ((aslot (assoc dfile (cadr deps))))
    (if (not aslot)
        (set-cdr! deps (list (append (cadr deps)
                                     (list (cons dfile 0))))))
    deps))
  
;; sets the target object "sfile" of a dependency; returns the updated deps
;; object
(define (arc:deps-set-target! deps dfile)
  (set-car! deps dfile)
  deps)

(define (arc:deps-target deps)
  (car deps))


;; creates a unified file name for a source name
(define (arc:deps-file-name fn)
  (let* ((fnp (arc:string->path fn))
         (fnbn (arc:path-append-ext fnp "P"))
         (dfn (arc:path-append (arc:deps-directory)
                               (arc:deps-flatten-file-name fnbn))))
    (arc:path->string dfn)))

;; makes a path 'flat': that means concats the path steps with %2d (=/)
;; into a string, so the path can be used for a single file name
(define (arc:deps-flatten-file-name fn)
  (let ((x (arc:reduce (lambda (elt lst)
                         (if (equal? elt "/")
                             (cons "%2f" lst)
                             (cons "%2d" (cons elt lst))) )
                       '()
                       fn)))
    (arc:string-list->string x)))

(define (arc:load-deps-file fn)
  (if (arc:sys 'file-exists? fn)
      (let* ((port (open-input-file fn))
             (deps (read port)))
        (close-input-port port)
        deps)
      #f))

(define (arc:save-deps-file fn deps)
  (if (arc:sys 'file-exists? fn)
      (delete-file fn))
  (let ((port (open-output-file fn)))
    ;; produce some informational stuff
    (display ";; don't edit this file" port) (newline port)
    (display ";; automatic created by arc vr " port) 
    (display %arc:version% port) (newline port)
    ;; write the real dependency thing
    (write deps port)
    (close-output-port port)))


;; ----------------------------------------------------------------------
;; file mtime stuff
;; ----------------------------------------------------------------------
;; walks on a dependency list as generated by parse-c-deps-file and fills
;; in the modification times in the deps cons'
(define (arc:deps-determine-mtime deps)
  (for-each (lambda (fc)
              (set-cdr! fc (arc:sys 'mtime (car fc))) )
            (cadr deps))
  deps)

(define (arc:mtime-file-changed? deps ofile)
  (let ((retv (let ((mtime (arc:sys 'mtime ofile))
                    (dps (arc:deps-determine-mtime deps)))
                (let loop ((fc (cadr dps)))
                  (if (null? fc)
                      #f
                      (or (< mtime (cdar fc))
                          (loop (cdr fc))) )))))
    (arc:log 'debug "mtime changed of " ofile " -> " retv)
    retv))
      


;; ----------------------------------------------------------------------
;; find and update the dependency
;; ----------------------------------------------------------------------
;; loads the dependencies of a target file ofile.  If no such dependencies
;; set is found (=has not been generated yet), it calls the function proc
;; with two arguments: sfile (=the source file) and ofile (=the target file).
;; The function is expected to return a valid deps object or #f if the 
;; the dependency couldn't be computed.
(define (arc:deps-get-deps sfile ofile proc)
  (case %arc:deps-mode%
    ((sep-files) (arc:deps-get-deps-single-file sfile ofile proc))
    ((database) (arc:deps-get-deps-database sfile ofile proc))
    (else
     #f)))

(define (arc:deps-get-deps-single-file sfile ofile proc)
  (let* ((fn (arc:deps-file-name ofile))
         (df (arc:load-deps-file fn)))
    (if (not df)
        (let* ((deps (apply proc (list sfile ofile))))
          (if (and deps
                   (arc:deps? deps))
              (arc:save-deps-file fn deps))
          deps)
        df)))

;; ----------------------------------------------------------------------
;; store the dependencies in one single database file
;; to achive acceptable performance on large projects (i.e. having many
;; source files), we're using a hash-table here.
;; ----------------------------------------------------------------------
(define %arc:deps-database% 'unloaded)
(define %arc:deps-database-dirty-handler% #f)
(define arc:deps-db-associator #f)
(define arc:deps-db-inquirer #f)
(define %arc:deps-database-size% 1024)

(define (arc:deps-db-file-name)
  (arc:path->string (arc:path-append (arc:deps-directory)
                                     "deps.db")))

(define (arc:deps-db-load-db dbn)
  (if (eq? %arc:deps-database% 'unloaded)
      (begin
        (set! %arc:deps-database% (or (arc:load-deps-file dbn)
                                      (make-hash-table %arc:deps-database-size%)))
        (set! arc:deps-db-associator (hash-associator string=?))
        (set! arc:deps-db-inquirer (hash-inquirer string=?)))))
  
(define (arc:deps-db-set-deps deps)
  (let ((dbn (arc:deps-db-file-name)))
    (arc:deps-db-load-db dbn)
 
    (let* ((key (arc:deps-target deps)))
      (arc:deps-db-associator %arc:deps-database% key deps)
;    (let* ((key (arc:deps-target deps))
;           (aslot (assoc key %arc:deps-database%)))
;      (if aslot
;          (set-cdr! aslot (cdr deps))
;          (set! %arc:deps-database% 
;                (append %arc:deps-database% (list deps))))
      (set! %arc:deps-database-dirty-handler%
            (lambda ()
              (arc:save-deps-file dbn %arc:deps-database%))))))
            

(define (arc:deps-db-get-deps key)
  (let ((dbn (arc:deps-db-file-name)))
    (arc:deps-db-load-db dbn)
    (arc:deps-db-inquirer %arc:deps-database% key)
    ))
;    (let* ((aslot (assoc key %arc:deps-database%)))
;      (if aslot
;          aslot
;          #f))))


(define (arc:deps-get-deps-database sfile ofile proc)
  (let* ((df (arc:deps-db-get-deps ofile)))
    (if (not df)
        (let* ((deps (apply proc (list sfile ofile))))
          (if (and deps
                   (arc:deps? deps))
              (arc:deps-db-set-deps deps))
          deps)
        df)))

(define (arc:deps-store-database)
  (if (procedure? %arc:deps-database-dirty-handler%)
      (apply %arc:deps-database-dirty-handler% '())))




;; ----------------------------------------------------------------------
;; parsing dependency files as generated by makedeps
;; ----------------------------------------------------------------------
;; parses a dependency file as generate by a c compile and prepared for
;; Makefiles.  These files have the format:
;;
;; target.o: depc.c dep2.h dep3.h \
;;   dep4.h dep5.h dep6.h dep7.h \
;;   dep8.h
;;
;; it returns a list, its first element is the target (in the makefile
;; notation), and its second is an list of cons'.  Each cons in the list
;; have in its car the dependencie's name and in the cdr a zero (this is to
;; take the modification time later)
(define (arc:parse-make-deps-file fn)
  (let* ((port (open-input-file fn))
         (state 'pre-ws)
         (buf ())
         (deps (arc:make-deps ""))
         )
    (do ((c (read-char port) (read-char port)))
        ((eof-object? c) #t)
      (case state
        ((pre-ws) (case c
                    ((#\space #\nl #\tab #\cr) 'ignore)
                    (else (begin
                            (set! state 'targ)
                            (set! buf (cons c buf))))))
        ((targ) (case c
                  ((#\:) (begin
                           (set! state 'targ-ws)
                           (arc:deps-set-target! deps 
                                                 (list->string (reverse buf)))
                           (set! buf ())))
                  (else (set! buf (cons c buf)))))
        ((targ-ws) (case c
                     ((#\space #\nl #\tab #\cr) 'ignore)
                     (else (begin 
                             (set! state 'dep)
                             (set! buf (cons c buf))))))
        ((dep) (case c
                 ((#\space #\nl #\tab #\cr) 
                  (begin
                    (set! state 'ws)
                    (arc:deps-set-deps! deps (list->string (reverse buf)))
                    (set! buf ())))
                 (else (set! buf (cons c buf)))))
        ((ws) (case c
                ((#\space #\nl #\tab #\cr) 'ignore)
                ((#\\) 'ignore)
                (else (begin
                        (set! state 'dep)
                        (set! buf (cons c buf))))))
        ))
    (close-input-port port)
    
    deps))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
