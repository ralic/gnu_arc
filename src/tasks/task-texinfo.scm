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

;; $Id: task-texinfo.scm,v 1.2 2003/04/19 01:08:18 eyestep Exp $

(arc:provide 'task-texinfo)


;; @todo
;; - find all generated info files (to do this, we need to understand the
;; naming conventions used by texinfo, and we do need a better regex file
;; matching system)

(define %arc:texi-pdf-command "texi2pdf")
(define %arc:texi-info-command "texi2pdf")
(define %arc:texi-html-command "texi2pdf")
(define %arc:texi-plain-command "makeinfo --no-headers")


(arc:log 'debug "loading 'texinfo' task")


;; processes texinfo documentation files producing pdf, html, dvi or info
;; files depending on the settings.  Needs a fair good amount of third
;; party software installed (like makeinfo, texi2pdf, texi2dvi, etc.)
;;
;; Keywords:
;; :src STRING
;; the main source file to process.  Only takes the main file; subordinate
;; texinfo files are normally included by texinfo statements.
;;
;; :dest STRING
;; used for html format only to write all html files to that particular 
;; directory.  If the directory does not exist yet, it is created.  This
;; is only used, when split?: is #t, if not given and split?: is #t, for
;; a texinfo file 'foo.texinfo' a directory 'foo' is created by default.
;;
;; :format (info | html | plain | dvi | pdf)
;; specifies the output format to be used.  Defaults to info
;;
;; :split? BOOLEAN
;; if #t split the output into multiple equal sized modules, if #f 
;; splitting is suppressed.  Splitting is default for info and html files;
;; dvi, pdf and plain files are never splitted.
;;
;; RETURNS
;; A list of files generated.  This fails for the info format if the
;; texinfo files contains a @setfilename statements, which generates a
;; filename different from the inputfilename.
;;
;; The outputfile name is created automatically depending on the format:
;; in: x.texi
;; info: x.info, x.info-1, x.info-2
;; pdf: x.pdf
;; html: x.html or: dest/x.html, dest/Intro.html, ...
;; dvi: x.dvi
;; plain: x.txt
(define arc:texinfo-keywords '((src    string  required)
                               (dest   string  optional)
                               (format symbol  optional)
                               (split? boolean optional)))
(define (arc:texinfo props body)
  (let* ((srcnm (arc:aval 'src props #f))
         (format (arc:aval 'format props 'info))
         (no-split (not (arc:aval 'split? props #t)))
         (destnm (case format
                   ((html) (arc:aval 'dest props #f))
                   (else #f)))
         (retv ()))
    
    (arc:log 'debug "texinfo (" format ")... " srcnm)
    
    (case format
      ((plain) (set! retv (arc:-texi-format-plain srcnm)))
      ((info) (set! retv (arc:-texi-format-info srcnm no-split)))
      ((pdf) (set! retv (arc:-texi-format-pdf srcnm)))
      ((dvi) (set! retv (arc:-texi-format-dvi srcnm)))
      ((html) (set! retv (arc:-texi-format-html srcnm no-split destnm)))
      (else (begin
              (arc:log 'error "format '" format "' not supported")
              (set! retv #f))))
    
    retv))


;; formating info text format from texinfo.
(define (arc:-texi-format-info srcnm no-split)
  (let* ((sc (arc:string->path srcnm))
         (destdirp (arc:path-without-last-comp sc))
         (destdir (arc:path->string destdirp))
         (srcp (arc:path->string (arc:path-last-comp sc)))

         (pattern (arc:path->string (arc:path-replace-last-ext 
                                     (arc:path-last-comp sc)
                                     "info")))
         (cwd (arc:sys 'getcwd))
         (cmd (string-append "makeinfo " 
                             srcp
                             (if no-split
                                 (string-append " --no-split") "")))
         (retv #f))
    (arc:sys 'chdir destdir)
    (arc:display cmd #\nl)
    (if (not (= (arc:sys 'system cmd) 0))
        (begin
          (arc:log 'error "texinfo: processing info '" srcnm "' failed")
          (set! retv #f))
        ;; TODO: find all info files!
        (set! retv (arc:-texi-format-info-find-info-files destdirp pattern)))
    
    (arc:sys 'chdir cwd)
    retv))

;; find all xxx.info files in the current working directory
(define (arc:-texi-format-info-find-info-files dirnp pattern)
  (let ((dp (arc:sys 'opendir "."))
        (retv ()))
    (do ((fn (arc:sys 'readdir dp) (arc:sys 'readdir dp)))
        ((not fn) 'done)
      (if (arc:string-prefix? fn pattern)
          (set! retv (append retv (list (arc:path->string 
                                         (arc:path-append dirnp fn)))))))
    (arc:sys 'closedir dp)
    retv))


;; formating plain text format from texinfo.
(define (arc:-texi-format-plain srcnm)
  (let* ((sc (arc:string->path srcnm))
         (destdirp (arc:path-without-last-comp sc))
         (destdir (arc:path->string destdirp))
         (destfnm (arc:path->string (arc:path-replace-last-ext
                                     (arc:path-last-comp sc)
                                     "txt")))
         (srcp (arc:path->string (arc:path-last-comp sc)))
         (cwd (arc:sys 'getcwd))
         (cmd (string-append "makeinfo --no-headers " 
                             srcp
                             " > " destfnm))
         (retv #f))
    (arc:sys 'chdir destdir)
    (arc:display cmd #\nl)
    (if (not (= (arc:sys 'system cmd) 0))
        (begin
          (arc:log 'error "texinfo: processing plain '" srcnm "' failed")
          (set! retv #f))
        (set! retv (list (arc:path->string (arc:path-append destdirp 
                                                            destfnm)))))
    
    (arc:sys 'chdir cwd)
    retv))

;; formating pdf text format from texinfo.
(define (arc:-texi-format-pdf srcnm)
  (let* ((sc (arc:string->path srcnm))
         (destdirp (arc:path-without-last-comp sc))
         (destdir (arc:path->string destdirp))
         (destfnm (arc:path->string (arc:path-replace-last-ext
                                     (arc:path-last-comp sc)
                                     "pdf")))
         (srcp (arc:path->string (arc:path-last-comp sc)))
         (cwd (arc:sys 'getcwd))
         (cmd (string-append "texi2pdf " srcp))
         (retv #f))
    (arc:sys 'chdir destdir)
    (arc:display cmd #\nl)
    (if (not (= (arc:sys 'system cmd) 0))
        (begin
          (arc:log 'error "texinfo: processing pdf '" srcnm "' failed")
          (set! retv #f))
        (set! retv (list (arc:path->string (arc:path-append destdirp 
                                                            destfnm)))))
    
    (arc:sys 'chdir cwd)
    retv))

;; formating dvi text format from texinfo.
(define (arc:-texi-format-dvi srcnm)
  (let* ((sc (arc:string->path srcnm))
         (destdirp (arc:path-without-last-comp sc))
         (destdir (arc:path->string destdirp))
         (destfnm (arc:path->string (arc:path-replace-last-ext
                                     (arc:path-last-comp sc)
                                     "dvi")))
         (srcp (arc:path->string (arc:path-last-comp sc)))
         (cwd (arc:sys 'getcwd))
         (cmd (string-append "texi2dvi " srcp))
         (retv #f))
    (arc:sys 'chdir destdir)
    (arc:display cmd #\nl)
    (if (not (= (arc:sys 'system cmd) 0))
        (begin
          (arc:log 'error "texinfo: processing dvi '" srcnm "' failed")
          (set! retv #f))
        (set! retv (list (arc:path->string (arc:path-append destdirp 
                                                            destfnm)))))
    
    (arc:sys 'chdir cwd)
    retv))


;; formating info text format from texinfo.
(define (arc:-texi-format-html srcnm no-split destnm)
  (let* ((sc (arc:string->path srcnm))
         (destdirp (arc:path-without-last-comp sc))
         (destdir (if destnm
                      (if (arc:path-absolute? (arc:string->path destnm))
                          destnm
                          (arc:path->string (arc:path-append destdirp
                                                             (arc:string->path
                                                              destnm))))
                      (arc:path->string destdirp)))
         (srcp (arc:path->string (arc:path-last-comp sc)))
         
         (pattern (arc:path->string (arc:path-replace-last-ext sc
                                                               "html")))
         (cwd (arc:sys 'getcwd))
         (cmd (string-append "makeinfo --html " 
                             (if no-split
                                 (string-append " --no-split ")
                                 (if destnm
                                     (string-append " -o " destnm " ")
                                     ""))
                             srcp))
         (basedir (arc:path->string destdirp))
         (retv #f))
    
    (arc:sys 'chdir basedir)
    (arc:display cmd #\nl)

    (if (not (= (arc:sys 'system cmd) 0))
        (begin
          (arc:sys 'chdir cwd)
          (arc:throw 'error 
                     (string-append "processing info '" srcnm "' failed"))))

    (arc:sys 'chdir cwd)

    ;; TODO: find all info files!
    (if no-split
        (set! retv pattern)
        (let ((html-path (if destnm
                             (string-append destdir "/*.html")
                             (string-append 
                              (arc:path->string (arc:path-without-last-ext sc))
                              "/*.html"))))
          (set! retv (arc:call-task 'fileset
                                    (list 'pattern html-path)
                                    #f)) ))
    retv))

(arc:register-task 'texinfo arc:texinfo arc:texinfo-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
