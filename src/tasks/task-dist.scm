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

;; $Id: task-dist.scm,v 1.2 2003/04/19 01:08:38 eyestep Exp $

(arc:provide 'task-dist)
(arc:require 'task-tar)
(arc:require 'task-mkdir)
(arc:require 'task-copy)

(arc:log 'debug "loading 'dist' task")


;; Prepares a distribution 
;;
;; Keywords:
;; :distnm STRING
;; the distribution name
;;
;; :version STRING
;; the version to use for the archive 
;;
;; :files str-list required
;; the list of files to include in the distribution
;;
;; RETURNS
;; the name of the distribution file created

(define arc:dist-keywords '((distnm string optional)
                            (version string optional)
                            (files strlist required)) )
(define (arc:dist props body)
  (let* ((distnm (if (arc:context-project (arc:context-current))
                     (arc:context-project (arc:context-current))
                     (arc:aval 'distnm props "xyz")))
         (version (if (arc:context-version (arc:context-current))
                      (arc:context-version (arc:context-current))
                      (arc:aval 'version props "0.0.0")))
         (dist-dir (string-append distnm "-" version))
         (dist-file (string-append dist-dir ".tar.gz")) )

    (arc:log 'debug "dist ... " dist-file)
    
    (if (arc:sys 'file-exists? dist-file)
        (arc:sys 'remove-file dist-file))
    
    (if (arc:sys 'file-exists? dist-dir)
        (arc:sys 'remove-dir dist-dir))

    (arc:call-task 'mkdir 
                   (list 'dir dist-dir)
                   #f)
    (arc:call-task 'copy
                   (list 'files (arc:aval 'files props ())
                         'todir dist-dir
                         'force? #t)
                   #f)
    (arc:call-task 'tar
                   (list 'tarfile dist-file
                         'files (list dist-dir)
                         'force? #t
                         'zip-mode 'gzip)
                   #f)
    (arc:call-task 'delete
                   (list 'path dist-dir
                         'recursive? #t)
                   #f)
;;; @old
;    (arc:mkdir ':dir dist-dir)
;    (arc:copy ':files (arc:aval ':files props ())
;              ':todir dist-dir
;              ':force? #t)
    
;    (arc:tar ':tarfile dist-file
;             ':files (list dist-dir)
;             ':force? #t
;             ':zip-mode 'gzip)
;    (arc:delete ':path dist-dir
;                ':recursive? #t)
    
    dist-file))

(arc:register-task 'dist arc:dist arc:dist-keywords)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
