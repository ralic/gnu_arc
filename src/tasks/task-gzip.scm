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

;; $Id: task-gzip.scm,v 1.1 2003/04/12 00:39:29 eyestep Exp $

(arc:provide 'task-gzip)


(arc:log 'debug "loading 'gzip' task")

;; compresses a single file using the gzip tool.  The original file is
;; unchanged.
;;
;; Keywords:
;; :file STRING
;; the path of the file to compress.  Must be a single file.  this is
;; required
;;
;; :zipfile STRING
;; the path of the zipfile to produce.  if such a file exists already, 
;; the target is removed first.  this is required
;;
;; :compress-level integer
;; chooses the compression level, must be between 1-9, default is 6
;;
;; RETURNS
;; <unspecified>

(define arc:gzip-keywords '((file string required)
                            (zipfile string required)
                            (compress-level integer optional)) )
(define (arc:gzip props body)
  (let* ((fn (arc:aval 'file props ""))
	 (zfn (arc:aval 'zipfile props ""))
	 (cl (case (arc:aval 'compress-level props 6)
	       ((1) "-1 ")
	       ((2) "-2 ")
	       ((3) "-3 ")
	       ((4) "-4 ")
	       ((5) "-5 ")
	       ((6) "-6 ")
	       ((7) "-7 ")
	       ((8) "-8 ")
	       ((9) "-9 ")
	       (else (begin
		       (arc:log 'error "bad compression level '" 
				(arc:aval ':compress-level props 6) "'")
		       "")))) )

    (arc:log 'debug "gzip '" fn "' to '" zfn "'")
    
    (if (or (= (string-length fn) 0)
	    (= (string-length zfn) 0))
	(arc:log 'fatal "invalid file names in gzip"))

    (if (arc:sys.file-exists? fn)
	(begin
	  (if (arc:sys.file-exists? zfn)
	      (arc:sys.remove-file zfn))
	  
	  (let ((gzipcmd (string-append "gzip -c " cl fn " > " zfn)))
	    (arc:display gzipcmd #\nl)
	    (if (not (equal? (system gzipcmd) 0))
		(arc:log 'error "failed to gzip file '" fn "'")
		#t)))
	(arc:log 'info "gzip: file '" fn "' does not exist")) )
  '<unspecified>)

(arc:register-task 'gzip arc:gzip arc:gzip-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
