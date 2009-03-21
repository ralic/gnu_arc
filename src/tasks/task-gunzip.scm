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

(arc:provide 'task-gunzip)


(arc:log 'debug "loading 'gunzip' task")

;; decompresses a single file using the gzip tool.  The original file
;; is unchanged.
;;
;; Keywords:
;; :file STRING
;; the path of the file to decompress.  This is required
;;
;; :dest STRING
;; the path of the destinatio file to produce.  this is required
;;
;; :force? BOOLEAN
;; by default the zipped file is decompress only, if the zipped file's
;; modification time is later than the destination file's modtime; if #t
;; the zip file is decompressed always; the default is #f (don't overwrite)
;;
;; RETURNS
;; <unspecified>

(define arc:gunzip-keywords '((file string required)
                              (dest string required)
                              (force? boolean optional)) )
(define (arc:gunzip props body)
  (let* ((fn (arc:aval 'file props ""))
	 (dest (arc:aval 'dest props ""))
	 (force? (arc:aval 'force? props #f))
	 (really-do #t))
    
    (arc:log 'debug "gunzip '" fn "' to '" dest "'")
    
    (if (or (= (string-length fn) 0)
	    (= (string-length dest) 0))
	(arc:log 'fatal "invalid file names in gunzip"))

    (if (arc:sys 'file-exists? fn)
	(begin
	  (if (arc:sys 'file-exists? dest)
	      (if force?
		  (begin
		    (arc:sys 'remove-file dest)
		    (set! really-do #t))
		  (let ((mt-fn (arc:sys 'get-mtime fn))
			(mt-dest (arc:sys 'get-mtime dest)))
		    (set! really-do (> mt-fn mt-dest))) ))
	  
	  (if really-do
	      (let ((gunzipcmd (string-append "gzip -d -c "
					      fn
					      " > "
					      dest)))
		(arc:display gunzipcmd #\nl)
		(if (not (equal? (system gunzipcmd) 0))
		    (arc:log 'error "failed to gunzip file '" fn "'")
		    #t))
	      (arc:log 'info "gunzip: file '" dest "' is newer. don't touch")))
	(arc:log 'info "gunzip: file '" fn "' does not exist")) )
  '<unspecified>)

(arc:register-task 'gunzip arc:gunzip arc:gunzip-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
