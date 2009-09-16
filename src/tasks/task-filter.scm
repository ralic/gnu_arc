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

(arc:provide 'task-filter)


(arc:log 'debug "loading 'filter' task")

;; Filters a files from a source to a destination by replacing placeholders
;; with values from a alist.
;;
;; Filters an input file to an output file.  In-file and out-file must not
;; be equal.  Words surrounded by @ are recognized as keys and looked up in
;; a alist.  The alist entries cadr is printed out, when such a key is
;; found.  An empty keyword (@@) is printed as single @ to the output
;; stream.  If a keyword is not found in the alist nothing is printed to
;; the outstream and an warning message is given, unless the :quiet?
;; property is set to #t.
;;
;; Keywords:
;; :src STRING
;; the path of the input file.  required.  The file is not changed.
;;
;; :dest STRING
;; the destination file.  If the destination file is not given and the
;; input files is named "xxx.in" the destination file is named "xxx" (in
;; the same directory); otherwise the task does nothing.
;;
;; :force? BOOLEAN
;; overwrite an yet existing destination file (default is #t).
;;
;; :seperator CHAR
;; the seperator to instead of @
;;
;; :values ALIST
;; gives the alist of key-values pairs containing the keys words to replace
;; in the source files.  required

;; RETURNS
;; The name of the destination file.
(define arc:filter-keywords '((src       string required)
                              (dest      string optional)
                              (force?    boolean optional)
                              (separator char optional)
                              (values    alist required)))

(define (arc:filter props body)
  (let* ((srcnm (arc:aval 'src props #f))
         (sepc (arc:aval 'separator props #\@))
         (destnm (or (arc:aval 'dest props #f)
                     (let* ((s (arc:string->path srcnm))
                            (lpc (arc:path-ext s)))
                       (if (equal? lpc "in")
                           (arc:path->string 
                            (arc:path-without-last-ext s))
                           (begin
                             (arc:log 'info "filter: no destination file given")
                             #f)))))
         (force (arc:aval 'force? props #t))
         (values (arc:aval 'values props '())) 
         (really-do #f)
         (retv #f))
    
    (arc:log 'verbose "filter " srcnm " -> " destnm)
    
    (if destnm
        (begin
          (if (arc:sys 'file-exists? destnm)
              (if force
                  (begin
                    (arc:sys 'remove-file destnm)
                    (set! really-do #t)))
              (set! really-do #t))
          
          (if really-do
              (begin 
                (arc:log 'debug "filter '" srcnm "' to '" destnm "'")
                (arc:filter-file* srcnm destnm values sepc)
                (set! retv destnm)))))
    
    retv))

(arc:register-task 'filter arc:filter arc:filter-keywords)

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
