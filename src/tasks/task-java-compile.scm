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

(arc:provide 'task-java-compile)


(arc:log 'debug "loading 'java-compile' task")

;; settings for the standard javac compiler
(define %arc:java-compiler% "javac")
(define %arc:java-defs% "")
(define %arc:java-def-flags% "")

(define %arc:java-debug-flag% "-g")
(define %arc:java-depr-flag% "-deprecation")
(define %arc:java-cp-flag% "-classpath")

;; compiles a set of Java files into java class files.
;;
;; the majority of settings, e.g. which java compiler to use, which
;; default flags to specify, etc. should be set by a system
;; configuration and read in by the master arc-script (if available)
;;
;; keywords:
;;
;; :sources FILESET
;; a fileset of java sources which should be compiled
;;
;; :flags STRING-LIST
;; a list of strings which should be added as flags to the java compiler; 
;; this settings depend on the real compiler used, so should be used with
;; care, to ensure portability
;;
;; :classpath STRING-LIST
;; a list of additional entries to the classpath to use
;;
;; :debug? BOOLEAN
;; indicates whether debug information should be compiled in. (default is #f)
;;
;; :outdir STRING
;; where the compiled class files should go to.

(define arc:java-compile-keywords '((sources strlist required)
                                    (debug? boolean optional)
                                    (flags strlist optional)
                                    (classpath strlist optional)
                                    (outdir string optional)
                                    (deprecation? boolean optional)) )
(define (arc:java-compile props body)
  (let* ((outdir (arc:aval 'outdir props #f)))
    (arc:log 'debug "java compile ...")
    
    (map 
     (lambda (fn)
       (let* ( (pn (arc:string->path fn))
	       (jflags (string-append 
			"" (arc:string-list->string* (arc:aval 'flags 
                                                               props ())
                                                     " ")
			" "
			(if (arc:aval 'deprecation? props #f)
			    (string-append %arc:java-depr-flag% " ") "")
			(if outdir
			    (string-append "-d " outdir " ")
			    "")
			(if (arc:aval 'debug? props #f) 
			    (string-append %arc:java-debug-flag% " ") ""))) )

         (arc:log 'verbose "compile '" fn "'")

	 (arc:-compile-java-file fn
				 (arc:-build-java-classpath
				  (arc:aval 'classpath props ()))
				 jflags)

         ;; TODO: this doesn't work as this simple.  Javac puts it class
         ;; files somewhere in a complex directory structure.  Should we
         ;; rebuild it?, how do we determine the package structure?
         ;; (without parsing the java sourcefile and analysing the
         ;; "package" statement) -> perhaps: remove the source directory
         ;; part from the full source path, build the directory structure
         ;; from the resulting parts
         (if outdir
             (arc:path->string
              (arc:path-replace-last-ext 
               (arc:path-append (arc:string->path outdir)
                                pn) "class"))
             (arc:path->string (arc:path-replace-last-ext pn "class"))))
       )
     (arc:aval 'sources props ())) ))

(arc:register-task 'java-compile arc:java-compile arc:java-compile-keywords)


(define (arc:-compile-java-file sfile classpath jflags)
  (let ((cmd-str (string-append %arc:java-compiler% " "
                                %arc:java-defs% " "
				(if (> (string-length classpath) 0)
				    (string-append %arc:java-cp-flag% 
						   " " classpath " ")
				    "")
                                %arc:java-def-flags% " "  ; default flags
                                jflags " "                ; custom cflags
                                sfile                     ; the source file
                                )))
    (arc:display cmd-str #\nl)
    (if (not (equal? (arc:sys 'system cmd-str) 0))
        (if (not %arc:keep-going-on-errors%)
            (quit)))))

(define (arc:-build-java-classpath strlist)
  (let loop ((t "")
	     (lx strlist))
    (if (null? lx)
	t
	(loop (string-append t (car lx) (if (null? (cdr lx)) 
					    "" 
					    (arc:pathlist-sep)))
	      (cdr lx)))))



;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
