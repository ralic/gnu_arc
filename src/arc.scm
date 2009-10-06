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

;; setup the program arguments
(define %arc:argv% (vector->list *args*))

;; setup the require/provide system
(define %arc:debug% (let ((ff (sys:getenv "ARC_DEBUG")))
                      (if (and ff (equal? ff "yes")) #t #f)))

;; define global variables and flags
(define %arc:verbose% #f)

(define arc:script-name #f)
(define arc:config-name #f)
(define arc:current-stmt #f)
(define %arc:find-script-rec% #f)
(define %arc:start-dir% '())
(define %arc:keep-going-on-errors% #f)
(define %arc:show-infos-only% #f)

(define arc:default-script-names '("ARCFile" "Arcfile" "ArcFile" "arcfile"
				   "ARCFILE" "build.arc" "Build.arc" 
				   "build.ARC" "Build.ARC" "BUILD.ARC"))
(define arc:default-config-names '("arc.config"
                                   "ARCConfig" "Arcconfig" "ArcConfig" 
                                   "arcconfig" "ARCCONFIG" "Config.arc" 
                                   "config.arc" "Arc.rc" "arc.rc"))

(load (string-append (arc:home) "/config.scm"))
(load (string-append (arc:home) "/misc.scm"))
(load (string-append (arc:home) "/sysnm.scm"))
(load (string-append (arc:home) "/strings.scm"))
(load (string-append (arc:home) "/logical.scm"))
(load (string-append (arc:home) "/excp.scm"))
(load (string-append (arc:home) "/path.scm"))

(define %arc:sysnm% (arc:canonical-sysnm (arc:host-os)
                                         (arc:host-maker)
                                         (arc:host-cpu)
                                         (arc:host-version)))

(load (string-append (arc:home) "/oop.scm"))
(load (string-append (arc:home) "/sys.scm"))



(if (sys:file-exists? (string-append (arc:home) "/version.scm"))
    (load (string-append (arc:home) "/version.scm"))
    (define %arc:version% "?"))


;; this variable defines the os as used for the evaluation machine.  This
;; does not need to be identical to the real system name
(define %arc:eval-os% (car %arc:sysnm%))

;; ----------------------------------------------------------------------
;; set up the include path
;; ----------------------------------------------------------------------
(define %arc:arc-incl-path% 
  (let ((ip (or (sys:getenv "ARC_INCL_PATH")
                (arc:include-path))))
    (if ip
        (append (append (arc:split-string ip (arc:pathlist-sep))
                        (list (arc:home)))
                (list (string-append (arc:home) "/tasks")))
        (append (list (arc:home))
                (list (string-append (arc:home) "/tasks"))))))


;; ----------------------------------------------------------------------
;; set up the system environment
;; ----------------------------------------------------------------------
(if %arc:debug%
    (arc:display "system name:        " %arc:sysnm% 'nl
                 "arc home:           " (arc:home) 'nl))

(load (string-append (arc:home) "/require.scm"))
(load (string-append (arc:home) "/getopt.scm"))
(load (string-append (arc:home) "/misc.scm"))
(load (string-append (arc:home) "/path.scm"))
(load (string-append (arc:home) "/hash.scm"))
(load (string-append (arc:home) "/eval.scm"))
(load (string-append (arc:home) "/arcconf.scm"))
(load (string-append (arc:home) "/show.scm"))
(load (string-append (arc:home) "/implicit.scm"))
(load (string-append (arc:home) "/ctx.scm"))
(load (string-append (arc:home) "/meta.scm"))
(load (string-append (arc:home) "/deps.scm"))
(load (string-append (arc:home) "/task-util.scm"))
(load (string-append (arc:home) "/traverse.scm"))
(load (string-append (arc:home) "/attrval.scm"))
(load (string-append (arc:home) "/filter.scm"))
(load (string-append (arc:home) "/pregexp.scm"))
(load (string-append (arc:home) "/fnmatch.scm"))
(load (string-append (arc:home) "/handler-factory.scm"))
(load (string-append (arc:home) "/bt-tasks.scm"))
  

;; display a help text
(define (arc:display-help)
  (arc:display
   "arc - a scheme based makefile and config system" 'nl
   "usage:" 'nl
   "  arc [options] [statement]" 'nl
   'nl
   "Options:" 'nl
   " -f FILE   Load FILE as build script instead of 'Arcfile' as default" 'nl
   " -C DIR    Change to directory DIR before reading a build script or doing" 'nl
   "           anything else" 'nl
   " -r        By default arc looks for the build script 'build.arc' in the" 'nl
   "           current working directory.  If the -r options is specified it" 'nl
   "           tries to search for a build script recursively upwards from the" 'nl
   "           current working directory an uses the first found." 'nl
   " -k        Continue as much as possible after an error; continue if " 'nl
   "           statement failed" 'nl
   " -v        be verbose" 'nl
   " -o OS     assume the operating system OS during evaluation.  OS is" 'nl
   "           set by default to the symbolic name of the current machine," 'nl
   "           but using this switch one can force the selection of targets" 'nl
   "           with special os: properties" 'nl
   " -i        show information about the callable statements in the" 'nl
   "           script to be loaded and exit" 'nl
   " -h        This help" 'nl
   " -V        Print the version and exit" 'nl
   'nl 
   "if no statement is given on the command line arc uses the default statement " 'nl
   "as stated in the defproject statement in the build script used." 'nl  
   'nl 'nl))

(define (arc:display-version)
  (arc:display
   "Arc version " %arc:version% 'nl
   "Copyright (C) 2002, 2003 Gregor Klinke" 'nl
   "This is free software; see the source for copying conditions." 'nl
   "There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A" 'nl
   "PARTICULAR PURPOSE." 'nl))

(define arc:opts '((verbose   "-v" "--verbose" #f)
                   (script    "-f" "--script" #t)
                   (conf      "-c" "--conf" #t)
                   (recursive "-r" "--rec" #f)
                   (keep      "-k" "--keep-going" #f)
                   (change    "-C" "--dir" #t)
                   (help      "-h" "--help" #f)
                   (info      "-i" "--info" #f)
                   (os        "-o" "--os" #t)
                   (version   "-V" "--version" #f)))
(let loop ((opt (arc:getopt %arc:argv% arc:opts)))
  (if (not opt)
      #f
      (let ((done #f))
        (display opt)(newline)
        (case opt
          ((verbose) (set! %arc:verbose% #t))
          ((conf) (set! arc:config-name *arc:optarg*))
          ((script) (set! arc:script-name *arc:optarg*))
          ((recursive) (set! %arc:find-script-rec% #t))
          ((keep) (set! %arc:keep-going-on-errors% #t))
          ((os) (set! %arc:eval-os% (string->symbol *arc:optarg*)))
          ((change) (begin
                      ;; store the current directory
                      (set! %arc:start-dir% (arc:path-cwd))
                      ;; change to new working directory
                      (sys:change-dir *arc:optarg*)))
          ((help) (begin
                    (arc:display-help)
                    (quit 0)))
          ((version) (begin
                       (arc:display-version)
                       (quit 0)))
          ((info) (set! %arc:show-infos-only% #t))
          ((#\?) (arc:msg "bad option: " *arc:optopt*))
          ((#\:) (begin
                   (arc:msg "missing arg " *arc:optopt*)))
          (else (begin 
                  (set! arc:current-stmt (string->symbol opt))
                  (set! done #t))))
        (if (not done)
            (loop (arc:getopt %arc:argv% arc:opts)))) ))


(if %arc:verbose%
    (begin
      (arc:display "arc system version " %arc:version% 'nl)
      (arc:display "assume os: " %arc:eval-os% 'nl)))


;; start the scripting logic
;; [1] look for a arcconfig file to load
(let ((script (if arc:config-name 
		  arc:config-name
		  (let loop ((nl arc:default-config-names))
		    (if (null? nl)
			#f
			(or (arc:find-script (car nl))
			    (loop (cdr nl))))))))
  (if %arc:verbose%
      (arc:msg "use config file: " script))
  (if script
      (arc:load-arcconfig script)))


;; [2] look for the Arcfile and go ...
(let ((script (if arc:script-name 
		  arc:script-name
		  (let loop ((nl arc:default-script-names))
		    (if (null? nl)
			#f
			(or (arc:find-script (car nl))
			    (loop (cdr nl))))))))
  (if %arc:verbose%
      (arc:msg "use script: " script))
  (if script
      (if %arc:show-infos-only%
          (arc:display-script script)
          (arc:evaluate-script script 'public))
      (arc:msg "no script file found")))


;; finish
(quit 0)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
