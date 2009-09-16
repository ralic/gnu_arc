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

;; find a build script.  If %arc:find-script-rec% is #t, then search the
;; current working directory recursively upwards to find the build script,
;; otherwise only look in the current working directory
(define (arc:find-script nm)
  (let ((cwd (arc:path-cwd)))
    (let loop ((cp cwd))
      (if (<= (arc:path-length cp) 0)
          #f
          (let ((nm (arc:path->string (arc:path-append cp nm))))
            (if (arc:sys 'file-exists? nm)
                nm
                (if (not %arc:find-script-rec%)
                    #f
                    (loop (arc:path-without-last-comp cp)))))))))



(define %arc:--script-in-loading--% "")

;; evaluate a script; script is the name of the file
(define (arc:evaluate-script script . at-level)
  (let ((current-stmt arc:current-stmt))
    ;; the load command load and evaluates the script
    (set! %arc:--script-in-loading--% script)
    
    (arc:load-arcfile script)
    
    (if (not current-stmt)
        (if (arc:context-current)
            (set! current-stmt 
                  (arc:context-default-stmt (arc:context-current)))))
    
    (if (not current-stmt)
        (set! current-stmt 'all))

    (arc:eval-stmt current-stmt at-level)

    (arc:deps-store-database)

    (arc:context-drop)
    ))


(define (arc:eval-stmt stmt-id . at-level)
  (if (arc:stmt-evaluated-yet? stmt-id)
      ;; if the statement is evaluated yet, look up it's value
      (arc:stmt-value (arc:find-stmt stmt-id))
      ;; otherwise evaluate it and return it's value
      (let ((stmt (arc:find-stmt stmt-id %arc:eval-os%))
	    (retval '()))
        (if (not stmt)
            (begin
              (arc:log 'error 
                       "statement '" stmt-id "' unknown "
                       "(at least on this platform). Aborted")
              ;;(arc:context-display)
              (quit))
            (if (arc:stmt-callable-from-scope? stmt 
                                               (if (not (null? at-level))
                                                   (car at-level)
                                                   'local))
                (let ((deps (arc:stmt-dependencies stmt)))
                  (cond
                   ;; no dependency known
                   ((not deps) (set! retval (arc:eval-and-register-stmt stmt)))
                   
                   ;; a list of dependencies is pre-evaluated in given order
                   ((list? deps) 
                    (let dloop ((dp deps))
                      (if (null? dp)
                          (set! retval (arc:eval-and-register-stmt stmt))
                          (begin
                            (set! retval (arc:eval-stmt (car dp)))
                            (dloop (cdr dp))))))
                   ;; a single dependecy
                   ((symbol? deps)
                    (arc:eval-stmt deps)
                    (set! retval (arc:eval-and-register-stmt stmt)))) )
                (arc:log 'error 
                         "statement " stmt-id 
                         " not visable from given level") ))
	retval)) )

(define (arc:stmt-callable-from-scope? stmt at-level)
  (case (arc:stmt-scope stmt)
    ((public) #t)
    ((script) (case at-level
                ((public) #f)
                ((script) #t)
                ((local) #t)
                (else #f)))
    ((local) (case at-level
               ((public script) #f)
               ((local) #t)
               (else #f)))
    (else #f)))


;; shorthand for eval-stmt
(define ($ id)
  (arc:eval-stmt id))

(define (arc:eval-and-register-stmt stmt)
  (arc:log 'verbose "eval '" (arc:stmt-display-name stmt) "' "
           (let ((info (arc:stmt-info stmt)))
             (if info 
                 (string-append "(" info ")")
                 "")))
  (let* ((body (arc:stmt-body stmt))
	 (once? (arc:stmt-once stmt))
	 (val (if body
                  (if (list? body)
                      (let* ((retv (map (lambda (x)
                                          (arc:eval-arc x))
                                        body))
                             (ll (length retv)))
                        (if (> ll 0)
                            (list-ref retv (- ll 1))
                            #f))
                      (arc:eval-arc body))
		  #f)))
    (if once?
	(begin
	  (arc:stmt-evaluated! (arc:stmt-id stmt))
	  (arc:stmt-value! stmt val)))
    val))


;; ----------------------------------------------------------------------
;; the (stmt) dependency machinery
;; ----------------------------------------------------------------------
;; this is a member list of all stmt ids yet evaluated
(define %arc:stmts-yet-evaluated% '())

(define (arc:stmt-evaluated-yet? id)
  (if (member id %arc:stmts-yet-evaluated%) #t #f))

(define (arc:stmt-evaluated! id)
  (set! %arc:stmts-yet-evaluated% (cons id %arc:stmts-yet-evaluated%)))



;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
