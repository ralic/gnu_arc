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

;; $Id: ctx.scm,v 1.5 2009/03/14 23:40:18 eyestep Exp $


;; each build script is evaluated in a given context.  the contexts
;; are stacked (for scripts loaded from scripts).  a context lists all
;; statements, properties and dependencies from the build script
;; loaded



;; ----------------------------------------------------------------------
;; contexts
;;
;; the context data structure is as follows:
;; #(:context (<stmt alist>) 
;;            (<meta info alist>)
;;            <id>)
;; ----------------------------------------------------------------------
;; the current context root; create the default context
(define %arc:contexts% ())

(define %ARC:STMT-SLOT% 1)
(define %ARC:META-SLOT% 2)
(define %ARC:ID-SLOT% 3)

(define %ARC:STMT-DEPS-SLOT% 0)		; the dependency list
(define %ARC:STMT-VAL-SLOT% 1)		; to take the return value
(define %ARC:STMT-META-SLOT% 2)		; meta info slot
(define %ARC:STMT-BODY-SLOT% 3)		; the body

;; ----------------------------------------------------------------------
;; creating and stacking contexts
;; ----------------------------------------------------------------------
(define (arc:make-context id)
  (let ((ctx (make-vector 4 ())))
    (vector-set! ctx 0 ':context)
    (vector-set! ctx %ARC:STMT-SLOT% ()) ; the properties (alist of property)
    (vector-set! ctx %ARC:META-SLOT% ()) ; meta info
    (vector-set! ctx %ARC:ID-SLOT% id)   ; the id
    ctx))

(define (arc:context? ctx)
  (and (vector? ctx)
       (>= (vector-length ctx) 4)
       (equal? (vector-ref ctx 0) ':context)))

(define (arc:context-push ctx)
  (if (arc:context? ctx)
      (set! %arc:contexts% (cons ctx %arc:contexts%))
      (arc:msg "Try to push a non-context object.  Ignored")))

(define (arc:context-drop)
  (if (not (null? %arc:contexts%))
      (let ((old-dir (arc:-ctx-meta-info (car %arc:contexts%) 'old-cwd)))
        (if old-dir
            (arc:sys 'chdir old-dir))
        (set! %arc:contexts% (cdr  %arc:contexts%)))
      (begin 
        (arc:log 'error "no context to drop. bad stacking")
        (quit))))

(define (arc:context-current)
  (if (null? %arc:contexts%)
      #f
      (car %arc:contexts%)))


;; ----------------------------------------------------------------------
;; setting the properties of a context
;; ----------------------------------------------------------------------
;; set the context information
(define (arc:-ctx-meta-info! ctx key val)
  (let ((val-assoc (assoc key (vector-ref ctx %ARC:META-SLOT%))))
    (if val-assoc
        (set-cdr! val-assoc (list val))
        (vector-set! ctx %ARC:META-SLOT%
                     (append (vector-ref ctx %ARC:META-SLOT%)
                             (list (list key val)))))))

(define (arc:-ctx-meta-info ctx key)
  (let ((val-assoc (assoc key (vector-ref ctx %ARC:META-SLOT%))))
    (if val-assoc
        (cadr val-assoc)
        #f)))

;; sets the basedir property of the specified context.  The base directory
;; is absolutized, i.e. is is calculated against the current working
;; directory, unless it is absolute
(define (arc:context-basedir! ctx bd)
  (if (arc:context? ctx)
      (begin
        (if (not (arc:-ctx-meta-info ctx 'old-cwd))
            (arc:-ctx-meta-info! ctx 'old-cwd (arc:sys 'getcwd)))
        (let* ((bdp (arc:string->path bd))
               (tp (if (arc:path-absolute? bdp)
                       bdp
                       (arc:path-append (arc:path-cwd) bdp))) 
               (tps (arc:path->string (arc:path-normalize tp))))
          (arc:-ctx-meta-info! ctx 'basedir tps)
          (if (not (arc:sys 'chdir tps))
              (begin
                (arc:msg "failed to change to directory '" tps "'.  Aborted")
                (quit)))))))

;; returns the basedir property of the specified context or #f if not given
(define (arc:context-basedir ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'basedir)))

;; sets the default-statement property of the specified context
(define (arc:context-default-stmt! ctx target)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info! ctx 'default-stmt target)))

;; returns the default-statement property of the specified context or #f if
;; not given
(define (arc:context-default-stmt ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'default-stmt)))

;; sets the info property of the specified context
(define (arc:context-info! ctx info)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info! ctx 'info info)))

;; returns the info property of the specified context or #f if not given
(define (arc:context-info ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'info)))

;; sets the name property of the specified context
(define (arc:context-project! ctx pn)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info! ctx 'project pn)))

;; returns the name property of the specified context or #f if not given
(define (arc:context-project ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'project)))

;; sets the version property of the specified context
(define (arc:context-version! ctx pn)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info! ctx 'version pn)))

;; returns the version property of the specified context or #f if not given
(define (arc:context-version ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'version)))

;; returns the id of the specified context (a symbol)
(define (arc:context-id ctx)
  (if (arc:context? ctx)
      (vector-ref ctx %ARC:ID-SLOT%)))

;; sets the home directory of the specified context
(define (arc:context-script-home! ctx pn)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info! ctx 'script-home pn)))

;; returns the home directory of the specified context or #f if not given
(define (arc:context-script-home ctx)
  (if (arc:context? ctx)
      (arc:-ctx-meta-info ctx 'script-home)))

(define (arc:context-os-match stmt os)
  (if stmt
      (let* ((oss (arc:stmt-os stmt)))
        (cond
         ( (list? oss)   (list? (memq os oss)) )
         ( (symbol? oss) (or (equal? oss 'all)
                             (equal? oss os)) )
         ( else #f )))
      #f))
  
(define (arc:context-stmt-redefined? stmt os)
  (arc:context-os-match stmt os))

(define (arc:context-stmt! ctx name deps props cmd)
  (if (arc:context? ctx)
      (let ((val-assoc (assoc name (vector-ref ctx %ARC:STMT-SLOT%))))
        (if (arc:context-stmt-redefined? val-assoc 
                                         (or (member 'os props) 'all))
            (arc:display "Statement '" name "' redefined.  Ignored" #\nl)
	    (vector-set! ctx %ARC:STMT-SLOT%
			 (append (vector-ref ctx %ARC:STMT-SLOT%)
				 (list (let ((s (arc:make-stmt name)))
					 (arc:stmt-dependencies! s deps)
					 (arc:stmt-properties! s props)
					 (arc:stmt-body! s cmd)
					 s))) )))))

;; returns the complete statement assoc-frame
(define (arc:context-stmt ctx name . os)
  (if (arc:context? ctx)
      (let ((osnm (if (and os
                           (not (null? os)))
                      (car os)
                      #f)))
        (if osnm
            (let loop ((x (vector-ref ctx %ARC:STMT-SLOT%)))
              (if (null? x)
                  #f
                  (if (and (equal? (arc:stmt-id (car x)) name)
                           (arc:context-os-match (car x) osnm) )
                      (car x)
                      (loop (cdr x)))))
            (assoc name (vector-ref ctx %ARC:STMT-SLOT%)) ))
      #f))

(define (arc:make-stmt id)
  (let ((s (cons id (make-vector 4))))
    (vector-set! (cdr s) %ARC:STMT-DEPS-SLOT% ())
    (vector-set! (cdr s) %ARC:STMT-VAL-SLOT% ())
    (vector-set! (cdr s) %ARC:STMT-META-SLOT% ())
    (vector-set! (cdr s) %ARC:STMT-BODY-SLOT% #f)
    s))


;; short cut for (arc:context-* (arc:context-current)).  This returns
;; properties of the current context (e.g. the version or project name).
(define (arc:current-context-property id)
  (let ((ctx (arc:context-current)))
    (if (arc:context? ctx)
        (arc:-ctx-meta-info ctx id)
        #f)))

;; sets the ** property of the current context
(define (arc:current-context-property! id val)
  (let ((ctx (arc:context-current)))
    (if (arc:context? ctx)
        (arc:-ctx-meta-info! ctx id val))))


;; ----------------------------------------------------------------------
;; projects
;; ----------------------------------------------------------------------

;; looks up a statement in the current context.  This function takes the
;; complete stack of contexts into account, i.e. if a requested statement
;; has not been found in the current context, it is looked up in the parent
;; context and so on.  If no property is found at all, it returns #f.  If
;; by-os is set to a symbolic os-name, the first statement with id 'id' is
;; used which matches the os.
(define (arc:find-stmt id . by-os)
  (let* ((osnm (if (not (null? by-os))
                   (car by-os)
                   #f)))
    (let loop ((ctx %arc:contexts%))
      (if (null? ctx)
          #f
          (or (arc:context-stmt (car ctx) id osnm)
              (loop (cdr ctx)))))))
  
;; returns the statement's id
(define (arc:stmt-id stmt)
  (if (pair? stmt)
      (car stmt)
      #f))

;; returns the body of a (looked up, e.g. by arc:find-stmt) statement
(define (arc:stmt-body stmt)
  (vector-ref (cdr stmt) %ARC:STMT-BODY-SLOT%))

(define (arc:stmt-body! stmt cmd)
  (if (pair? stmt)
      (vector-set! (cdr stmt) %ARC:STMT-BODY-SLOT% cmd)))

;; returns the dependencies of a (looked up, e.g. by arc:find-stmt)
;; statement.  if no dependecy list was specified for the statement,
;; returns #f
(define (arc:stmt-dependencies stmt)
  (if (pair? stmt)
      (vector-ref (cdr stmt) %ARC:STMT-DEPS-SLOT%)
      #f))

(define (arc:stmt-dependencies! stmt deps)
  (if (pair? stmt)
      (vector-set! (cdr stmt) %ARC:STMT-DEPS-SLOT% deps)))

(define (arc:-stmt-meta-info stmt key default)
  (if (pair? stmt)
      (let* ((mi (vector-ref (cdr stmt) %ARC:STMT-META-SLOT%))
	     (d (if (list? mi)
		    (member key mi)
		    #f)))
        (if d
            (cadr d)
            default))
      default))

;; returns the info of a (looked up, e.g. by arc:find-stmt) statement,
;; if no info was specified for the statement, returns #f
(define (arc:stmt-info stmt)
  (arc:-stmt-meta-info stmt 'info #f))

;; returns the os-condition of a (looked up, e.g. by arc:find-stmt)
;; statement, if no os-condition was specified for the statement,
;; returns 'all
(define (arc:stmt-os stmt)
  (arc:-stmt-meta-info stmt 'os 'all))

;; returns the once? property of a (looked up, e.g. by arc:find-stmt)
;; statement, if no once? property was specified for the statement,
;; returns #t
(define (arc:stmt-once stmt)
  (arc:-stmt-meta-info stmt 'once? #t))

;; returns the scope-property-condition of a (looked up, e.g. by
;; arc:find-stmt) statement, if no scope property was specified for the
;; statement, returns 'public
(define (arc:stmt-scope stmt)
  (arc:-stmt-meta-info stmt 'scope 'public))


(define (arc:stmt-properties! stmt props)
  (if (pair? stmt)
      (vector-set! (cdr stmt) %ARC:STMT-META-SLOT% props)))

;; returns the evaluation value of a statement.  This function does
;; not start an evaluation, if the statement has not been evaluated
;; yet!
(define (arc:stmt-value stmt)
  (if (pair? stmt)
      (vector-ref (cdr stmt) %ARC:STMT-VAL-SLOT%)
      #f))

;; sets the evaluation value of a statement
(define (arc:stmt-value! stmt val)
  (if (pair? stmt)
      (vector-set! (cdr stmt) %ARC:STMT-VAL-SLOT% val)))


;; returns an alist of all statements visable at a given context stack
;; situation (statements from sub-loaded scripts override statements
;; from parent scripts)
(define (arc:context-all-stmt ctx)
  (let loop ((res ())
             (mq %arc:contexts%))
    (if (null? mq)
        res
        (loop (let loop2 ((rx res)
                          (ta (vector-ref (car mq) %ARC:STMT-SLOT%)))
                (if (null? ta)
                    rx
                    (loop2 (let ((val-assoc (assoc (caar ta) rx)))
                             (if val-assoc
                                 rx
                                 (append rx (list (car ta)))))
                           (cdr ta))))
              (cdr mq)))))


;; ----------------------------------------------------------------------
;; tests
;; ----------------------------------------------------------------------
;; displays the complete stack of contexts
(define (arc:context-display)
  (let loop ((mq %arc:contexts%))
    (if (null? mq)
        #t
        (begin
          (if (arc:context? (car mq))
              (begin
                (arc:display "Name:  " 
                             (vector-ref (car mq) %ARC:ID-SLOT%) #\nl)
                (arc:display "-- Statements -----------: " #\nl)
                (arc:context-display-stmts
                 (vector-ref (car mq) %ARC:STMT-SLOT%)) 
                (arc:display "-- Flags -------------: " #\nl
                             (vector-ref (car mq) %ARC:META-SLOT%) #\nl)
                )
              (arc:display "??" #\nl))
          (loop (cdr mq))))))

(define (arc:context-display-stmts stmt-alist)
  (let loop ((ta stmt-alist))
    (if (null? ta)
        #t
        (begin
          (arc:display (caar ta) (arc:stmt-info (car ta)) #\nl)
          (loop (cdr ta))))))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
