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

;; $Id: meta.scm,v 1.4 2003/04/19 01:08:37 eyestep Exp $


(define (arc:eval-arc-defines expr)
  (cond 
   ((list? expr) (case (car expr)
                   ((project) (arc:--apply-defproject expr))
                   ((stmt) (arc:--apply-defstmt expr))
                   (else (arc:msg "unknown toplevel expr: " expr))))
   (else (arc:msg "unknown toplevel expr: " expr))))

(define (arc:load-arcfile nm)
  (let* ((*in* (open-input-file nm)))
    
    (do ((expr (read *in*) (read *in*)))
        ((eof-object? expr) #t)
      (arc:eval-arc-defines expr))
    (close-input-port *in*)
    #t))
         

;; --------------------------------------------------------------------
;; evaluate the top level statements
;; --------------------------------------------------------------------
;; defines a new project and creates a prepare context object.  The
;; context object is set automatically as the current objet.  The list
;; of keys is used to define the necessary meta information.  The
;; following keys are identified
;;
;; :info STRING
;;   sets an informational description to this project
;;
;; :project STRING
;;   sets the name of the project 
;;
;; :basedir STRING
;;   sets the base directory of the current build script.  by default the
;;   the directory in which the build script has been found is taken as
;;   base directory
;;
;; :default SYMBOL
;;   sets the default statement of the project.  by default the default
;;   statement is 'all
;;
(define (arc:--apply-defproject expr)
  (let* ((id (cadr expr))
         (keys (arc:get-keywords (cddr expr)))
         (props* (arc:compile-key-list '((info string optional)
                                         (project string optional)
                                         (version string optional)
                                         (basedir string optional)
                                         (default symbol optional))
                                       keys
                                       "project"))
         (ctx (arc:make-context id)))
    ;; the --script-in-loading-- variable is set by the eval-script!
    (arc:context-script-home! ctx 
                              (arc:path->string 
                               (arc:path-without-last-comp 
                                (arc:string->path 
                                 %arc:--script-in-loading--%))))
    (for-each (lambda (prop)
                (case (car prop)
                  ((info) (arc:context-info! ctx (cadr prop)))
                  ((project) (arc:context-project! ctx (cadr prop)))
                  ((version) (arc:context-version! ctx (cadr prop)))
                  ((basedir) (arc:context-basedir! ctx (cadr prop)))
                  ((default) (arc:context-default-stmt! ctx 
                                                        (cadr prop)))) )
              props*)
    (arc:context-push ctx)
    ctx))


;; ----------------------------------------------------------------------
;; statements
;;
;; the statement data structure is as follows:
;;
;; (id . #(<dependency list> <value> (<meta info member list>) lambda))
;;
;; use arc:make-stmt to generate a statement
;; ----------------------------------------------------------------------

;; defines a statements for a given id.  Statements are connected by
;; dependencies, either implicit dependency, which are computed by
;; reference to statement's returnvalues, or by explicit dependencies
;; given in the :depends property.  The following paramters are
;; requested:
;;
;; (defstmt ID [':depends EXPLICIT-DEP-LIST] [KEYS] LAMBDA-EXPR)
;;
;; ID: must be symbol given a unique statement (unique in the current
;; context).
;;
;; EXPLICIT-DEP-LIST: is a list of statement symbols.  If only one
;; dependend statement is to be listed, this can be a symbol, too.
;; E.g.: ':depends '(init prep) or ':depends 'compile
;;
;; KEYS: could be any number of key-value pairs.  The following keys are
;; recognized:
;;
;; :info STRING
;; sets an informational description to this statement
;; 
;; :os SYMBOL
;; this statement will only be evaluated if the symbol matches the current
;; operating system; defaults to 'all
;;
;; :once? BOOLEAN
;; if #t the statement is evaluated only once and its return value is cached
;; for later (and repeated) use.  If #f the statement is evaluated every time
;; its dependency is tracked or its returnvalue is requested.  The
;; default is #t (once only)
;;
;; scope SYMBOL
;; defines the visability of this statement, i.e. if the statement is
;; usable as a direct target from outside.  There are three different
;; levels 'local (the statement may only be 'used' from the current
;; Arcfile), 'script (the statement may only be 'used' from the current
;; Arcfile or any Arcfiles called from it), 'public (the statement is a
;; entry point from the outside).
;;
;; LIST: the statement's expression body.  This code list is executed when
;; a statement is evaluated.  This is required and must at least be a #f.
;; The returnvalue of this expression-list is the evaluation value of the
;; statement, and may be requested in other statements using the [id]
;; expression (arc:eval-stmt).

(define (arc:--apply-defstmt expr)
  (let* ((id (cadr expr))
         (keys-body (arc:get-keywords-and-body (cddr expr)))
         (props* (arc:compile-key-list '((info string optional)
                                         (os symbol optional)
                                         (once? boolean optional)
                                         (scope symbol optional)
                                         (depends (symbol list) optional))
                                       (car keys-body)
                                       (symbol->string id)))
         (deps ())
         (bad #f)
         (scope (case (arc:aval 'scope props* 'public)
                  ((local) 'local)
                  ((script) 'script)
                  ((public) 'public)
                  (else (begin
                          (arc:log 'error 
                                   "bad symbol used for scope in "
                                   "statement '" id "'")
                          'public))))
         (props ()))
    (for-each (lambda (prop)
                (case (car prop)
                  ((info) (set! props (append props (list 'info
                                                          (cadr prop)))))
                  ((os) (set! props (append props (list 'os
                                                        (cadr prop)))))
                  ((once?) (set! props (append props (list 'once?
                                                           (cadr prop)))))
                  ((scope) (set! props (append props (list 'scope
                                                           scope))))
                  ((depends) (set! deps (cadr prop)))))
              props*)
    
    (arc:context-stmt! (arc:context-current) id deps props (cdr keys-body))
    ))




;; --------------------------------------------------------------------
;; evaluation of the meta scheme
;; --------------------------------------------------------------------
(define %arc:frames% ())
(define (arc:make-frame)
  (cons ':frame ()))

(define (arc:bind-var-to-frame frame name value)
  (set-cdr! frame (append (cdr frame) (list (cons name value)))))

;; looks up a var slot by recursively walking up the frames.  it returns
;; the complete cons with (name . value) or #f if no var was found
(define (arc:frame-lookup name)
  (let loop ((fr %arc:frames%))
    (if (null? fr)
        #f
        (or (assoc name (cdar fr))
            (loop (cdr fr))))))

(define (arc:frame-assign! frame name value)
  (let ((var (assoc name (cdr frame))))
    (if var
        (set-cdr! var value)
        (arc:bind-var-to-frame frame name value))))


;; this is the main evaluation function for the meta-scheme code.  This
;; seems to be totally useless ("we're evaluation scheme code inside a
;; scheme interpreter"), but it makes it possible to plug in trasparently
;; tasks in the function name space, use special syntaxes (without macros)
;; -- and last but not least limit and prescribe the available functions
(define (arc:eval-arc expr)
  (cond 
   ((null? expr) ())
   ((list? expr) 
    (case (car expr)
      ((quote) (cadr expr))

      ;; computational
      ((+) (apply + (arc:eval-list (cdr expr))))
      ((-) (apply - (arc:eval-list (cdr expr))))
      ((*) (apply * (arc:eval-list (cdr expr))))
      ((/) (apply / (arc:eval-list (cdr expr))))
      ((modulo) (apply modulo (arc:eval-list (cdr expr))))
      ((exp*) (apply * (arc:make-list (arc:eval-arc (caddr expr))
                                      (arc:eval-arc (cadr expr)))))
      ((>) (apply > (arc:eval-list (cdr expr))))
      ((<) (apply < (arc:eval-list (cdr expr))))
      ((<=) (apply <= (arc:eval-list (cdr expr))))
      ((>=) (apply >= (arc:eval-list (cdr expr))))
      ((=) (apply = (arc:eval-list (cdr expr))))
      ((not) (apply not (arc:eval-list (cdr expr))))
      ((null?) (apply null? (arc:eval-list (cdr expr))))
      ((equal?) (apply equal? (arc:eval-list (cdr expr))))
      
      ;; conditional and logical
      ((if) (if (arc:eval-arc (cadr expr))
                (if (not (null? (cddr expr)))
                    (arc:eval-arc (caddr expr))
                    #f)
                (if (not (null? (cdddr expr)))
                    (arc:eval-arc (cadddr expr))
                    #f)))
      ((or) (let loop ((x (cdr expr)))
              (if (null? x)
                  #f
                  (or (arc:eval-arc (car x))
                      (loop (cdr x))))))
      ((and) (let loop ((x (cdr expr))
                        (lv #t))
               (if (null? x)
                   lv
                   (if (not (arc:eval-arc (car x)))
                       #f
                       (loop (cdr x) (car x))))))

      ;; @todo case, cond

      ;; binding syntax
      ((let*) (arc:eval-let* expr))
      ((let) (if (symbol? (cadr expr))
                 (arc:eval-named-let (cadr expr) (cddr expr))
                 (arc:eval-let expr)))
      ((begin) (let loop ((x (cdr expr))
                          (lv #f))
                 (if (null? x)
                     lv
                     (loop (cdr x)
                           (arc:eval-arc (car x))))))

      ;; predicates
      ((list?) (apply list? (arc:eval-list (cdr expr))))
      ((symbol?) (apply symbol? (arc:eval-list (cdr expr))))
      ((pair?) (apply pair? (arc:eval-list (cdr expr))))
      ((integer?) (apply integer? (arc:eval-list (cdr expr))))
      ((number?) (apply number? (arc:eval-list (cdr expr))))
      ((string?) (apply string? (arc:eval-list (cdr expr))))
      ((procedure?) (apply procedure? (arc:eval-list (cdr expr))))
      ((boolean?) (apply boolean? (arc:eval-list (cdr expr))))
      ((string-list?) (apply arc:string-list? (arc:eval-list (cdr expr))))
      ((alist?) (apply arc:alist? (arc:eval-list (cdr expr))))

      ;; string functions
      ((string-append) (apply arc:to-str (arc:eval-list (cdr expr))))
      ((string-split) (apply arc:split-string (arc:eval-list (cdr expr))))
      ((string-suffix?) (apply arc:string-suffix? (arc:eval-list (cdr expr))))
      ((string-prefix?) (apply arc:string-prefix? (arc:eval-list (cdr expr))))
      ((string-length) (apply arc:string-length (arc:eval-list (cdr expr))))
      ((string->symbol) (apply string->symbol (arc:eval-list (cdr expr))))
      ((symbol->string) (apply symbol->string (arc:eval-list (cdr expr))))

      ;; path functions (different to the functions in path.scm, these meta
      ;; functions work directly on strings)
      ((path-length) 
       (arc:path-length (arc:string->path (arc:eval-arc (cadr expr)))))
      ((path->list) 
       (arc:path->list (arc:string->path (arc:eval-arc (cadr expr)))))
      ((list->path)
       (arc:path->string (arc:eval-arc (cadr expr))))
      ((path-without-last-ext)
       (arc:path->string
        (arc:path-without-last-ext (arc:string->path (arc:eval-arc (cadr expr))))))
      ((path-without-last-comp)
       (arc:path->string
        (arc:path-without-last-comp (arc:string->path (arc:eval-arc (cadr expr))))))
      ((path-append)
       (arc:path->string
        (arc:path-append (arc:string->path (arc:eval-arc (cadr expr)))
                         (arc:eval-arc (caddr expr)))))
      ((path-append-ext)
       (arc:path->string
        (arc:path-append-ext (arc:string->path (arc:eval-arc (cadr expr)))
                             (arc:eval-arc (caddr expr)))))
      ((path-replace-last-ext)
       (arc:path->string
        (arc:path-replace-last-ext (arc:string->path (arc:eval-arc 
                                                      (cadr expr)))
                                    (arc:eval-arc (caddr expr)))))
      
      ((path-last-comp)
       (arc:path->string
        (arc:path-last-comp (arc:string->path (arc:eval-arc (cadr expr))))))
      ((path-ext)
       (arc:path-ext (arc:string->path (arc:eval-arc (cadr expr)))))

      ((path-cwd) (arc 'sys.getcwd))
      ((path-homedir) (arc:sys 'homedir))
      ((path-absolute?) 
       (arc:path-absolute? (arc:string->path (arc:eval-arc (cadr expr)))))
      ((path-begins-with?)
       (arc:path-begins-with? (arc:string->path (arc:eval-arc (cadr expr)))
                              (arc:string->path (arc:eval-arc (caddr expr)))))
      ((path-abbreviate)
       (arc:path->string
        (arc:path-abbreviate (arc:string->path (arc:eval-arc (cadr expr))))))
      ((path-normalize)
       (arc:path->string
        (arc:path-normalize (arc:string->path (arc:eval-arc (cadr expr))))))
      ((path-absolutize)
       (arc:path->string
        (arc:path-absolutize (arc:string->path (arc:eval-arc (cadr expr))))))
      


      ;; list functions
      ((list) (arc:eval-list (cdr expr)))
      ((cdr) (apply cdr (arc:eval-list (cdr expr))))
      ((car) (apply car (arc:eval-list (cdr expr))))
      ((string-list->string) (apply arc:string-list->string
                                    (arc:eval-list (cdr expr))))
      ((length) (apply length (arc:eval-list (cdr expr))))
      ((make-alist) (apply arc:list->alist (arc:eval-list (cdr expr))))

      ((map) (apply map (arc:eval-list (cdr expr))))
      ((for-each) (apply for-each (arc:eval-list (cdr expr))))


      ((lambda) 
       (begin
         (lambda args
           (let* ((frame (arc:make-frame)))
             (if (symbol? (cadr expr))
                 (arc:frame-assign! frame (cadr expr) args)
                 (let inner-loop ((a args)
                                  (e (cadr expr)))
                   (if (null? a)
                       #t
                       (begin
                         (arc:frame-assign! frame (car e) (car a))
                         (inner-loop (cdr a) (cdr e))))))
             (set! %arc:frames% (cons frame %arc:frames%))
             (let body-loop ((x (cddr expr))
                             (retv #f))
               (if (null? x)
                   (begin
                     (set! %arc:frames% (cdr %arc:frames%))
                     retv)
                   (body-loop (cdr x)
                              (arc:eval-arc (car x)))))))))
      
      
      ;; i/o
      ((echo) (apply arc:msg (arc:eval-list (cdr expr))))
      
      ;; access to properties and statements
      ((->) (arc:eval-stmt (cadr expr) 'local))
      ((prop) (or (arc:current-context-property (cadr expr))
                  (arc:env-get (cadr expr))))

      ;; else: (i) see if the called function is a declared function (from
      ;; named let for instance), than (ii) look if it is a generic tasks
      (else (let ((var (arc:frame-lookup (car expr))))
              (if var
                  (if (procedure? (cdr var))
                      (apply (cdr var) (arc:eval-list (cdr expr)))
                      (arc:log 'fatal "unbound variable in task position: " 
                               (car expr)))
                  (arc:eval-task (car expr) (cdr expr)))))) )
    
   ;; symbols beginning with : are regarded as self-quoting
   ((symbol? expr) (let ((ss (symbol->string expr)))
                     (if (equal? (string-ref ss 0) #\:)
                         expr
                         (let ((var (arc:frame-lookup expr)))
                           (if var
                               (cdr var)
                               (begin
                                 (arc:log 'error "unbound variable: " expr)
                                 #f))))))
   
   (else expr)))

(define (arc:eval-list lst)
  (map (lambda (x)
         (arc:eval-arc x))
       lst))

(define (arc:eval-let* expr)
  (let ((frame (arc:make-frame)))
    (set! %arc:frames% (cons frame %arc:frames%))
    (let loop ((x (cadr expr)))
      (if (null? x)
          #t
          (begin
            (arc:bind-var-to-frame frame (caar x)
                                   (arc:eval-arc (cadar x)))
            (loop (cdr x)))))
    (let loop ((x (cddr expr))
               (retv #f))
      (if (null? x)
          (begin
            (set! %arc:frames% (cdr %arc:frames%))
            retv)
          (loop (cdr x)
                (arc:eval-arc (car x)))))))

(define (arc:eval-let expr)
  (let ((frame (arc:make-frame)))
    (let loop ((x (cadr expr)))
      (if (null? x)
          #t
          (begin
            (arc:bind-var-to-frame frame (caar x)
                                   (arc:eval-arc (cadar x)))
            (loop (cdr x)))))
    
    (set! %arc:frames% (cons frame %arc:frames%))
    
    (let loop ((x (cddr expr))
               (retv #f))
      (if (null? x)
          (begin
            (set! %arc:frames% (cdr %arc:frames%))
            retv)
          (loop (cdr x)
                (arc:eval-arc (car x)))))))

(define (arc:eval-named-let label expr)
  (let* ((frame (arc:make-frame))
         (body (lambda args
                 (let inner-loop ((a args)
                                  (e (car expr)))
                   (if (null? a)
                       #t
                       (begin
                         (arc:frame-assign! frame (caar e) (car a))
                         (inner-loop (cdr a) (cdr e)))))
                 
                 (let body-loop ((x (cdr expr))
                                 (retv #f))
                   (if (null? x)
                       retv
                       (body-loop (cdr x)
                                  (arc:eval-arc (car x)))))) )
         ;; make argument list
         (args (let arg-loop ((a (car expr))
                              (retv ()))
                 (if (null? a)
                     retv
                     (arg-loop (cdr a)
                               (append retv 
                                       (list (arc:eval-arc (cadar a)))))))) )
    
    ;; make frame visible
    (arc:bind-var-to-frame frame label body)
    (set! %arc:frames% (cons frame %arc:frames%))
    
    ;; start body loop 
    (let ((retv (apply body args)))
      (set! %arc:frames% (cdr %arc:frames%))
      retv)))


(define (arc:eval-task task-name expr)
  (let* ((task (arc:lookup-task task-name))
         (rtv (arc:get-keywords-and-body expr))
         (props (if task
                    (or (arc:compile-key-list (caddr task)
                                              (car rtv)
                                              task-name)
                        (begin
                          (arc:msg "missing properties")
                          (quit)))
                    #f)))
    (if task
        (arc:try (lambda (type arg)
                   (arc:log 'error (arc:to-str "(" type ") "
                                               task-name ": " arg)))
                 (lambda ()
                   (apply (cadr task) (list props (cdr rtv)))))
        (arc:msg "unknown task: " task-name))))

(define (arc:call-task task-name param-alist body)
  (let* ((task (arc:lookup-task task-name))
         (props (or (arc:compile-key-list (caddr task) param-alist task-name)
                    (begin
                      (arc:msg "missing properties")
                      (quit)))) )
    (if task
        (arc:try (lambda (type arg)
                   (arc:log 'error (arc:to-str "(" type ") "
                                               task-name ": " arg)))
                 (lambda ()
                   (apply (cadr task) (list props body))))
        (arc:msg "unknown task: " task-name))))

;; functions



;; ----------------------------------------------------------------------
;; task registry
;; ----------------------------------------------------------------------
(define arc:tasks ())

(define (arc:register-task task-name proc keyword-table)
  (let ((aval (assoc task-name arc:tasks)))
    (if aval
        (set-cdr! aval (list proc keyword-table))
        (set! arc:tasks (append arc:tasks (list (list task-name 
                                                      proc
                                                      keyword-table)))))))

(define (arc:lookup-task task-name)
  (assoc task-name arc:tasks))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
