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

;; input target-system: %arc:system%
;; input installation-path of home, %arc:path%
;; input installation-path of scripts, %arc:exec%
;; input scheme-implementation, %arc:impl%

(load "../src/strings.scm")
(load "../src/misc.scm")
(load "../src/getopt.scm")
(load "../src/sysnm.scm")
(load "../src/filter.scm")

(define %arc:system% "")
(define %arc:path% "")
(define %arc:exec% "")
(define %arc:impl% "")

(define arc:opts '((sys "-s" "--sys" #t "the system name")
                   (path "-p" "--path" #t "the arc home path (e.g. /usr/local/share/arc)")
                   (impl "-i" "--impl" #t "the scheme implementation")
                   (exec "-x" "--exec" #t "the execution path (e.g. /usr/local/bin)") ))

(let loop ((opt (arc:getopt %arc:argv% arc:opts)))
  (if (not opt)
      #f
      (begin
        (case opt
          ((sys) (set! %arc:system% *arc:optarg*))
          ((path) (set! %arc:path% *arc:optarg*))
          ((exec) (set! %arc:exec% *arc:optarg*))
          ((impl) (set! %arc:impl% *arc:optarg*))
          ((#\?) (begin 
                   (arc:msg "unknown option: " *arc:optopt*) 
                   (quit)))
          ((#\:) (begin
                   (arc:msg "missing arg " *arc:optopt*) 
                   (quit)))
          (else (begin
                  (arc:msg "unknown option: " opt))))
        (loop (arc:getopt %arc:argv% arc:opts)))))

(define %arc:src-dir% (arc:path->string
                       (arc:path-append
                        (arc:path-without-last-comp (arc:path-cwd))
                        "src")))

(define (arc:pdisplay port . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline port))
            (else (display (car v) port)))
          (loop (cdr v))))))


(define %arc:sysnm% (arc:canonical-sysnm %arc:system% #f #f #f ))

(case (car %arc:sysnm%)
  ((linux bsd sunos) (load "./bstr-unix.scm"))
  ((beos) (load "./bstr-beos.scm"))
  ((win32) (load "./bstr-win32.scm"))
  (else (begin
          (display "unknown system. can't install arc automatically")
          (newline)
          (quit))))


(cond
 ((string-ci=? %arc:impl% "asc") 'nop)
                                   
 ((or (string-ci=? %arc:impl% "scm")
      (string-ci=? %arc:impl% "guile")
      (string-ci=? %arc:impl% "ksi")) (prepare-script))
 (else (begin
         (display "unknown scheme implementation")
         (newline)
         (quit))))

(bootstrap-script)

(arc:filter-file "../src/config.scm.in" "../src/config.scm"
                 `(("arc-home" %arc:path%)
                   ("exec-path" %arc:exec%)
                   ("include-path" ,(include-path))
                   ("host-os" ,(car %arc:sysnm%))
                   ("host-cpu" ,(cadr %arc:sysnm%))
                   ("host-maker" ,(caddr %arc:sysnm%))
                   ("host-version" ,(cadddr %arc:sysnm%))) )

(quit)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
