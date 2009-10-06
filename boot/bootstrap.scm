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

;; input target-system: %arc:system%
;; input installation-path of home, %arc:path%
;; input installation-path of scripts, %arc:exec%
;; input scheme-implementation, %arc:impl%

(define %arc:argv% (vector->list *args*))

(define %arc:debug% (let ((ff (sys:getenv "ARC_DEBUG")))
                      (if (and ff (equal? ff "yes")) #t #f)))

(load "../src/misc.scm")
(load "../src/path.scm")
(load "../src/strings.scm")
(load "../src/misc.scm")
(load "../src/getopt.scm")
(load "../src/sysnm.scm")
(load "../src/filter.scm")
(load "../src/excp.scm")
(load "../src/logical.scm")

(define %arc:system% "")
(define %arc:arch% "")
(define %arc:path% "")
(define %arc:prefix% "/usr")
(define %arc:exec% "")
(define %arc:impl% 'asc)

(define arc:opts '((sys "-s" "--sys" #t "the system name")
                   (arch "-a" "--arch" #t "the system architecture")
                   (pfx "-P" "--prefix" #t "the installation prefix")
                   (path "-p" "--path" #t "the arc home path (e.g. /usr/local/share/arc)")
                   (exec "-x" "--exec" #t "the execution path (e.g. /usr/local/bin)") ))

(let loop ((opt (arc:getopt %arc:argv% arc:opts)))
  (if (not opt)
      #f
      (begin
        (case opt
          ((sys)  (set! %arc:system% *arc:optarg*))
          ((arch) (set! %arc:arch% *arc:optarg*))
          ((path) (set! %arc:path% *arc:optarg*))
          ((pfx)  (set! %arc:prefix% *arc:optarg*))
          ((exec) (set! %arc:exec% *arc:optarg*))
          ((#\?) (begin 
                   (arc:msg "unknown option: " *arc:optopt*) 
                   (quit -1)))
          ((#\:) (begin
                   (arc:msg "missing arg " *arc:optopt*) 
                   (quit -1)))
          (else (begin
                  (arc:msg "unknown option: " opt))))
        (loop (arc:getopt %arc:argv% arc:opts)))))

(define %arc:sysnm% (arc:canonical-sysnm %arc:system% #f %arc:arch% #f ))

;(define %arc:home% "../src")
(load "../src/oop.scm")
(load "../src/sys.scm")

(define %arc:src-dir% (arc:path->string
                       (arc:path-append
                        (arc:path-without-last-comp (arc:path-cwd))
                        "src")))
(define %arc:app-dir% (arc:path->string
                       (arc:path-append
                        (arc:path-without-last-comp (arc:path-cwd))
                        "app")))

(define (arc:pdisplay port . values)
  (let loop ((v values))
    (if (null? v)
        #t
        (begin
          (case (car v)
            ((#\newline) (newline port))
            ((nl) (newline port))
            (else (display (car v) port)))
          (loop (cdr v))))))


(case (car %arc:sysnm%)
  ((linux bsd sunos cygwin darwin) (load "./bstr-unix.scm"))
  ((beos)                          (load "./bstr-beos.scm"))
  ((win32)                         (load "./bstr-win32.scm"))
  (else (begin
          (display "unknown system. can't install arc automatically")
          (newline)
          (quit -1))))


(bootstrap-script)

(arc:filter-file "../src/config.scm.in" "../src/config.scm"
                 `(("arc-home"     ,%arc:path%)
                   ("exec-path"    ,%arc:exec%)
                   ("include-path" ,(include-path))
                   ("host-os"      ,(car %arc:sysnm%))
                   ("host-cpu"     ,(cadr %arc:sysnm%))
                   ("host-maker"   ,(caddr %arc:sysnm%))
                   ("host-version" ,(cadddr %arc:sysnm%))) )

(arc:filter-file "../arc.config.in" "../arc.config"
                 `(("arc-home"     ,%arc:path%)
                   ("prefix"       ,%arc:prefix%)
                   ("exec-path"    ,%arc:exec%)
                   ("include-path" ,(include-path))
                   ("host-os"      ,(car %arc:sysnm%))
                   ("host-cpu"     ,(cadr %arc:sysnm%))
                   ("host-maker"   ,(caddr %arc:sysnm%))
                   ("host-version" ,(cadddr %arc:sysnm%))) )

(quit 0)


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
