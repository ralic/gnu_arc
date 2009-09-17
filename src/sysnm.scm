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

(define arc:uname-s-matrix
  '((("linux")                  linux)
    (("bsd" "freebsd" "ultrix") bsd)
    (("win32")                  win32)
    (("osx" "darwin")           darwin)
    (("beos")                   beos)
    (("sunos" "solaris")        sunos)
    (("CYGWIN_NT-5.0" "cygwin") cygwin)))
(define arc:uname-m-matrix
  '((("i386" "i486" "i586" "i686") ix86)
    (("x86_64")                    x86_64)
    (("ppc")                       ppc)
    (("ppc64")                     ppc64)
    (("alpha" "21064")             alpha)
    (("mk68k" "amiga")             m68k)
    (("sparc" "usparc")            sparc)))

(define (arc:find-in-uname-matrix matrix key)
  (let loop ((mtx matrix))
    (if (null? mtx)
        "unknown"
        (or (let loop2 ((nd (caar mtx)))
              (if (null? nd)
                  #f
                  (if (string-ci=? key (car nd))
                      (cadar mtx)
                      (loop2 (cdr nd)))))
            (loop (cdr mtx))))))


(define (arc:canonical-sysnm os maker cpu version)
  (let ((os*      (if os os "unknown"))
        (maker*   (if maker maker "unknown"))
        (cpu*     (if cpu cpu "unknown"))
        (version* (if version version "unknown")))
    (list 
     (arc:find-in-uname-matrix arc:uname-s-matrix os*)
     (arc:find-in-uname-matrix arc:uname-m-matrix cpu*)
     maker*
     version*)))
         

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
