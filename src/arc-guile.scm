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

(define %arc:home% (or (getenv "ARC_HOME")
                       "/usr/share/arc"))

(define %arc:scheme-impl% 'guile)

;; setup the program arguments
(define %arc:argv% (list-tail (program-arguments) 1))

;;(load (string-append %arc:home% "/strings.scm"))
;;(load (string-append %arc:home% "/logical.scm"))

;; load system specific code
;;(load (string-append %arc:home% "/oop.scm"))
;;(load (string-append %arc:home% "/sys.scm"))
;;(load (string-append %arc:home% "/sys-guile.scm"))

;; eval the loader
(load (string-append %arc:home% "/arc.scm"))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
