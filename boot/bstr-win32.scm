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

;; bootstrapping code for win32 platforms

(define (prepare-script)
  (sys:mkdirs "../app/arc")
  (let ((port (open-output-file "../app/arc.bat")))
    (arc:pdisplay port 
                  "@echo off" 'nl
                  "if not \"%ARC_HOME%\"==\"\" goto START_ARC" 'nl
                  "set ARC_HOME=" %arc:path% 'nl
                  'nl
                  ":START_ARC" 'nl
                  (string-append "asc -s " %arc:path% "\\arc.scm "
                                 "-- $1 $2 $3 $4 $5 $6 $7 $8 $9")
                  'nl)
    (close-output-port port)) )

(define (bootstrap-script)
  (let ((port (open-output-file "../arc.bat")))
    (arc:pdisplay port 
                  "@echo off" 'nl
                  "set ARC_HOME=" %arc:src-dir% 'nl
                  "app\\arc.bat %1 %2 %3 %4 %5 %6 %7 %8 %9" 'nl)
    (close-output-port port)))
  
(define (include-path)
  (string-append %arc:path% ";"
                 %arc:path% "\\site"))

  
;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
