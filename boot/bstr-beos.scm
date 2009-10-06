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

;; bootstraping code for the beos system

(define (prepare-script)
  (sys:mkdirs "../app")
  (let ((port (open-output-file "../app/arc")))
    (arc:pdisplay port 
                  "#!/bin/sh" 'nl
                  'nl
                  "if [ -z \"$ARC_HOME\" ]; then" 'nl
                  "  export ARC_HOME=" %arc:path% 'nl
                  "fi" 'nl
                  'nl
                  "asc -s $ARC_HOME/arc.scm -- $*"
                  'nl)
    (close-output-port port)
    (sys:chmod "../app/arc" 'exec)) )

(define (bootstrap-script)
  (let ((port (open-output-file "../arc")))
    (arc:pdisplay port 
                  "#!/bin/sh" 'nl
                  'nl
                  "export ARC_HOME=" %arc:src-dir% 'nl
                  "exec app/arc $*" 'nl)
    (close-output-port port)
    (sys:chmod "../arc" 'exec)) )

(define (include-path)
  (string-append %arc:path% ":"
		 (string-append %arc:path% "/site" ":"
                    "/boot/home/config/arc" ":"
                    "/boot/home/config/arc/site")))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
