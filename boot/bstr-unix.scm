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

;; bootstrapping code for unix/bsd-alike systems: linux, bsds, sunos

(define (prepare-script)
  (sys:mkdirs "../app")
  (let ((port (open-output-file "../app/arc")))
    (arc:pdisplay port 
                  "#!/bin/sh" 'nl
                  'nl
                  "if [ -z \"$ARC_HOME\" ]; then" 'nl
                  "  ARC_HOME=" %arc:path% 'nl
                  "  export ARC_HOME" 'nl
                  "fi" 'nl
                  'nl
                  "arc -s $ARC_HOME/arc.scm -- $*"
                  'nl)
    (close-output-port port)
    (sys:chmod "../app/arc" 'exec)) )

(define (bootstrap-script)
  (let ((port (open-output-file "../arc")))
    (arc:pdisplay port 
                  "#!/bin/sh" 'nl
                  'nl
                  "ARC_HOME=" %arc:src-dir% 'nl
                  "export ARC_HOME" 'nl
                  "ARC_INIT_DIR=" %arc:app-dir% 'nl
                  "export ARC_INIT_DIR" 'nl
                  "exec `dirname $0`/app/arc $*" 'nl)
    (close-output-port port)
    (sys:chmod "../arc" 'exec)) )

(define (include-path)
  (string-append %arc:path%                  ":"
                 "/usr/local/share/arc"      ":"
                 "/usr/share/arc"            ":"
                 "/usr/local/share/arc/site" ":"
                 "/usr/share/arc/site"       ":" 
                 "/usr/local/lib/arc"        ":" 
                 "/usr/lib/arc"))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
