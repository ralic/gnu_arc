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

;; bootstrapping code for unix/bsd-alike systems: linux, bsds, sunos

(define (prepare-script)
  (arc:sys 'mkdirs "../app")
  (let ((port (open-output-file "../app/arc"))
        (scheme-cmd (cond 
                     ((string-ci=? %arc:impl% "scm") 'scm)
                     ((string-ci=? %arc:impl% "asc") 'asc)
                     ((string-ci=? %arc:impl% "guile") 'guile)
                     ((string-ci=? %arc:impl% "ksi") 'ksi)
                     (else 'asc))) )
    (arc:pdisplay port 
                  "#!/bin/sh" #\nl
                  #\nl
                  "if [ -z \"$ARC_HOME\" ]; then" #\nl
                  "  ARC_HOME=" %arc:path% #\nl
                  "  export ARC_HOME" #\nl
                  "fi" #\nl
                  #\nl
                  (case scheme-cmd
                    ((scm) "scm $ARC_HOME/arc-scm.scm $*" )
                    ((guile) "guile -l $ARC_HOME/arc-guile.scm -- $*")
                    ((asc) "asc -s $ARC_HOME/arc-asc.scm -- $*")
                    ((ksi) "ksi -s $ARC_HOME/arc-ksi.scm -- $*"))
                  #\nl)
    (close-output-port port)
    (arc:sys 'chmod "../app/arc" #o755)) )

(define (bootstrap-script)
  (let ((port (open-output-file "../arc"))
        (scheme-cmd (cond 
                     ((string-ci=? %arc:impl% "scm") 'scm)
                     ((string-ci=? %arc:impl% "asc") 'asc)
                     ((string-ci=? %arc:impl% "guile") 'guile)
                     ((string-ci=? %arc:impl% "ksi") 'ksi)
                     (else 'asc))) )
    (arc:pdisplay port 
                  "#!/bin/sh" #\nl
                  #\nl
                  "ARC_HOME=" %arc:src-dir% #\nl
                  "export ARC_HOME" #\nl
                  "exec app/arc $*" #\nl)
    (close-output-port port)
    (arc:sys 'chmod "../arc" #o755)) )

(define (include-path)
  (string-append %arc:path% ":"
                 "/usr/local/share/arc" ":"
                 "/usr/share/arc" ":"
                 "/usr/local/share/arc/site" ":"
                 "/usr/share/arc/site" ":" 
                 "/usr/local/lib/arc" ":" 
                 "/usr/lib/arc"))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
