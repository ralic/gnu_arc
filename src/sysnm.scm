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

(define (arc:canonical-sysnm os maker cpu version)
  (let ((os* (if os os "unknown"))
        (maker* (if maker maker "unknown"))
        (cpu* (if cpu cpu "unknown"))
        (version* (if version version "unknown")))
    (list (cond 
           ((string-ci=? os* "linux") 'linux)
           ((or (string-ci=? os* "bsd")
                (string-ci=? os* "freebsd")
                (string-ci=? os* "ultrix")) 'bsd)
           ((string-ci=? os* "win32") 'win32)
           ((or (string-ci=? os* "osx")
                (string-ci=? os* "darwin")) 'macosx)
           ((string-ci=? os* "beos") 'beos)
           ((or (string-ci=? os* "sunos")
                (string-ci=? os* "solaris")) 'sunos)
           (else 'unknown))
          (cond
           ((or (string-ci=? cpu* "i386")
                (string-ci=? cpu* "i486")
                (string-ci=? cpu* "i586")
                (string-ci=? cpu* "i686")) 'ix86)
           ((or (string-ci=? cpu* "ppc")) 'ppc)
           ((or (string-ci=? cpu* "alpha")
                (string-ci=? cpu* "21064")) 'alpha)
           ((or (string-ci=? cpu* "m68k")
                (string-ci=? cpu* "amiga")) 'm68k)
           ((or (string-ci=? cpu* "sparc")
                (string-ci=? cpu* "usparc")) 'sparc)
           (else 'unknown))
          maker*
          version*)))
         

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
