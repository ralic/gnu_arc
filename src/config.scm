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

;; this script will be set up and changed at arc installation.  this script
;; should include some basic system informations, which won't normally
;; change, e.g. machine type, host type and name, installed os version and
;; type, etc.

;; $Id: config.scm,v 1.3 2003/04/19 01:08:37 eyestep Exp $


(define (arc:include-path) "/usr/local/share/arc:/usr/local/share/arc:/usr/share/arc:/usr/local/share/arc/site:/usr/share/arc/site:/usr/local/lib/arc:/usr/lib/arc")

(define (arc:host-os) "linux")
(define (arc:host-cpu) "unknown")
(define (arc:host-maker) "unknown")
(define (arc:host-version) "unknown")
(define (arc:exec-path) "/usr/local/bin")
(define (arc:home) "/usr/local/share/arc")

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
