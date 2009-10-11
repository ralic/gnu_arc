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

;; this walks a tree and applies 'proc' to each file it finds
(define (arc:walk-tree cpath proc)
  (if (sys:file-directory? (arc:path->string cpath))
      (let ((dir (open-dir-port (arc:path->string cpath))))
        (if dir
            (begin
              (do ((filenm (read-dir-port dir) (read-dir-port dir)))
                  ((eof-object? filenm) 'done)
                (if (not (or (string=? filenm ".") 
                             (string=? filenm "..")))
                    (if (sys:file-directory? (arc:path->string 
                                              (arc:path-append cpath filenm)))
                        (if (not (eq? (arc:walk-tree (arc:path-append cpath filenm) proc)
                                      'could-not-open-dir))
                            (apply proc (list ':dir (arc:path-append cpath filenm))))
                        (apply proc (list ':file (arc:path-append cpath filenm))) )))
              (close-dir-port dir))
            'could-not-open-dir))
      'could-not-open-dir))


;; proc: the function takes two parameters, the first is either :dir or
;;       :file, indicating whether the entry is a directory or a file.  The
;;       second gives the file path (as returned by arc:string->path).
;;       Directories are reported *after* their content
(define (arc:traverse-dir path proc)
  (arc:walk-tree (arc:string->path path) proc))



;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
