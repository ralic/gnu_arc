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

(define (arc:scan-dir-with-pattern root-path pattern)
  (let* ((cwd (arc:sys 'getcwd))
         (p1 (arc:string->path pattern))
         (retv '()))
    (arc:traverse-dir (arc:path->string root-path)
                      (lambda (kind fn)
                        (let ((e (arc:path-abbreviate fn)))
                          (if (arc:-match-path p1 e)
                              (set! retv 
                                    (append retv (list (arc:path->string e))))))))
    retv))

(define (arc:-match-path p1 p2)
  (cond
   ((or (null? p1) (null? p2)) #f)
   ((string=? (car p1) "*")
    (if (and (null? (cdr p1))
             (null? (cdr p2)))
        #t
        (if (arc:-match-path (cdr p1) (cdr p2))
            #t
            #f)))
   ((string=? (car p1) "**")
    (if (and (null? (cdr p1))
             (null? (cdr p2)))
        #t
        (if (and (not (null? (cdr p1)))
                 (or (string=? (cadr p1) (car p2))
                     (arc:-match-suffix-pattern (cadr p1) (car p2)))
                 (arc:-match-path (cdr p1) p2))
            #t
            (if (arc:-match-path p1 (cdr p2))
                #t
                #f))))
   ((string=? (car p1) (car p2))
    (if (and (null? (cdr p1))
             (null? (cdr p2)))
        #t
        (if (arc:-match-path (cdr p1) (cdr p2))
            #t 
            #f)))
   ((arc:-match-suffix-pattern (car p1) (car p2))
    (if (and (null? (cdr p1))
             (null? (cdr p2)))
        #t
        (if (arc:-match-path (cdr p1) (cdr p2))
            #t 
            #f)))
   (else #f)))

;; p1: the suffix pattern (step element from the pattern)
;; p2: the real path element (from the directory checked)
(define (arc:-match-suffix-pattern p1 p2)
  (cond
   ((arc:string-prefix? p1 "*")
    (arc:string-suffix? p2 (arc:pattern-extr-wildcard p1)))
   ((and (arc:string-prefix? p1 "(")
         (arc:string-suffix? p1 ")"))
    (pregexp-match p1 p2))
   (else #f)))

(define (arc:pattern-extr-wildcard pattern-str)
  (let loop ((lp (- (string-length pattern-str) 1)))
    (if (< lp 0)
        pattern-str
        (if (equal? (string-ref pattern-str lp) #\*)
            (substring pattern-str (+ lp 1) (string-length pattern-str))
            (loop (- lp 1))))))


(define (arc:substract-files-list file-list exclude-list)
  (arc:reduce (lambda (elt lst)
                (if (member elt exclude-list)
                    lst
                    (cons elt lst)))
              '()
              file-list)
  )

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
