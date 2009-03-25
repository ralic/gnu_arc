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

(define %arc:path-sep% #\/)
(define %arc:ext-sep% #\.)

(define (arc:path->string path)
  (let ((ps (string %arc:path-sep%)))
    (let loop ((res "")
               (pl path))
      (if (null? pl)
          res
          (loop (string-append res 
                               (if (or (equal? res "") 
                                       (equal? res ps)) "" ps)
                               (list-ref pl 0))
                (cdr pl))))))


(define (arc:string->path str)
  (let ((t (arc:split-string str %arc:path-sep%)))
    (if (and (> (string-length str) 0)
             (equal? (string-ref str 0) %arc:path-sep%))
        (cons "/" t)
        t)))

(define (arc:path-length path)
  (length path))

(define (arc:path->list path)
  path)

(define (arc:list->path l)
  l)

(define (arc:path-subpath path start end)
  (arc:sublist path start end))

(define (arc:path-without-last-ext path)
  (let ((ll (length path)))
    (if (> ll 0)
        (let* ((str (list-ref path (- ll 1)))
               (strl (string-length str))
               (ext-pos (let loop ((lp (- strl 1)))
                          (if (< lp 0)
                              #f
                              (if (equal? (string-ref str lp) %arc:ext-sep%)
                                  lp
                                  (loop (- lp 1))))))
               (newval (if ext-pos
                           (substring str 0 ext-pos)
                           (if (equal? ext-pos 0)
                               ""
                               str)))
               )
          (append (arc:sublist path 0 (- ll 1)) (list newval)))
        path)))

(define (arc:path-without-last-comp path)
  (let ((ll (length path)))
    (if (> ll 0)
        (arc:sublist path 0 (- (length path) 1))
        ())))

(define (arc:path-append path comp)
  (if (list? comp)
      (append path comp)
      (append path (list comp))))

(define (arc:path-append-ext path ext)
  (let ((ll (length path)))
    (if (> ll 0)
        (append (arc:sublist path 0 (- ll 1))
                (list (string-append (car (list-tail path (- ll 1)) )
                                     (string %arc:ext-sep%) ext)))
        ())))

(define (arc:path-replace-last-ext path ext)
  (arc:path-append-ext (arc:path-without-last-ext path) ext))

(define (arc:path-last-comp path)
  (let ((ll (length path)))
    (if (> ll 0)
        (list-tail path (- ll 1))
        ())))

(define (arc:path-ext path)
  (let ((ll (length path)))
    (if (> ll 0)
        (let* ((str (list-ref path (- ll 1)))
               (strl (string-length str))
               (ext-pos (let loop ((lp (- strl 1)))
                          (if (< lp 0)
                              #f
                              (if (equal? (string-ref str lp) %arc:ext-sep%)
                                  lp
                                  (loop (- lp 1)))))) )
          (if ext-pos
              (substring str (+ ext-pos 1) strl)
              ""))
        "")))

(define (arc:path-cwd)
  (arc:string->path (arc:sys 'getcwd)))

(define (arc:home-dir)
  (arc:string->path (arc:sys 'homedir)))

(define (arc:path-absolute? path)
  (and (list? path)
       (> (length path) 0)
       (equal? (car path) (string %arc:path-sep%))))


;; checks if a path begins with a prefix path.  Both path and prefix must
;; be paths
(define (arc:path-begins-with? path prefix)
  (if (< (length path) (length prefix))
      #f
      (let loop ((pc path)
                 (fx prefix))
        (if (or (null? pc)
                (null? fx))
            #t
            (if (not (string-ci=? (car pc) (car fx)))
                #f
                (loop (cdr pc) (cdr fx)) )))))

;; abbreviates a path to its shortest possible form (by removing common
;; parts to the current working directory)
(define (arc:path-abbreviate path)
  (let* ((cwd (arc:path-cwd))
         (home (arc:home-dir))
         (normp (arc:path-normalize path)))
    (cond
     ((arc:path-begins-with? normp cwd)
      (arc:path-subpath normp (length cwd) (length normp)))
     ((arc:path-begins-with? normp home)
      (arc:path-append '("~")
                       (arc:path-subpath normp (length home) (length normp))))
     (else normp))))

;; normalizes a path by removing and resolving all ".." and "." parts.
(define (arc:path-normalize path)
  (let loop ((res ())
             (pc path))
    (if (null? pc)
        res
        (if (not (null? pc))
            (if (not (string=? (car pc) "."))
                (if (string=? (car pc) "..")
                    (loop (arc:sublist res 0 (- (length res) 1)) (cdr pc))
                    (loop (append res (list (car pc))) (cdr pc)))
                (loop res (cdr pc)))
            (loop (append res (list (car pc))) (cdr pc)) ))))

;; make a path absolute (by adding the current working directory if necessary)
(define (arc:path-absolutize path)
  (if (equal? (car path) "~")
      (set! path (arc:path-append (arc:home-dir)
                                  (arc:path-subpath path 1 
                                                    (arc:path-length path)))))
  (set! path (arc:path-normalize path))
  (if (arc:path-absolute? path)
      path
      (arc:path-append (arc:path-cwd) path)))


(define *arc:file-counter* (arc:make-counter 0 1))

(define (arc:temp-path)
  (arc:string->path (arc:sys 'tempdir)))

(define (arc:temp-file-name name)
  (arc:path->string
   (arc:path-append (arc:temp-path)
                    (string-append "arc-" (number->string (arc:sys 'getpid)) "-"
                                   name "-"
                                   (number->string (*arc:file-counter*))))))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
