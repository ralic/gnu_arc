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

;; $Id: sys-guile.scm,v 1.3 2003/04/13 23:42:10 eyestep Exp $


;; ----------------------------------------------------------------------
;; mkdir & mkdirs
;; ----------------------------------------------------------------------
(define (arc:sys.mkdir path)
  (mkdir path #o755))

(define (arc:sys.mkdirs path)
  (let ((dirnm (arc:string->path path)))
    (let loop ((pc 0))
      (if (>= pc (arc:path-length dirnm))
          #f
          (let ((aps (arc:path->string (arc:path-subpath dirnm 0 (+ pc 1)))))
            (if (not (file-exists? aps))
                (if (not (arc:sys.mkdir aps))
                    (begin
                      (arc:msg "failed to create directory '" aps "'")
                      #f)
                    (begin
                      (arc:log 'debug "directory '" aps "' created")
                      (loop (+ pc 1))))
                (loop (+ pc 1))))))))

;; ----------------------------------------------------------------------
;; remove
;; ----------------------------------------------------------------------
(define (arc:sys.remove-file path)
  (if (file-exists? path)
      (delete-file path)
      (begin
        (arc:msg "failed to delete file '" path "'")
        #f)))

(define (arc:sys.remove-dir path)
  (arc:traverse-dir path
                    (lambda (kind fn)
                      (case kind
                        ((:dir) (rmdir (arc:path->string fn)))
                        ((:file) (delete-file (arc:path->string fn))))))
  (rmdir path))


;; ----------------------------------------------------------------------
;; making file links
;; ----------------------------------------------------------------------
(define (arc:sys.symlink from to)
  (case (car %arc:sysnm%)
    ((linux beos) (let ((cmd (string-append "ln -s " from " " to)))
               (if (file-exists? to)
                   (arc:sys.remove-file to))
               (system cmd)))
    ((win32) (begin 
               (arc:msg "symlinks are not supported on windows plattforms")
               #f))
    ;; unknown, how other system do this
    (else #f)))


;; ----------------------------------------------------------------------
;; directory navigation
;; ----------------------------------------------------------------------
(define (arc:sys.chdir path)
  (if (file-exists? path)
      (begin
        (arc:log 'debug "change to path '" path "'")
        (chdir path)
        #t)
      #f))

(define (arc:sys.getcwd)
  (getcwd))

(define (arc:sys.homedir)
  (case (car %arc:sysnm%)
    ((win32) (or (getenv "HOME")
                 "c:/"))
    ((beos) (or (getenv "HOME")
                "/boot/home"))
    (else (getenv "HOME"))))


;; executes a system command, whereby the command may be accessable through
;; the global path.  The return value is the returnvalue of the system
;; command
(define (arc:sys.system cmd)
  (system cmd))

(define (arc:sys.stat fn)
  (if (file-exists? fn)
      (stat fn)
      #f))

;; ----------------------------------------------------------------------
;; read dir tools
;; ----------------------------------------------------------------------
(define (arc:sys.opendir dir)
  (if (file-exists? dir)
      (opendir dir)
      #f))

(define (arc:sys.readdir dir)
  (let ((fn (readdir dir)))
    (if (eof-object? fn)
        #f
        fn)))

(define (arc:sys.closedir dir)
  (closedir dir))

(define (arc:sys.get-mtime fn)
  (if (file-exists? fn)
      (vector-ref (arc:sys.stat fn) 9)
      0))

(define (arc:sys.utime fn atime mtime)
  (utime fn atime mtime))

(define (arc:sys.current-time)
  (current-time))


;; ----------------------------------------------------------------------
;; checks if a file is executable
;; ----------------------------------------------------------------------
(define (arc:sys.file-exists? fn)
  (file-exists? fn))

(define (arc:sys.file-executable? fn)
  (case (car %arc:sysnm%)
    ((linux beos) (if (file-exists? fn)
                 (> (arc:logand (vector-ref (stat fn) 2) #o111) 0)
                 #f))
    ((win32) (if (file-exists? fn)
                 (or (arc:string-suffix? fn ".com")
                     (arc:string-suffix? fn ".exe")
                     (arc:string-suffix? fn ".bat")
                     (arc:string-suffix? fn ".cmd"))
                 #f))
    (else #f)))

;; i have no idea how to make this portable.  the "stat" function returns
;; system dependend data, which is in itself consistent.  But I've no idea,
;; which flags are used by the specific operating system.  
;;
;; linux uses: 16384 has directory flag
;; win32 (2000): all directories have 12361 (8192 + 4096 + 64 + 8 + 1) set
(define (arc:sys.file-directory? fn)
  (case (car %arc:sysnm%)
    ((linux beos) (if (file-exists? fn)
                 (= (arc:logand (vector-ref (stat fn) 2) 16384) 16384)
                 #f))
    ((win32) (if (file-exists? fn)
                 (= (arc:logand (vector-ref (stat fn) 2) 12361) 12361)
                 #f))
    (else #f)))




;; ----------------------------------------------------------------------
;; copying
;; ----------------------------------------------------------------------
(define (arc:sys.copy-file file tofile)
  (if (file-exists? file)
      (copy-file file tofile)
      #f))

(define (arc:sys.copy-dir dir todir)
  (if (file-exists? dir)
      (let* ((dp (arc:string->path todir))
             (srcp (arc:string->path dir))
             (srcp-brl (arc:path-length (arc:path-without-last-comp srcp))))
        (arc:traverse-dir dir
                          (lambda (kind fn)
                            (let* ((pl (arc:path-length fn))
                                   (sp (arc:path-subpath fn srcp-brl pl))
                                   (tp (arc:path-append dp sp)))
                              (case kind
                                ((:dir) (arc:sys.mkdirs (arc:path->string tp)))
                                ((:file) 
                                 (begin
                                   (arc:sys.mkdirs 
                                    (arc:path->string (arc:path-without-last-comp tp)))
                                   (arc:sys.copy-file (arc:path->string fn)
                                                      (arc:path->string tp))))))))
        #t)
      #f))

(define (arc:sys.chmod fn mod)
  (case (car %arc:sysnm%)
    ((linux beos bsd sunos)
     (begin
       (if (symbol? mod)
           (case mod
             ((file) (chmod fn #o644))
             ((exec) (chmod fn #o755))))
       (if (integer? mod)
           (chmod fn mod))))
    (else 0)))

;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:




