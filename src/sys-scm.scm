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

;; $Id: sys-scm.scm,v 1.2 2003/04/12 23:55:54 eyestep Exp $

;;(load (string-append %arc:home% "/logical.scm"))


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
;                      (if %arc:verbose%
;                          (arc:msg "directory '" aps "' created"))
                      (loop (+ pc 1))))
                (loop (+ pc 1))))))))


;; ----------------------------------------------------------------------
;; remove
;; ----------------------------------------------------------------------
(define (arc:sys.remove-file path)
  (if %arc:verbose%
      (arc:msg "remove file '" path "'"))
  (if (not (delete-file path))
      (begin
        (arc:msg "failed to delete file '" path "'")
        #f)
      #t))

(define (arc:sys.remove-dir path)
  (arc:traverse-dir path
                    (lambda (kind fn)
                      (case kind
                        ((:dir) (begin
                                  (if %arc:verbose%
                                      (arc:msg "remove dir '" 
                                               (arc:path->string fn) 
                                               "'"))
                                  (rmdir (arc:path->string fn))))
                        ((:file) (arc:sys.remove-file (arc:path->string fn))))))
  (rmdir path))


;; ----------------------------------------------------------------------
;; making file links
;; ----------------------------------------------------------------------
(define (arc:sys.symlink from to)
  (case (car %arc:sysnm%)
    ((linux) (let ((cmd (string-append "ln -s " from " " to)))
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
  (if %arc:verbose%
      (arc:msg "change to path '" path "'"))
  (chdir path))

(define (arc:sys.getcwd)
  (getcwd))

(define (arc:sys.homedir)
  (case (car %arc:sysnm%)
    ((win32) (let ((hm (getenv "HOME")))
               (or hm
                   "c:/")))
    (else (getenv "HOME"))))

;; executes a system command, whereby the command may be accessable through
;; the global path.  The return value is the returnvalue of the system
;; command
(define (arc:sys.system cmd)
  (system cmd))

(define (arc:sys.stat fn)
  (stat fn))


;; ----------------------------------------------------------------------
;; read dir tools
;; ----------------------------------------------------------------------
(define (arc:sys.opendir dir)
  (opendir dir))

(define (arc:sys.readdir dir)
  (readdir dir))

(define (arc:sys.closedir dir)
  (closedir dir))

(define (arc:sys.get-mtime fn)
  (let ((fstat (arc:sys.stat fn)))
    (if fstat 
        (vector-ref fstat 9)
        0)))


(define (arc:sys.utime fn atime mtime)
  (utime fn atime mtime))

(define (arc:sys.current-time)
  (current-time))


;; ----------------------------------------------------------------------
;; checks if a file is executable
;; ----------------------------------------------------------------------
(define (arc:sys.file-exists? fn)
  (file-exists? fn))

;; i have no idea how to make this portable.  the "stat" function returns
;; system dependend data, which is in itself consistent.  But I've no idea,
;; which flags are used by the specific operating system.  
;;
;; linux uses: 16384 has directory flag
;; win32 (2000): all directories have 12361 (8192 + 4096 + 64 + 8 + 1) set
(define (arc:sys.file-directory? fn)
  (case (car %arc:sysnm%)
    ((linux) (if (file-exists? fn)
                 (= (arc:logand (vector-ref (stat fn) 2) 16384) 16384)
                 #f))
    ((win32) (if (file-exists? fn)
                 (= (arc:logand (vector-ref (stat fn) 2) 12361) 12361)
                 #f))
    (else #f)))
  

(define (arc:sys.file-executable? fn)
  (case (car %arc:sysnm%)
    ((linux) (if (file-exists? fn)
                 (> (arc:logand (vector-ref (stat fn) 2) #o111) 0)
                 #f))
    ((win32) (if (file-exists? fn)
                 (or (arc:string-suffix? fn ".com")
                     (arc:string-suffix? fn ".exe")
                     (arc:string-suffix? fn ".bat")
                     (arc:string-suffix? fn ".cmd"))
                 #f))
    (else #f)))


;; ----------------------------------------------------------------------
;; copying
;; ----------------------------------------------------------------------
(define (arc:sys.copy-file file tofile)
  (case (car %arc:sysnm%)
    ((linux bsd macosx sunos beos)
     (let ((cpcmd (string-append "cp -f " file " " tofile)))
       (= (arc:sys.system cpcmd) 0)))
    ((win32)
     (let ((cpcmd (string-append "xcopy " file " " tofile)))
       (= (arc:sys.system cpcmd) 0)))
    ;; else low level in-scheme only copy.  this is slow, and doesn't
    ;; copies parameters, flags, etc.
    (else (if (file-exists? file)
              (let ((*in* (open-input-file file))
                    (*out* (open-output-file tofile)))
                (do ((c (read-char *in*) (read-char *in*)))
                    ((eof-object? c) #t)
                  (write-char c *out*))
                (close-output-port *out*)
                (close-input-port *in*)
                #t)
              #f))))

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
                                                      (arc:path->string tp))) )))))
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
