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

;; $Id: sys.scm,v 1.1 2003/04/19 01:03:35 eyestep Exp $


(define <arc:sys-generic>
  (arc:make-class 
   '<arc:sys-generic>                   ; name of the class
   <arc:object>                         ; superclass
   '()                                  ; slots

   ;; methods
   `((mkdir ,(lambda (self path) 
               (mkdir path #o755)))

     (mkdirs 
      ,(lambda (self path)
         (let ((dirnm (arc:string->path path)))
           (let loop ((pc 0))
             (if (>= pc (arc:path-length dirnm))
                 #f
                 (let ((aps (arc:path->string (arc:path-subpath dirnm 0
                                                                (+ pc 1)))))
                   (if (not (self 'file-exists? aps))
                       (if (not (self 'mkdir aps))
                           (begin
                             (arc:log 'error 
                                      "failed to create directory '" aps "'")
                             #f)
                           (begin
                             (arc:log 'debug "directory '" aps "' created")
                             (loop (+ pc 1))))
                       (loop (+ pc 1)))))))) )

     (remove-file 
      ,(lambda (self path)
         (arc:log 'debug "remove file '" path "'")
         (case %arc:scheme-impl%
           ((guile ksi) (if (file-exists? path)
                            (delete-file path)
                            (begin
                              (arc:msg "xfailed to delete file '" path "'")
                              #f)))
           ((scm asc) (if (not (delete-file path))
                          (begin
                            (arc:log 'error "failed to delete file '" path "'")
                            #f)
                          #t))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (remove-dir
      ,(lambda (self path)
         (arc:traverse-dir 
          path
          (lambda (kind fn)
            (case kind
              ((:dir) (begin
                        (arc:log 'debug "remove dir '" 
                                 (arc:path->string fn) 
                                 "'")
                        (rmdir (arc:path->string fn))))
              ((:file) (self 'remove-file (arc:path->string fn))))))
         (rmdir path)))


     (symlink
      ,(lambda (self from to)
         (case %arc:scheme-impl%
           ((guile) (begin
                      (if (file-exists? to)
                          (self 'remove-file to))
                      (symlink from to)
                      #t))
           ((scm asc ksi) (let ((cmd (string-append "ln -s " from " " to)))
                            (if (file-exists? to)
                                (arc:sys 'remove-file to))
                            (system cmd)))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
           

     (chdir
      ,(lambda (self path)
         (case %arc:scheme-impl%
           ((guile ksi) (if (file-exists? path)
                            (begin
                              (arc:log 'debug "change to path '" path "'")
                              (chdir path)
                              #t)
                            #f))
           ((scm asc) (begin
                        (arc:log 'debug "change to path '" path "'")
                        (chdir path)))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
           

     (getcwd
      ,(lambda (self)
         (getcwd)) )
     
     (homedir
      ,(lambda (self)
         (getenv "HOME")) )
     
     (system
      ,(lambda (self cmd)
         (system cmd)) )
         

     (stat 
      ,(lambda (self fn)
         (case %arc:scheme-impl%
           ((guile ksi) (and (file-exists? fn)
                             (stat fn)))
           ((scm asc) (stat fn))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (opendir
      ,(lambda (self dir)
         (case %arc:scheme-impl%
           ((guile ksi) (if (file-exists? dir)
                            (opendir dir)
                            #f))
           ((scm asc) (opendir dir))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (readdir
      ,(lambda (self dir)
         (case %arc:scheme-impl%
           ((guile ksi) (let ((fn (readdir dir)))
                          (if (eof-object? fn)
                              #f
                              fn)))
           ((scm asc) (readdir dir))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (closedir 
      ,(lambda (self dir)
         (closedir dir)))

     (mtime 
      ,(lambda (self fn)
         (case %arc:scheme-impl%
           ((guile ksi) (if (file-exists? fn)
                            (vector-ref (arc:sys 'stat fn) 9)
                            0))
           ((asc scm) (let ((fstat (arc:sys 'stat fn)))
                        (if fstat 
                            (vector-ref fstat 9)
                            0)))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (utime 
      ,(lambda (self fn atime mtime)
         (utime fn atime mtime)))

     (current-time
      ,(lambda (self)
         (current-time)) )

     (file-exists? 
      ,(lambda (self fn)
         (file-exists? fn)) )

     (file-directory? 
      ,(lambda (self fn)
         (case %arc:scheme-impl%
           ((guile) (and (file-exists? fn)
                         (eq? (vector-ref (stat fn) 13) 'directory)))
           ((asc scm ksi)
            (and (file-exists? fn)
                 (= (arc:logand (vector-ref (stat fn) 2) 16384) 
                    16384)) )
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
  
     
     (file-executable? 
      ,(lambda (self fn)
         (and (file-exists? fn)
              (> (arc:logand (vector-ref (stat fn) 2) #o111) 0))))

     
     (copy-file 
      ,(lambda (self file tofile)
         (case %arc:scheme-impl%
           ((guile) (if (file-exists? file)
                        (copy-file file tofile)
                        #f))
           ((scm asc ksi)
            ;; low level in-scheme only copy.  this is slow, and doesn't
            ;; copies parameters, flags, etc.  This should be overridden by
            ;; system specific implementation
            (if (file-exists? file)
                (let ((*in* (open-input-file file))
                      (*out* (open-output-file tofile)))
                  (do ((c (read-char *in*) (read-char *in*)))
                      ((eof-object? c) #t)
                    (write-char c *out*))
                  (close-output-port *out*)
                  (close-input-port *in*)
                  #t)
                #f))
           (else
            (arc:throw 'internal "unknown scheme implementation")))) )
     
     (copy-dir 
      ,(lambda (self dir todir)
         (if (file-exists? dir)
             (let* ((dp (arc:string->path todir))
                    (srcp (arc:string->path dir))
                    (srcp-brl (arc:path-length
                               (arc:path-without-last-comp srcp))))
               (arc:traverse-dir 
                dir
                (lambda (kind fn)
                  (let* ((pl (arc:path-length fn))
                         (sp (arc:path-subpath fn srcp-brl pl))
                         (tp (arc:path-append dp sp)))
                    (case kind
                      ((:dir) (self 'mkdirs (arc:path->string tp)))
                      ((:file) 
                       (begin
                         (self 'mkdirs 
                               (arc:path->string (arc:path-without-last-comp tp)))
                         (self 'copy-file (arc:path->string fn)
                               (arc:path->string tp))))))))
               #t)
             #f)) )

     (chmod 
      ,(lambda (self fn mod)
         (cond
          ((symbol? mod) (case mod
                           ((file) (chmod fn #o644))
                           ((exec) (chmod fn #o755))))
          ((integer? mod)
           (system (string-append "chmod "
                                  (number->string mod 8)
                                  " "
                                  fn)))
          (else
           (arc:throw 'internal 
                      "chmod: expected symbol or integer as mod")))) )
     
     
     )))


(define arc:sys
  (case (car %arc:sysnm%)
    ((linux) (begin
               (load (string-append %arc:home% "/sys-unix.scm"))
               (load (string-append %arc:home% "/sys-linux.scm"))
               (<arc:sys-linux> 'alloc)))
    ((beos) (begin
              (load (string-append %arc:home% "/sys-unix.scm"))
              (load (string-append %arc:home% "/sys-beos.scm"))
              (<arc:sys-beos> 'alloc)))
    ((bsd) (begin
             (load (string-append %arc:home% "/sys-unix.scm"))
             (load (string-append %arc:home% "/sys-bsd.scm"))
             (<arc:sys-bsd> 'alloc)))
    ((sunos) (begin
               (load (string-append %arc:home% "/sys-unix.scm"))
               (load (string-append %arc:home% "/sys-sunos.scm"))
               (<arc:sys-sunos> 'alloc)))
    ((win32) (begin
               (load (string-append %arc:home% "/sys-win32.scm"))
               (<arc:sys-win32> 'alloc)))
    ((cygwin) (begin
                (load (string-append %arc:home% "/sys-unix.scm"))
                (load (string-append %arc:home% "/sys-cygwin.scm"))
                (<arc:sys-cygwin> 'alloc)))
    (else
     (arc:throw 'internal "system not supported: " (car %arc:sysnm%)))))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
