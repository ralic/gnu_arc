  
   ;; check if we have an system installer:
  (set! installer
        (cond
         ;; the gnu installer
         ((= (system "install --help > /dev/null 2>&1") 0) 'ginstall)
         ;; some other installer
         ((= (system "install -h > /dev/null 2>&1") 0) 'install)
         ;; nothing known
         (else 'unknown)))

  ;; install
  (case installer
    ((ginstall) (begin
                  (system (string-append "install -d -v arc " %arc:exec%))
                  ;; install the 
                  (system (string-append "install -d -v ../src/*.scm "
                                         %arc:path%))
                  (system (string-append "install -d -v ../src/tasks/*.scm "
                                         %arc:path% "/tasks/")) ))
    ((install) (begin
                 (display "to be done") (newline)))
    
    (else (begin
            (display "to be done") (newline)))) )
