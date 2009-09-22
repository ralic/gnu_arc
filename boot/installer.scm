  
   ;; check if we have an system installer:
  (set! installer
        (cond
         ;; the gnu installer
         ((= (sys:execute* "install" #("--help" ">" "/dev/null" "2>&1")) 0) 'ginstall)
         ;; some other installer
         ((= (sys:execute* "install" #("-h" ">" "/dev/null" "2>&1")) 0) 'install)
         ;; nothing known
         (else 'unknown)))

  ;; install
  (case installer
    ((ginstall) (begin
                  (sys:execute "install" #("-d" "-v" "arc" %arc:exec%))
                  ;; install the 
                  (sys:execute "install" #("-d" "-v" "../src/*.scm" %arc:path%))
                  (sys:execute "install" #("-d" "-v" "../src/tasks/*.scm" 
                                           (string-append %arc:path% "/tasks/"))) ))
    ((install) (begin
                 (display "to be done") (newline)))
    
    (else (begin
            (display "to be done") (newline)))) )
