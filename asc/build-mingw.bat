@echo off
rem use this build script on windows and msdos system, when you don't have
rem a bash or something similar installed

rem expects the following environment parameters set
rem ASC_HOME   - gives the home directory where to install the asc init files 

rem build script for unix environments
echo #define IMPLINIT "%ASC_HOME%/asc-init.scm" > scmenv.h
echo #define INIT_FILE_NAME "asc-init.scm" >> scmenv.h

rem Compile C source files
gcc -O -c ioext.c continue.c scm.c scmmain.c findexec.c script.c time.c repl.c scl.c eval.c sys.c subr.c debug.c unif.c rope.c

rem Link C object files
gcc -o asc ioext.o continue.o scm.o scmmain.o findexec.o script.o time.o repl.o scl.o eval.o sys.o subr.o debug.o unif.o rope.o -lm

