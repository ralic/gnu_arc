#! /bin/sh

# expects the following environment parameters set
# ASC_HOME   - gives the home directory where to install the asc init files 

# build script for unix environments
echo "#define ARC_HOME \"$ASC_HOME\"" > scmenv.h
echo "#define IMPLINIT \"$ASC_HOME/asc-init.scm\"" >> scmenv.h
echo "#define INIT_FILE_NAME \"asc-init.scm\"" >> scmenv.h

case "$1" in
    Linux)
	      CFLAGS=-Dlinux
	      ;;
    FreeBSD)
	      CFLAGS=-D__FreeBSD__
	      ;;
    Darwin)
        PLATFORM_FEAT=-DOSX
	      ;;
    *)
esac

case "$2" in
    i386 | i486 | i586 | i686)
	ARCH="-DWordSize_32 -DByteOrder_LE"
	;;
    x86_64)
	ARCH="-DWordSize_64 -DByteOrder_LE"
	;;
    ppc)
	ARCH="-DWordSize_32 -DByteOrder_BE"
	;;
    ppc64)
	ARCH="-DWordSize_64 -DByteOrder_BE"
	;;
    *)
esac
    
# Compile C source files
echo "Compiling tasc source files ... ($CFLAGS $ARCH)"
cc $PLATFORM_FEAT -DUSE_MATH=0 -DUSE_DL=0 -DUSE_ASCII_NAMES=1 -DUSE_STRLWR=1

cc $CFLAGS $ARCH -O -c ioext.c continue.c scm.c scmmain.c findexec.c script.c time.c repl.c scl.c eval.c sys.c subr.c debug.c unif.c rope.c

# Link C object files
echo "Linking asc ..."
cc -o asc ioext.o continue.o scm.o scmmain.o findexec.o script.o time.o repl.o scl.o eval.o sys.o subr.o debug.o unif.o rope.o -lm -lc

