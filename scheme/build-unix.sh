#! /bin/sh

# expects the following environment parameters set
# ARC_INSTALL_HOME - gives the home directory where to install the arc init file

# build script for unix environments
echo "#define ARC_HOME \"$ARC_INSTALL_HOME\"" > scmenv.h
echo "#define INIT_FILE_NAME \"arc-init.scm\"" >> scmenv.h

INCLDIR="-Iinclude"

case "$1" in
    Linux)
	      PLATFORM_FEAT=-Dlinux
	      CFLAGS="-I. -O3 -Wall -static"
        OBJEXT=.o
        CC=gcc
	      ;;
    FreeBSD)
        PLATFORM_FEAT=-D__FreeBSD__
	      CFLAGS="-I. -O3 -Wall -static"
        OBJEXT=.o
        CC=cc
	      ;;
    Darwin)
        PLATFORM_FEAT=-DOSX
        CFLAGS="-I. -g"
        LDFLAGS="-framework Carbon"
        OBJEXT=.o
        CC=gcc
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
    
FEATURES="-DUSE_WARN_UNDEFS=0"

# Compile C source files
echo "Compiling arc source files ... ($CC $CFLAGS $INCLDIR $ARCH $PLATFORM_FEAT)"
$CC $INCLDIR $CFLAGS $ARCH $PLATFORM_FEAT $FEATURES -c arc.c eval.c sexp.c dirport.c fsys.c

# Link C object files
echo "Linking arc ... ($CC $LDFLAGS)"
$CC $LDFLAGS -o arc arc.o eval.o sexp.o dirport.o fsys.o