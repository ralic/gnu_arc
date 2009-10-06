#! /bin/sh

# expects the following environment parameters set
# ASC_HOME   - gives the home directory where to install the asc init files 

# build script for unix environments
echo "#define ARC_HOME \"$ASC_HOME\"" > scmenv.h
echo "#define IMPL_INIT \"$ASC_HOME/asc-init.scm\"" >> scmenv.h
echo "#define INIT_FILE_NAME \"asc-init.scm\"" >> scmenv.h

case "$1" in
    Linux)
	      PLATFORM_FEAT=-Dlinux
	      CFLAGS="-I. -O3 -Wall -Wno-char-subscripts"
        OBJEXT=.o
        CC=gcc
	      ;;
    FreeBSD)
        PLATFORM_FEAT=-D__FreeBSD__
	      CFLAGS="-I. -O3 -Wall -Wno-char-subscripts"
        OBJEXT=.o
        CC=cc
	      ;;
    Darwin)
        PLATFORM_FEAT=-DOSX
        CFLAGS="-I. -O3 -Wno-switch-enum -Wno-char-subscripts -DSUN_DL"
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
    
CFLAGS2="-DSTANDALONE=0 -DUSE_MATH=0 -DUSE_ASCII_NAMES=1 -DUSE_STRLWR=1 -DUSE_COLON_HOOK=0 -DUSE_ERROR_HOOK=0 -DUSE_EXTOBJ=1"

# Compile C source files
echo "Compiling tasc source files ... ($CFLAGS $CFLAGS2 $ARCH $PLATFORM_FEAT)"
$CC $CFLAGS $CFLAGS2 $ARCH $PLATFORM_FEAT -c scheme.c main.c fsys.c dirport.c

# Link C object files
echo "Linking tasc ..."
$CC $LDFLAGS -o asc scheme.o main.o fsys.o dirport.o

# cp init.scm ../app/
# cp asc-init.scm ../app/
# cp tasc ../app/tarc