/* Copyright (C) 1990-1999 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place, Suite 330, Boston, MA 02111, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of SCM.
 *
 * The exception is that, if you link the SCM library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the SCM library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name SCM.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * SCM, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for SCM, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#ifndef _asc_scmfig_h
#define _asc_scmfig_h
#define SHORT_INT
#define CDR_DOUBLES

/* "scmfig.h" system-dependent configuration.
   Author: Aubrey Jaffer */

#ifdef sequent
# include <strings.h>
# define strchr index
# define strrchr rindex
#else
# ifndef PLAN9
#  include <string.h>
# endif
#endif

#include "scmenv.h"

#define INITS init_ioext();
#define ARRAYS
#define BIGNUMS
#define FLOATS


/* IMPLINIT is the full pathname (surrounded by double quotes) of
   Init.scm, the Scheme initialization code.  This is best defined in
   the makefile.  If available, SCM uses the value of environment
   variable SCM_INIT_PATH instead of IMPLINIT. */

/* #define IMPLINIT "/usr/jaffer/scm/Init.scm" */

/* INITS is calls to initialization routines for any compiled
   libraries being linked into scm.  This is best done in the makefile.
File:	INITS line:		functions defined:

sc2.c	init_sc2();	substring-move-left!, substring-move-right!,
			substring-fill!, append!, and last-pair
rgx.c	init_rgx();	regcomp and regexec. */

/* #define INITS init_sc2(); */

/* #define SICP */

/* setbuf(0) needs to be done for tty ports in order for CHAR-READY?
   to work.  This can cause problems under MSDOS and other systems. */

/* #define NOSETBUF */

/* #define RECKLESS */

/* #define CAUTIOUS */

/* #define STACK_LIMIT (HEAP_SEG_SIZE/2) */

/* #define BIGNUMS */

/* #define ARRAYS */

/* #define FLOATS */

/* Define SINGLES if you want single precision floats and
   (sizeof(float)==sizeof(long)) */

#ifdef FLOATS
# ifndef _MSC_VER
#  define SINGLES
# endif
#endif

/* #define SINGLESONLY */

/* Define CDR_DOUBLES if (sizeof(double)==sizeof(long)), i.e.
   a `single' is really a double. */
#ifdef FLOATS
# ifdef __alpha
#  define CDR_DOUBLES
# endif

# ifdef _UNICOS		/* doubles are no better than singles on Cray. */
#  define SINGLESONLY
# endif

# ifdef CDR_DOUBLES
#  define SINGLES
#  define SINGLESONLY
# endif
#endif

/* #define ENGNOT */

/* Define SUN_DL to configure code in "dynl.c" so that dynamic linking
   is done using the SUN dynamic linking library "dl". */

/* #define SUN_DL */

/* Define DLD to configure code in "dynl.c" so that dynamic linking is
   done using the "dld" library.  DLD is ported to Linux, VAX
   (Ultrix), Sun 3 (SunOS 3.4 and 4.0), SPARCstation (SunOS 4.0),
   Sequent Symmetry (Dynix), and Atari ST.  See scm/README or
   scm/ANNOUNCE for ftp sites offering dld. */

/* #define DLD */

/* Define HAVE_DYNL if dynamic linking is available */

#ifdef DLD
# define HAVE_DYNL
#endif
#ifdef SUN_DL
# define HAVE_DYNL
#endif
#ifdef HP_SHL
# define HAVE_DYNL
#endif

#ifdef HAVE_DYNL
# define CCLO
#endif

/* Define GC_FREE_SEGMENTS if you want segments of unused heap to
   be freed up after garbage collection.  Don't define it if you
   never want the heap to shrink. */

#ifndef DONT_GC_FREE_SEGMENTS
# define GC_FREE_SEGMENTS
#endif

/* MEMOIZE_LOCALS means to convert references to local variables to ILOCs,
   (relative lexical addresses into the environment).  This memoization
   makes evaluated Scheme code harder to read, so you may want to undefine
   this flag for debugging -- but SCM will run 3 to 5 times slower */
#define MEMOIZE_LOCALS

/* #define CHEAP_CONTINUATIONS */

/* #define TICKS */

/* PROT386 should be defined on the compilation command line if the
   program is to be run on an intel 386 in protected mode.  `Huge'
   pointers common on MSDOS compilers do not work in protected mode.
   PROT386 is required if scm is to run as part of a Microsoft Windows
   application.  Added by Stephen Adams 8 May 92 */

/* #define PROT386 */

/* #define NON_PREEMPTIVE if you are using an non-preemptive operating
   system in which periodic polling for interrupts is necessary.
   Provide your own main procedure (e.g., WinMain, in Windows) or
   modify "scmmain.c".  Define and initialize unsigned int poll_count,
   and provide a procedure named poll_routine(), which POLL calls each
   time poll_count reaches zero.  poll_routine() must reinitialize
   poll_count.  It may also check for external actions, such as
   Windows messages.  The value assigned to poll_count can be quite
   large, e.g., 1000, while still maintaining good response time. */

/* #define CAREFUL_INTS */

/* Define MACRO if you want C level support for hygienic and referentially
   transparent macros. */

#define MACRO

/* STDC_HEADERS indicates that the include file names are the same as
   ANSI C.  For most modern systems this is the case. */

/* added by Yasuaki Honda */
#ifdef THINK_C
# define __STDC__
# ifndef macintosh
#  define macintosh
# endif
#endif

/* added by Bob Schumaker, cobblers@netcom.com */
#ifdef __MWERKS__
# ifndef macintosh
#  define macintosh
# endif
# define bzero(p, n)	memset(p, 0, n)
# define bcopy			memcpy
#endif
/* added by Denys Duchier */
#ifndef SVR4
# ifdef __SVR4
#  define SVR4
# endif
#endif

#ifdef __STDC__
# ifndef __HIGHC__		/* overly fussy compiler */
#  define USE_ANSI_PROTOTYPES
# endif
# ifndef __GNUC__
#  define STDC_HEADERS
# else
#  ifdef sparc
#   ifdef SVR4
#    define STDC_HEADERS
#   endif
#  else
#   ifndef tahoe
#    ifndef sun
#     define STDC_HEADERS
#    endif
#   endif
#  endif
# endif
#endif
#ifdef __alpha
# define SHORT_INT
#endif
#ifdef MSDOS			/* Microsoft C 5.10 and 6.00A */
# ifndef GO32
#  define SHORT_INT
#  define SHORT_SIZET
# endif
#endif
#ifdef _QC
# define SHORT_INT
# define SHORT_SIZET
#endif
#ifdef __TURBOC__
# define SHORT_INT
# define SHORT_SIZET
# define LACK_SBRK
# ifndef __TOS__
#  define MSDOS
# endif
#endif
#ifdef __HIGHC__
# define LACK_SBRK
#endif
#ifdef _WIN32
# define MSDOS
# define LACK_SBRK
# define LACK_TIMES
#endif
#ifdef _MSDOS
# define MSDOS
#endif
#ifdef MSDOS
# define STDC_HEADERS
#endif
#ifdef vms
# define STDC_HEADERS
#endif
#ifdef nosve
# define STDC_HEADERS
#endif

#ifdef linux
# define HAVE_SELECT
# define HAVE_SYS_TIME_H
# define STDC_HEADERS
#endif

#ifdef _UNICOS
# define STDC_HEADERS
#endif

#ifdef _AIX
# define _POSIX_SOURCE
# define LACK_FTIME
#endif

#ifdef __sgi__
# define LACK_FTIME
# define STDC_HEADERS
# define USE_ANSI_PROTOTYPES
# define HAVE_SYS_TIME_H
# define __SVR4
#endif

#ifdef __SVR4
# define HAVE_SELECT
#endif

#ifdef PLAN9
# define STDC_HEADERS
#endif

#ifdef hpux
# define LACK_E_IDs
#endif

/* C-Set++ for OS/2 */
#ifdef __IBMC__
# define STDC_HEADERS
# define LACK_TIMES
# define LACK_SBRK
#endif

#ifdef __CYGWIN32__
# define LACK_FTIME
# define HAVE_SELECT
# define HAVE_SYS_TIME_H
# undef MSDOS
#endif

#ifdef __amigaos__
# define HAVE_SELECT
# define HAVE_SYS_TIME_H
# define LACK_SBRK
#endif

/* PROMPT is the prompt string printed at top level */

#ifndef PROMPT
# ifdef SICP
#  define PROMPT "==> "
# else
#  define PROMPT "> "
# endif
#endif

/* #define BRACKETS_AS_PARENS to have [ and ] be read as ( and ) in forms. */

/* #define BRACKETS_AS_PARENS */

/* LINE_INCREMENTORS are the characters which cause the line count to
   be incremented for the purposes of error reporting.  This feature
   is only used for scheme code loaded from files.

   WHITE_SPACES are other characters which should be treated like spaces
   in programs.  in both cases sparate characters with ":case " */

#define LINE_INCREMENTORS  '\n'
#ifdef MSDOS
# define WHITE_SPACES  ' ':case '\t':case '\r':case '\f':case 26
#else
# define WHITE_SPACES  ' ':case '\t':case '\r':case '\f'
#endif

/* Define BIGDIG to an integer type whose size is smaller than long if
   you want bignums.  BIGRAD is one greater than the biggest BIGDIG. */
/* Define DIGSTOOBIG if the digits equivalent to a long won't fit in a long. */
#ifdef BIGNUMS
# ifdef _UNICOS
#  define DIGSTOOBIG
#  if (1L << 31) <= USHRT_MAX
#   define BIGDIG unsigned short
#  else
#   define BIGDIG unsigned int
#  endif
#  define BITSPERDIG 32
# else
#  define BIGDIG unsigned short
#  define BITSPERDIG (sizeof(BIGDIG)*CHAR_BIT)
# endif
# define BIGRAD (1L << BITSPERDIG)
# define DIGSPERLONG ((sizet)((sizeof(long)*CHAR_BIT+BITSPERDIG-1)/BITSPERDIG))
# define BIGUP(x) ((unsigned long)(x) << BITSPERDIG)
# define BIGDN(x) ((x) >> BITSPERDIG)
# define BIGLO(x) ((x) & (BIGRAD-1))
/* NUMDIGS_MAX is the maximum number of digits for BIGNUMS */
# ifndef NUMDIGS_MAX
#  define NUMDIGS_MAX 1000
# endif
#endif

#ifndef BIGDIG
# ifndef FLOATS
#  define INUMS_ONLY
# endif
#endif

#ifdef __GNUC__
# define FENCE asm volatile ("")
#else
# define FENCE /**/
#endif

#ifdef NON_PREEMPTIVE
# define VERIFY_INTS(s1, s2) /**/
# define DEFER_INTS /**/
# ifdef TICKS
#  define POLL {if (0==poll_count--) poll_routine(); \
	      if (0==tick_count--) tick_signal();}
# else
#  define POLL {if (0==poll_count--) poll_routine();}
# endif
# define CHECK_INTS POLL
# define ALLOW_INTS POLL
# define DEFER_INTS_EGC /**/
# define ALLOW_INTS_EGC /**/
#else
# ifdef CAREFUL_INTS
typedef struct {char *fname; int linum;} ints_infot;
extern ints_infot *ints_info;
#  define VERIFY_INTS(s1, s2) {if (!ints_disabled)\
     ints_warn(s1, s2, __FILE__, __LINE__); }
#  define DEFER_INTS \
   {static ints_infot info = {__FILE__, __LINE__};\
     FENCE;if (1==ints_disabled) ints_viol(&info, 1);\
	   else {ints_info = &info; ints_disabled = 1;FENCE;}}
#  define ALLOW_INTS \
   {static ints_infot info = {__FILE__, __LINE__};\
    FENCE;if (1!=ints_disabled) ints_viol(&info, 0);\
	  else {ints_info = &info; ints_disabled = 0;FENCE;CHECK_INTS}}
#  define DEFER_INTS_EGC \
   {static ints_infot info = {__FILE__, __LINE__};\
    FENCE;if (1==ints_disabled) ints_viol(&info, 1);\
	  else {ints_info = &info; ints_disabled = 2;FENCE;}}
#  define ALLOW_INTS_EGC \
   {static ints_infot info = {__FILE__, __LINE__};\
    FENCE;if (1==ints_disabled) ints_viol(&info, 0);\
	  else {ints_info = &info; ints_disabled = 0;FENCE;CHECK_INTS}}
# else
#  define VERIFY_INTS(s1, s2) /**/
#  define DEFER_INTS {FENCE;ints_disabled = 1;FENCE;}
#  define ALLOW_INTS {FENCE;ints_disabled = 0;FENCE;CHECK_INTS}
#  define DEFER_INTS_EGC {FENCE;ints_disabled = 2;FENCE;}
#  define ALLOW_INTS_EGC {FENCE;ints_disabled = 0;FENCE;CHECK_INTS}
# endif
# ifdef TICKS
#  define CHECK_INTS {if (deferred_proc) (*deferred_proc)(); POLL;}
#  define POLL {if (0==tick_count--) tick_signal();}
# else
#  define CHECK_INTS {if (deferred_proc) (*deferred_proc)();}
#  define POLL /**/
# endif
#endif

#ifdef STACK_LIMIT
# define CHECK_STACK {stack_check();}
#else
# define CHECK_STACK /**/
#endif

/* Cray machines have pointers that are incremented once for each word,
   rather than each byte, the 3 most significant bits encode the byte
   within the word.  The following macros deal with this by storing the
   native Cray pointers like the ones that looks like scm expects.  This
   is done for any pointers that might appear in the car of a cell, pointers
   to vector elts, functions, &c are not munged.  */
#ifdef _UNICOS
# define SCM2PTR(x) ((int)(x) >> 3)
# define PTR2SCM(x) (((SCM)(x)) << 3)
# define POINTERS_MUNGED
#else
# ifdef TEST_SCM2PTR
#  define SCM2PTR(x) ((x) ^ 0xf0L)
#  define PTR2SCM(x) (((SCM)(x)) ^ 0xf0L)
#  define POINTERS_MUNGED
# else
#  define SCM2PTR(x) (x)
#  define PTR2SCM(x) ((SCM)(x))
# endif
#endif

/* FIXABLE is non-null if its long argument can be encoded in an INUM. */

#define POSFIXABLE(n) ((n) <= MOST_POSITIVE_FIXNUM)
#define NEGFIXABLE(n) ((n) >= MOST_NEGATIVE_FIXNUM)
#define UNEGFIXABLE(n) ((n) <= -MOST_NEGATIVE_FIXNUM)
#define FIXABLE(n) (POSFIXABLE(n) && NEGFIXABLE(n))

/* The following 8 definitions are defined automatically by the C
   pre-processor.  You will need to override these if you are
   cross-compiling or if the C pre-processor has different properties
   than the compiler. */

#if (((-1)%2==-1) && ((-1)%(-2)==-1) && (1%2==1) && (1%(-2)==1))
#else
# define BADIVSGNS
#endif

/* SRS is signed right shift */
/*--- Turbo C++ v1.0 has a bug with right shifts of signed longs!
      It is believed to be fixed in Turbo C++ v1.01		---*/
#if (-1==(((-1)<<2)+2)>>2) && (__TURBOC__ != 0x295)
# define SRS(x, y) ((x)>>y)
# ifdef __TURBOC__
#  define INUM(x) (((x)>>1)>>1)
# else
#  define INUM(x) SRS(x, 2)
# endif
#else
# define SRS(x, y) (((x)<0) ? ~((~(x))>>y) : (x)>>y)
# define INUM(x) SRS(x, 2)
#endif

#ifdef __TURBOC__
/* shifts of more than one are done by a library call, single shifts are
   performed in registers */
# define MAKINUM(x) ((((x)<<1)<<1)+2L)
#else
# define MAKINUM(x) (((x)<<2)+2L)
#endif

#ifdef _DCC
# define ASCII
#else
# if (('\n'=='\025') && (' '=='\100') && ('a'=='\201') && ('A'=='\301'))
#  define EBCDIC
# endif
# if (('\n'=='\012') && (' '=='\040') && ('a'=='\141') && ('A'=='\101'))
#  define ASCII
# endif
#endif

/* CHAR_CODE_LIMIT is the number of distinct characters represented by
   the unsigned char datatype. */
/* MOST_POSITIVE_FIXNUM is the INUM closest to positive infinity. */
/* MOST_NEGATIVE_FIXNUM is the INUM closest to negative infinity. */

#ifdef __STDC__
# define HAVE_LIMITSH
#endif
#ifdef MWC
# define HAVE_LIMITSH
#endif

#ifdef HAVE_LIMITSH
# include <limits.h>
# ifdef UCHAR_MAX
#  define CHAR_CODE_LIMIT (UCHAR_MAX+1L)
# else
#  define CHAR_CODE_LIMIT 256L
# endif
# define MOST_POSITIVE_FIXNUM (LONG_MAX>>2)
# ifdef _UNICOS			/* Stupid cray bug */
#  define MOST_NEGATIVE_FIXNUM ((long)LONG_MIN/4)
# else
#  define MOST_NEGATIVE_FIXNUM SRS((long)LONG_MIN, 2)
# endif				/* UNICOS */
#else
# define CHAR_CODE_LIMIT 256L
# define MOST_POSITIVE_FIXNUM ((long)((unsigned long)~0L>>3))
# if (0 != ~0)
#  define MOST_NEGATIVE_FIXNUM (-MOST_POSITIVE_FIXNUM-1)
# else
#  define MOST_NEGATIVE_FIXNUM (-MOST_POSITIVE_FIXNUM)
# endif
#endif

/* INTBUFLEN is the maximum number of characters neccessary for the
   printed or string representation of an exact number. */

#ifndef CHAR_BIT
# define CHAR_BIT 8
#endif
#ifndef LONG_BIT
# define LONG_BIT (CHAR_BIT*sizeof(long)/sizeof(char))
#endif
#define INTBUFLEN (5+LONG_BIT)

/* FLOBUFLEN is the maximum number of characters neccessary for the
   printed or string representation of an inexact number. */

#ifdef FLOATS
# define FLOBUFLEN (10+2*(sizeof(double)/sizeof(char)*CHAR_BIT*3+9)/10)
#endif /* FLOATS */

/* MAXEXP is the maximum double precision exponent */
/* FLTMAX is less than or equal the largest single precision float */

#ifdef FLOATS
# ifdef STDC_HEADERS
#  ifndef macintosh
#   ifndef PLAN9
#    include <float.h>
#   endif
#  endif
# endif
# ifdef DBL_MAX_10_EXP
#  define MAXEXP DBL_MAX_10_EXP
# else
#  define MAXEXP 308   /* IEEE doubles */
# endif
# ifndef DBL_DIG
#  define DBL_DIG 15
# endif
# ifndef DBL_MAX_EXP
#  define DBL_MAX_EXP 1024
# endif
# ifdef FLT_MAX
#  define FLTMAX FLT_MAX
# else
#  define FLTMAX 1e+23
# endif
#endif

#ifdef unix
# define HAVE_UNIX
#endif
#ifdef __unix__
# define HAVE_UNIX
#endif
#ifdef _IBMR2
# define HAVE_UNIX
# define STDC_HEADERS
#endif

/* Only some machines have pipes */
#ifdef HAVE_UNIX
  /* DJGPP (gcc for i386) defines unix! */
# define HAVE_PIPE
#endif

/* IS_INF tests its floating point number for infiniteness */

#ifndef IS_INF
# define IS_INF(x) ((x)==(x)/2)
#endif

#ifndef macintosh
# ifdef __WINDOWS__		/* there should be a better flag for this. */
#  define PROT386
# endif
#endif

/* PTR_LT defines how to compare two CELLPTRs (which may not be in the
   same array).  CELLPTR is a pointer to a cons cell which may be
   compared or differenced.  SCMPTR is used for stack bounds. */

#if defined(__TURBOC__) && !defined(__TOS__)
# ifdef PROT386
typedef cell *CELLPTR;
typedef SCM *SCMPTR;
#  define PTR_LT(x, y) (((long)(x)) < ((long)(y)))
# else
typedef cell huge *CELLPTR;
typedef SCM  huge *SCMPTR;
#  define PTR_LT(x, y) ((x) < (y))
# endif
#else /* not __TURBOC__ */
typedef cell *CELLPTR;
typedef SCM  *SCMPTR;
# ifdef nosve
#  define PTR_MASK 0xffffffffffff
#  define PTR_LT(x, y) (((int)(x)&PTR_MASK) < ((int)(y)&PTR_MASK))
# else
#  define PTR_LT(x, y) ((x) < (y))
# endif
#endif

#define PTR_GT(x, y) PTR_LT(y, x)
#define PTR_LE(x, y) (!PTR_GT(x, y))
#define PTR_GE(x, y) (!PTR_LT(x, y))

#ifdef STDC_HEADERS
# ifdef PLAN9
#  define sizet long
# else
#  include <stdlib.h>
#  ifdef AMIGA
#   include <stddef.h>
#  endif
#  define sizet size_t
# endif
#else
# ifdef _SIZE_T
#  define sizet size_t
# else
#  define sizet unsigned int
# endif
#endif

#if __BEOS__
#  include <stdlib.h>
#endif

#ifdef macintosh
# include <unistd.h>
#endif

/* On VMS, GNU C's errno.h contains a special hack to get link attributes
   for errno correct for linking with libc. */

#ifndef PLAN9
# include <errno.h>
#endif

/* SYSCALL retries system calls that have been interrupted (EINTR) */
#ifdef vms
# ifndef __GNUC__
#  include <ssdef.h>
#  define SCM_INTERRUPTED(errno) (EVMSERR==errno && \
				  (vaxc$errno>>3)==(SS$_CONTROLC>>3))
# endif
#endif

#ifndef SCM_INTERRUPTED
# ifdef EINTR
#  if (EINTR > 0)
#   define SCM_INTERRUPTED(errno) (EINTR==errno)
#  endif
# endif
#endif

#ifndef SCM_INTERRUPTED
# define SCM_INTERRUPTED(errno) (0)
#endif

#define SYSCALL(line) do{errno = 0;line}while(SCM_INTERRUPTED(errno))

#ifdef EMFILE
# ifdef ENFILE
#  define SCM_NEED_FDS(errno) (EMFILE==errno || ENFILE==errno)
# else
#  define SCM_NEED_FDS(errno) (EMFILE==errno)
# endif
#else
# define SCM_NEED_FDS(errno) (0)
#endif

#define SCM_OPENCALL(line) {int gcs = 0;\
			     while (!0) {errno = 0; if (line) break;\
			     if (0==gcs++ && SCM_NEED_FDS(errno)) \
			       gc_for_open_files();\
			      else if (!SCM_INTERRUPTED(errno)) break;}}

#ifndef MSDOS
# ifdef ARM_ULIB
    extern volatile int errno;
# else
    extern int errno;
# endif
#endif
#ifdef __TURBOC__
# if (__TURBOC__==1)
 /* Needed for TURBOC V1.0 */
 extern int errno;
# endif
#endif

/* EXIT_SUCCESS is the default code to return from SCM if no errors
   were encountered.  EXIT_FAILURE is the default code to return from
   SCM if errors were encountered.  The return code can be explicitly
   specified in a SCM program with (quit <n>). */

#ifndef EXIT_SUCCESS
# ifdef vms
#  define EXIT_SUCCESS 1
# else
#  define EXIT_SUCCESS 0
# endif
#endif
#ifndef EXIT_FAILURE
# ifdef vms
#  define EXIT_FAILURE 2
# else
#  define EXIT_FAILURE 1
# endif
#endif

/* Yasuaki Honda, Bob Schumaker */
/* Think C and Metrowerks lack isascii macro */
#ifdef macintosh
# define isascii(c) ((unsigned)(c) <= 0x7f)
#endif
#ifdef _DCC
# define isascii(c) ((unsigned)(c) <= 0x7f)
#endif

#ifdef __STDC__
# define VOLATILE volatile
#else
# define VOLATILE /**/
#endif

#ifdef _MSC_VER
				// Disable annoying warnings for:
# pragma warning (disable: 4102) // unreferenced label
# pragma warning (disable: 4018) // signed/unsigned mismatch
# pragma warning (disable: 4101) // unreferenced variables
# pragma warning (disable: 4244) // conversion from unsigned long to unsigned short
#endif

/* end of automatic C pre-processor definitions */


#endif
