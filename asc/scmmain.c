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

/* "scmmain.c" main() for SCM.
   Author: Aubrey Jaffer */

/* added by Dai Inukai 2001-03-21*/
#ifdef __FreeBSD__
# include <floatingpoint.h>
#endif

#include "scm.h"
#include "version.h"

#ifdef __IBMC__
# include <io.h>
#endif
#ifdef __OpenBSD__
# include <unistd.h>
#endif
#ifdef __BEOS__
# include <unistd.h>
#endif
#ifndef GENERIC_NAME
# define GENERIC_NAME "asc"
#endif
#ifndef INIT_GETENV
# define INIT_GETENV "ASC_INIT_FILE"
#endif

#ifndef INIT_FILE_NAME
# define INIT_FILE_NAME "init.scm"
#endif

char *scm_find_implpath(execpath)
     char *execpath;
{
  char *implpath = 0;

  char *getenvpath = getenv(INIT_GETENV);
  /*  fprintf(stderr, "%s=%s\n", INIT_GETENV, getenvpath); fflush(stderr);*/
  if (getenvpath) 
    implpath = scm_cat_path(0L, getenvpath, 0L);
  if (implpath) {/* The value of the environment variable supersedes
		    other locations, only if the file exists. */
    implpath = scm_try_path(implpath);
    if (!implpath) {
      fputs("Value of "INIT_GETENV" (=\"", stderr);
      fputs(getenvpath, stderr);
      fputs("\") not found; Trying elsewhere\n", stderr);
    }
  }

  if (!implpath && execpath)
    implpath = find_impl_file(execpath, GENERIC_NAME, INIT_FILE_NAME, dirsep);
  
#ifdef IMPLINIT
  if (!implpath) 
    implpath = scm_cat_path(0L, IMPLINIT, 0L);
#endif
  
  return implpath;
}
char *generic_name[] = { GENERIC_NAME };

int main(argc, argv)
     int argc;
     char **argv;
{
  char *script_arg = 0;		/* location of SCSH style script file or 0. */
  char *implpath = 0, **nargv;
  int nargc, iverbose = 0, buf0stdin;
  SCM retval;

/* added by Dai Inukai 2001-03-21 */
#ifdef __FreeBSD__
  fp_prec_t fpspec;
#endif

/*    {char ** argvv = argv; */
/*    for (;*argvv;argvv++) {fputs(*argvv, stderr); fputs(" ", stderr);} */
/*    fputs("\n", stderr);} */

  /* for macintosh */
  if (0 == argc) {
    argc = 1; 
    argv = generic_name;
  }
  
#ifndef LACK_SBRK
  init_sbrk();			/* Do this before malloc()s. */
#endif
  
  /* added by Dai Inukai 2001-03-21 */
#ifdef __FreeBSD__
  fpspec = fpsetprec(FP_PE); /* IEEE 64 bit FP mantissa*/
#endif

  execpath = 0;			/* even when dumped */
  if ((nargv = script_process_argv(argc, argv))) { /* SCSH style scripts */
    script_arg = argv[2];	/* Save for scm_find_execpath() call */
    nargc = script_count_argv(nargv);
  }
  else {
    nargv = argv; 
    nargc = argc;
  }

  /* execpath must be set to executable's path in order to use DUMP or DLD. */
  execpath = scm_find_execpath(nargc, nargv, script_arg);
  implpath = scm_find_implpath(execpath);

  if (isatty(fileno(stdin)) && isatty(fileno(stdout)))
    iverbose = (nargc <= 1) ? 2 : 1;

  buf0stdin = init_buf0(stdin);
  do {				/* You must call scm_init_from_argv()
				   or init_scm() to initialize SCM */
    scm_init_from_argv(nargc, nargv, script_arg, iverbose, buf0stdin);
    init_signals();		/* signals are optional */
				/* Now we are ready to run Scheme code! */
    retval = scm_top_level(implpath, 0L);
    restore_signals();		/* signals are optional */
				/* final_scm() when you are done with SCM. */
    if (retval) 
      break;

    if (2 <= iverbose) 
      fputs(";RESTART\n", stderr);
    final_scm(!0);
  } while (!0);

  final_scm(
#ifdef CAREFUL_INTS
	    1
#else
	    1 /* freeall || (2 <= verbose) */ /* Free storage when we're done. */
#endif
	    );
  if (2 <= iverbose) 
    fputs(";EXIT\n", stderr);
  fflush(stderr);
  if (implpath) 
    free(implpath);
  if (execpath) 
    free(execpath);
  execpath = 0;

  /* added by Dai Inukai 2001-03-27 */
#ifdef __FreeBSD__
  fpspec = fpsetprec(fpspec); /* Set back to FP_PD which is 53 bit FP. */
                              /* This may not be needed because the    */
                              /* kernel is set to FP_PD by default.    */
#endif

  return (int)INUM(retval);
}

/* init_user_scm() is called by the scheme procedure
   SCM_INIT_EXTENSIONS in "Init5xx.scm" */
void init_user_scm()
{
  /* Put calls to your C initialization routines here. */
}
