/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.
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

/* "ioext.c" code for system calls in common between PC compilers and unix.
   Author: Aubrey Jaffer */

#include "scm.h"

#ifdef __EMX__
# include <sys/types.h>
#endif

#ifndef macintosh
# ifdef vms
#  include <stat.h>
# else
#  include <sys/stat.h>
# endif

# ifdef __TURBOC__
#  include <io.h>
# endif
SCM	stat2scm P((struct stat *stat_temp));
/* int	mkdir P((const char *path, mode_t mode)); */
#endif
#ifdef hpux
# include <unistd.h>
#endif
#ifdef __sgi__
# include <unistd.h>
#endif
#ifdef __FreeBSD__
# include <unistd.h>
#endif
#ifdef __OpenBSD__
# include <unistd.h>
#endif
/* added by Denys Duchier */
#ifdef __SVR4
# include <sys/types.h>
# include <unistd.h>
#endif
#ifdef linux
# include <unistd.h>
#endif
#ifdef GO32
# include <unistd.h>
#endif
#ifdef __osf__
# include <unistd.h>
#endif
#ifdef __MACH__
# include <unistd.h>
#endif
#if __BEOS__
# include <unistd.h>
#endif
#ifdef __CYGWIN32__
# include <process.h>
# include <unistd.h>
#endif

#ifndef STDC_HEADERS
	int chdir P((const char *path));
	int unlink P((const char *name));
	int link P((const char *from, const char *to));
	char *getcwd P((char *buf, sizet size));
	int access P((const char *name, int type));
	int dup P((int fd));
	int dup2 P((int fd, int fd2));
	int close P((int fd));
	int rmdir P((const char *path));
	int execv P((const char *, char *const *));
	int execvp P((const char *, char *const *));
	int putenv P((const char *));
#else
# ifdef _WIN32
#  include <direct.h>
#  include <io.h>
#  include <process.h>
# endif
# ifdef __HIGHC__
#  include <direct.h>
#  include <dirent.h>
#  include <process.h>
#  define mkdir(foo,bar) mkdir(foo)
# endif
#endif /* STDC_HEADERS */

#ifdef __EMX__
	int execv P((const char *, char *const *));
	int execvp P((const char *, char *const *));
	int putenv P((const char *));
#endif

static char s_read_line[] = "read-line";
SCM read_line(port)
     SCM port;
{
  register int c;
  register int j = 0;
  sizet len = 30;
  SCM tok_buf = makstr((long) len);
  register char *p = CHARS(tok_buf);
  if UNBNDP(port) port = cur_inp;
  else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG1, s_read_line);
  if (EOF==(c = lgetc(port))) return EOF_VAL;
  while(1) {
    switch (c) {
    case LINE_INCREMENTORS:
    case EOF:
      if (j>0 && '\r'==p[j-1]) j--;
      if (len==j) return tok_buf;
      return resizuve(tok_buf, (SCM)MAKINUM(j));
    default:
      if (j >= len) {
	p = grow_tok_buf(tok_buf);
	len = LENGTH(tok_buf);
      }
      p[j++] = c;
      c = lgetc(port);
    }
  }
}
static char s_read_line1[] = "read-line!";
SCM read_line1(str, port)
     SCM str, port;
{
  register int c;
  register int j = 0;
  register char *p;
  sizet len;
  ASSERT(NIMP(str) && STRINGP(str), str, ARG1, s_read_line1);
  p = CHARS(str);
  len = LENGTH(str);
  if UNBNDP(port) port = cur_inp;
  else ASSERT(NIMP(port) && OPINPORTP(port), port, ARG2, s_read_line1);
  c = lgetc(port);
  if (EOF==c) return EOF_VAL;
  while(1) {
    switch (c) {
    case LINE_INCREMENTORS:
    case EOF:
      return MAKINUM(j);
    default:
      if (j >= len) {
	lungetc(c, port);
	return BOOL_F;
      }
      p[j++] = c;
      c = lgetc(port);
    }
  }
}
static char s_write_line[] = "write-line";
SCM l_write_line(obj, port)
     SCM obj, port;
{
  display(obj, port);
  return newline(port);
}

static char	s_file_position[] = "file-position",
		s_file_set_pos[] = "file-set-position";
SCM file_position(port)
     SCM port;
{
	long ans;
	ASSERT(NIMP(port) && OPFPORTP(port), port, ARG1, s_file_position);
	SYSCALL(ans = ftell(STREAM(port)););
	if CRDYP(port) ans--;
	return MAKINUM(ans);
      }
SCM file_set_position(port, pos)
     SCM port, pos;
{
	SCM ans;
	ASSERT(NIMP(port) && OPFPORTP(port), port, ARG1, s_file_set_pos);
#ifndef RECKLESS
	if (TRACKED & SCM_PORTFLAGS(port)) {
	  if (INUM0==pos) {
	    int i = SCM_PORTNUM(port);
	    scm_port_table[i].line = 1L;
	    scm_port_table[i].col = 1;
	  }
	  else {
	    if (2 <= verbose)
	      scm_warn("Setting file position for tracked port: ", "", port);
	    SCM_PORTFLAGS(port) &= (~TRACKED);
	  }
	}
#endif
	CLRDY(port);		/* Clear ungetted char */
	SYSCALL(ans = (fseek(STREAM(port), INUM(pos), 0)) ? BOOL_F : BOOL_T;);
#ifdef HAVE_PIPE
# ifdef ESPIPE
	if (!OPIOPORTP(port))
	  ASSERT(ESPIPE != errno, port, ARG1, s_file_set_pos);
# endif
#endif
	return ans;
}

static char s_reopen_file[] = "reopen-file";
SCM reopen_file(filename, modes, port)
     SCM filename, modes, port;
{
  FILE *f;
  char cmodes[4];
  long flags;
  ASSERT(NIMP(filename) && STRINGP(filename), filename, ARG1, s_reopen_file);
  ASSERT(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_reopen_file);
  flags =  mode_bits(CHARS(modes), cmodes);
  ASSERT(flags, modes, ARG2, s_reopen_file);
  DEFER_INTS;
  ASSERT(NIMP(port) && FPORTP(port) && OPENP(port), port, ARG3, s_reopen_file);
  SCM_OPENCALL((f = freopen(CHARS(filename), cmodes, STREAM(port))));
  if (!f) {
    ALLOW_INTS;
    return BOOL_F;
  }
  else {
    SCM_PORTFLAGS(port) = flags;
    SCM_SETFLAGS(port, flags);
    if (BUF0 & flags)
      i_setbuf0(port);
    SCM_PORTDATA(port) = filename;
  }
  ALLOW_INTS;
  return port;
}

#ifndef MCH_AMIGA
# ifndef macintosh
static char s_dup[]="duplicate-port";
SCM l_dup(oldpt, modes)
     SCM oldpt, modes;
{
  long flags;
  char cmodes[4];
  int tfd;
  FILE *f;
  SCM newpt;
  ASSERT(NIMP(oldpt) && OPFPORTP(oldpt), oldpt, ARG1, s_dup);
  ASSERT(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_dup);
  flags = mode_bits(CHARS(modes), cmodes);
  ASSERT(flags, modes, ARG2, s_dup);
  NEWCELL(newpt);
  DEFER_INTS;
  SCM_OPENCALL((tfd = dup(fileno(STREAM(oldpt)))));
  if (-1==tfd) {ALLOW_INTS;return BOOL_F;};
  SYSCALL(f = fdopen(tfd, cmodes););
  if (!f) {
    int lerrno = errno;
    close(tfd);
    errno = lerrno;
#  ifdef EINVAL
    if (lerrno==EINVAL) wta(modes, (char *)ARG2, s_dup);
#  endif
    wta(MAKINUM(tfd), (char *)NALLOC, s_port_type);
  }
  newpt = scm_port_entry(f, tc16_fport, flags);
  SCM_PORTDATA(newpt) = SCM_PORTDATA(oldpt);
  if (BUF0 & flags)
    i_setbuf0(newpt);
  ALLOW_INTS;
  return newpt;
}
static char s_dup2[]="redirect-port!";
SCM l_dup2(into_pt, from_pt)
     SCM into_pt, from_pt;
{
  int ans, oldfd, newfd;
  DEFER_INTS;
  ASSERT(NIMP(into_pt) && OPFPORTP(into_pt), into_pt, ARG1, s_dup2);
  ASSERT(NIMP(from_pt) && OPFPORTP(from_pt), from_pt, ARG1, s_dup2);
  oldfd = fileno(STREAM(into_pt));
  newfd = fileno(STREAM(from_pt));
  SCM_OPENCALL((ans = dup2(oldfd, newfd)));
  if (-1==ans) {ALLOW_INTS;return BOOL_F;};
  ALLOW_INTS;
  return into_pt;
}
# endif

# ifndef vms
#  ifndef _WIN32
#   include <dirent.h>
static char s_opendir[]="opendir";
SCM l_opendir(dirname)
     SCM dirname;
{
  DIR *ds;
  SCM dir;
  ASSERT(NIMP(dirname) && STRINGP(dirname), dirname, ARG1, s_opendir);
  NEWCELL(dir);
  DEFER_INTS;
  SCM_OPENCALL((ds = opendir(CHARS(dirname))));
  if (!ds) {ALLOW_INTS; return BOOL_F;}
  CAR(dir) = tc16_dir | OPN;
  SETCDR(dir, ds);
  ALLOW_INTS;
  return dir;
}
static char s_readdir[]="readdir";
SCM l_readdir(port)
     SCM port;
{
  struct dirent *rdent;
  DEFER_INTS;
  ASSERT(OPDIRP(port), port, ARG1, s_readdir);
  SYSCALL(rdent = readdir((DIR *)CDR(port)););
  if (!rdent) {ALLOW_INTS; return BOOL_F;}
  ALLOW_INTS;
  /* rdent could be overwritten by another readdir to the same handle */
  return makfrom0str((char *)rdent->d_name);
}
static char s_rewinddir[]="rewinddir";
SCM l_rewinddir(port)
     SCM port;
{
  ASSERT(OPDIRP(port), port, ARG1, s_rewinddir);
  rewinddir((DIR *)CDR(port));
  return UNSPECIFIED;
}
static char s_closedir[]="closedir";
SCM l_closedir(port)
     SCM port;
{
  int sts;
  ASSERT(DIRP(port), port, ARG1, s_closedir);
  DEFER_INTS;
  if CLOSEDP(port) {ALLOW_INTS;return BOOL_F;}
  SYSCALL(sts = closedir((DIR *)CDR(port)););
  if (sts) {ALLOW_INTS; return BOOL_F;}
  CAR(port) = tc16_dir;
  ALLOW_INTS;
  return BOOL_T;
}

int dir_print(sexp, port, writing)
     SCM sexp; SCM port; int writing;
{
  prinport(sexp, port, "directory");
  return !0;
}
sizet dir_free(p)
     CELLPTR p;
{
  if OPENP((SCM)p) closedir((DIR *)CDR((SCM)p));
  return 0;
}

long tc16_dir;
static smobfuns dir_smob = {mark0, dir_free, dir_print, 0};
#  endif /* _WIN32 */
# endif /* vms */

static char s_mkdir[] = "mkdir";
SCM l_mkdir(path, mode)
     SCM path, mode;
{
  int val;
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_mkdir);
  ASSERT(INUMP(mode), mode, ARG2, s_mkdir);
# ifdef _WIN32
  SYSCALL(val = mkdir(CHARS(path)););
# else
  SYSCALL(val = mkdir(CHARS(path), INUM(mode)););
				/* (mode_t)INUM(mode) might be needed */
# endif
  return val ? BOOL_F : BOOL_T;
}
# ifdef vms
static char s_dot_dir[] = ".DIR";
# endif
static char s_rmdir[] = "rmdir";
SCM l_rmdir(path)
     SCM path;
{
  int val;
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_rmdir);
# ifdef vms
  return del_fil(st_append(cons2(path, s_dot_dir, EOL)));
# else
  SYSCALL(val = rmdir(CHARS(path)););
  return val ? BOOL_F : BOOL_T;
# endif
}
#endif /* MCH_AMIGA */

#ifndef THINK_C
static char s_chdir[] = "chdir";
SCM lchdir(str)
     SCM str;
{
  int ans;
  ASSERT(NIMP(str) && STRINGP(str), str, ARG1, s_chdir);
  SYSCALL(ans = chdir(CHARS(str)););
  return ans ? BOOL_F : BOOL_T;
}
# ifndef MCH_AMIGA
#  ifdef __TURBOC__
#   include <dir.h>
#  endif
SCM l_getcwd()
{
  char *ans;
#  ifndef vms
  char wd[256];
  SYSCALL(ans = getcwd(wd, 256););
  return ans ? makfrom0str(wd) : BOOL_F;
#  else
  SYSCALL(ans = getenv("PATH"););
  return ans ? makfrom0str(ans) : BOOL_F;
#  endif
}

#  ifndef __MWERKS__
static char s_chmod[] = "chmod";
SCM l_chmod(pathname, mode)
     SCM pathname, mode;
{
  int val;
  ASSERT(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_chmod);
  ASSERT(INUMP(mode), mode, ARG2, s_chmod);
  SYSCALL(val = chmod(CHARS(pathname), INUM(mode)););
  return val ? BOOL_F : BOOL_T;
}
#  endif

#  ifndef vms
#   ifdef __EMX__
#    include <sys/utime.h>
#   else
#    ifdef _WIN32
#     include <sys/utime.h>
#    else
#     include <utime.h>
#    endif
#   endif
static char s_utime[] = "utime";
SCM l_utime(pathname, acctime, modtime)
     SCM pathname, acctime, modtime;
{
  int val;
  struct utimbuf utm_tmp;
  utm_tmp.actime = num2ulong(acctime, (char *)ARG2, s_utime);
  utm_tmp.modtime = num2ulong(modtime, (char *)ARG3, s_utime);
  ASSERT(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_utime);
  SYSCALL(val = utime(CHARS(pathname), &utm_tmp););
  return val ? BOOL_F : BOOL_T;
}
#  endif /* vms */

#  ifndef __MWERKS__
static char s_umask[] = "umask";
SCM l_umask(mode)
     SCM mode;
{
  ASSERT(INUMP(mode), mode, ARG1, s_umask);
  return MAKINUM(umask(INUM(mode)));
}
#  endif
# endif /* MCH_AMIGA */
#endif /* THINK_C */

static char s_ren_fil[] = "rename-file";
SCM ren_fil(oldname, newname)
     SCM oldname, newname;
{
  SCM ans;
  ASSERT(NIMP(oldname) && STRINGP(oldname), oldname, ARG1, s_ren_fil);
  ASSERT(NIMP(newname) && STRINGP(newname), newname, ARG2, s_ren_fil);
#if 1 /* def STDC_HEADERS */
  SYSCALL(ans = (rename(CHARS(oldname), CHARS(newname))) ? BOOL_F: BOOL_T;);
  return ans;
#else
  DEFER_INTS;
  SYSCALL(ans = link(CHARS(oldname), CHARS(newname)) ? BOOL_F : BOOL_T;);
  if (!FALSEP(ans)) {
    SYSCALL(ans = unlink(CHARS(oldname)) ? BOOL_F : BOOL_T;);
    if FALSEP(ans)
      SYSCALL(unlink(CHARS(newname));); /* unlink failed.  remove new name */
  }
  ALLOW_INTS;
  return ans;
#endif
}
static char s_fileno[] = "fileno";
SCM l_fileno(port)
     SCM port;
{
  ASSERT(NIMP(port) && OPPORTP(port), port, ARG1, s_fileno);
  if (tc16_fport != TYP16(port)) return BOOL_F;
  return MAKINUM(fileno(STREAM(port)));
}
#ifndef THINK_C
# ifndef __MWERKS__
#  ifndef F_OK
#   define F_OK 00
#   define X_OK 01
#   define W_OK 02
#   define R_OK 04
#  endif
static char s_access[] = "access";
SCM l_access(pathname, mode)
     SCM pathname, mode;
{
  int val;
  int imodes;
  ASSERT(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_access);
  if INUMP(mode) imodes = INUM(mode);
  else {
    ASSERT(NIMP(mode) && STRINGP(mode), mode, ARG2, s_access);
    imodes = F_OK | (strchr(CHARS(mode), 'r') ? R_OK : 0)
      | (strchr(CHARS(mode), 'w') ? W_OK : 0)
	| (strchr(CHARS(mode), 'x') ? X_OK : 0);
  }
  SYSCALL(val = access(CHARS(pathname), imodes););
  return val ? BOOL_F : BOOL_T;
}
# endif /* __MWERKS__ */

SCM stat2scm P((struct stat *stat_temp));

char s_stat[] = "stat";
SCM l_stat(str)
  SCM str;
{
  int i;
  struct stat stat_temp;
  if IMP(str)
  badarg1: wta(str, (char *)ARG1, s_stat);
  if STRINGP(str) {SYSCALL(i = stat(CHARS(str), &stat_temp););}
  else {
# ifndef MCH_AMIGA
    if (!OPFPORTP(str)) goto badarg1;
    SYSCALL(i = fstat(fileno(STREAM(str)), &stat_temp););
# else
    goto badarg1;
# endif
  }
  if (i) return BOOL_F;
  return stat2scm(&stat_temp);
}
# ifdef MCH_AMIGA
SCM stat2scm(stat_temp)
     struct stat *stat_temp;
{
  SCM ans = make_vector(MAKINUM(3), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  ve[ 0] = ulong2num((unsigned long)stat_temp->st_attr);
  ve[ 1] = ulong2num((unsigned long)stat_temp->st_mtime);
  ve[ 2] = ulong2num((unsigned long)stat_temp->st_size);
  return ans;
}
# else
SCM stat2scm(stat_temp)
     struct stat *stat_temp;
{
  SCM ans = make_vector(MAKINUM(11), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  ve[ 0] = ulong2num((unsigned long)stat_temp->st_dev);
  ve[ 1] = ulong2num((unsigned long)stat_temp->st_ino);
  ve[ 2] = ulong2num((unsigned long)stat_temp->st_mode);
  ve[ 3] = ulong2num((unsigned long)stat_temp->st_nlink);
  ve[ 4] = ulong2num((unsigned long)stat_temp->st_uid);
  ve[ 5] = ulong2num((unsigned long)stat_temp->st_gid);
  ve[ 6] = ulong2num((unsigned long)stat_temp->st_rdev);
  ve[ 7] = ulong2num((unsigned long)stat_temp->st_size);
  ve[ 8] = ulong2num((unsigned long)stat_temp->st_atime);
  ve[ 9] = ulong2num((unsigned long)stat_temp->st_mtime);
  ve[10] = ulong2num((unsigned long)stat_temp->st_ctime);
  return ans;
}
#  ifdef __TURBOC__
#   include <process.h>
#  endif
SCM l_getpid()
{
  return MAKINUM((unsigned long)getpid());
}
# endif /* MCH_AMIGA */
#endif /* THINK_C */

#ifndef __IBMC__
# ifndef macintosh
#  ifndef __WATCOMC__
#   ifndef _Windows
#    ifdef __TURBOC__
#     include <process.h>
#    endif
char s_execv[] = "execv";
char s_execvp[] = "execvp";
SCM i_execv(modes, path, args)
     char * modes;
     SCM path, args;
{
  char **execargv;
  int i = ilength(args);
  ASSERT(i>0, args, WNA, s_execv);
  ASSERT(NIMP(path) && STRINGP(path), path, ARG1, s_execv);
  /*  dowinds(EOL); */
  args = cons(path, args);
  DEFER_INTS;
  execargv = makargvfrmstrs(args, s_execv);
  ignore_signals();
  if (strchr(modes, 'p'))
    execvp(execargv[0], &execargv[1]);
  else
    execv(execargv[0], &execargv[1]);
  unignore_signals();
  ALLOW_INTS;
  perror(execargv[0]);
  return MAKINUM(errno);
}
SCM lexec(path, arg0, args)
     SCM path, arg0, args;
{
  return i_execv("", path, cons(arg0, args));
}
SCM lexecp(path, arg0, args)
     SCM path, arg0, args;
{
  return i_execv("p", path, cons(arg0, args));
}
SCM lexecv(path, args)
     SCM path, args;
{
  return i_execv("", path, args);
}
SCM lexecvp(path, args)
     SCM path, args;
{
  return i_execv("p", path, args);
}
static char s_putenv[] = "putenv";
SCM l_putenv(str)
     SCM str;
{
  ASSERT(NIMP(str) && STRINGP(str), str, ARG1, s_putenv);
  return putenv(CHARS(str)) ? BOOL_F : BOOL_T;
}
#   endif
#  endif
# endif
#endif

static iproc subr1s[] = {
	{s_file_position, file_position},
	{s_fileno, l_fileno},
#ifndef MCH_AMIGA
# ifndef vms
#  ifndef _WIN32
	{s_opendir, l_opendir},
	{s_readdir, l_readdir},
	{s_rewinddir, l_rewinddir},
	{s_closedir, l_closedir},
#  endif
# endif
	{s_rmdir, l_rmdir},
#endif
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef __MWERKS__
	{s_umask, l_umask},
#  endif
# endif
	{s_stat, l_stat},
	{s_chdir, lchdir},
#endif
	{0, 0}};

static iproc subr1os[] = {
	{s_read_line, read_line},
	{0, 0}};

static iproc subr2s[] = {
	{s_ren_fil, ren_fil},
#ifndef macintosh
	{s_access, l_access},
#endif
#ifndef MCH_AMIGA
	{s_mkdir, l_mkdir},
# ifndef macintosh
	{s_dup, l_dup},
	{s_dup2, l_dup2},
	{s_chmod, l_chmod},
# endif
#endif
	{0, 0}};

#include <fcntl.h>		/* for O_RDONLY, O_RDWR, O_EXCL */
#ifdef O_EXCL
SCM scm_try_create_file(fname, modes, perms)
     SCM fname, modes, perms;
{
  SCM port;
  FILE *f;
  char cmodes[4];
  long flags;
  int fd, fdflags = O_CREAT | O_EXCL;
# ifdef S_IROTH
  mode_t cperms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
# else
  int cperms = 0666;
# endif
  ASSERT(NIMP(fname) && STRINGP(fname), fname, ARG1, s_try_create_file);
  ASSERT(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_try_create_file);
  if (NNULLP(perms)) {
    perms = CAR(perms);
    ASSERT(INUMP(perms), perms, ARG3, s_try_create_file);
# ifdef S_IROTH
    cperms = (mode_t)INUM(perms);
# else
    cperms = INUM(perms);
# endif
  }
  flags = mode_bits(CHARS(modes), cmodes);
  ASSERT(flags, modes, ARG2, s_try_create_file);
  fdflags |= (RDNG & flags) ? O_RDWR : O_WRONLY;
  DEFER_INTS;
  SCM_OPENCALL((fd = open(CHARS(fname), fdflags, cperms)));
  if (fd >= 0 && (f = fdopen(fd, cmodes))) {
    port = scm_port_entry(f, tc16_fport, flags);
    if (BUF0 & flags) i_setbuf0(port);
    SCM_PORTDATA(port) = fname;
  }
  else
    port = BOOL_F;
  ALLOW_INTS;
  return port;
}
#endif

static iproc subr2os[] = {
	{s_file_set_pos, file_set_position},
	{s_read_line1, read_line1},
	{s_write_line, l_write_line},
	{0, 0}};

void init_ioext()
{
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr2os, tc7_subr_2o);
	init_iprocs(subr2s, tc7_subr_2);
#ifdef O_EXCL
	make_subr(s_try_create_file, tc7_lsubr_2, scm_try_create_file);
#endif
	make_subr(s_reopen_file, tc7_subr_3, reopen_file);
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef __MWERKS__
	make_subr("getpid", tc7_subr_0, l_getpid);
#  endif
	make_subr("getcwd", tc7_subr_0, l_getcwd);
#  ifndef vms
#   ifndef _WIN32
	make_subr(s_utime, tc7_subr_3, l_utime);
	tc16_dir = newsmob(&dir_smob);
#   endif
#  endif
# endif
#endif
#ifndef __IBMC__
# ifndef macintosh
#  ifndef __WATCOMC__
#   ifndef _Windows
	make_subr(s_execv, tc7_subr_2, lexecv);
	make_subr(s_execvp, tc7_subr_2, lexecvp);
	make_subr("execl", tc7_lsubr_2, lexec);
	make_subr("execlp", tc7_lsubr_2, lexecp);
	make_subr(s_putenv, tc7_subr_1, l_putenv);
#   endif
#  endif
# endif
#endif
	add_feature("i/o-extensions");
	add_feature("line-i/o");
	scm_ldstr("\n\
(define (file-exists? path) (access path \"r\"))\n\
(define (directory-for-each proc dirname . args)\n\
  (define dir (opendir (if (symbol? dirname)\n\
			   (symbol->string dirname)\n\
			   dirname)))\n\
  (if dir\n\
      (let ((selector\n\
	     (cond ((null? args) identity)\n\
		   ((> (length args) 1)\n\
		    (asc:error 'directory-for-each\n\
				'too-many-arguments\n\
				(cdr args)))\n\
		   ((procedure? (car args)) (car args))\n\
		   ((string? (car args))\n\
		    (require 'glob)\n\
		    (filename:match?? (car args)))\n\
		   (else (asc:error 'directory-for-each\n\
				     'unknown-selector-type\n\
				     (car args))))))\n\
	(do ((filename (readdir dir) (readdir dir)))\n\
	    ((not filename) (closedir dir))\n\
	  (and (selector filename) (proc filename))))))\n\
");
	add_feature("directory-for-each");
}
