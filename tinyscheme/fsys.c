#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "sys/stat.h"
#include <stdio.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/wait.h>

#include "scheme-private.h"

#if defined(OSX)
#include <CoreServices/CoreServices.h>

static pointer scm_fsref_to_path(scheme* sc, FSRef* ref)
{
  CFURLRef url = CFURLCreateFromFSRef(NULL, ref);

  if (url) {
    pointer result = sc->NIL;
    CFStringRef str = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
    
    if (str) {
      char path[PATH_MAX];
      int len = CFStringGetLength(str);
      if (CFStringGetCString(str, path, PATH_MAX, kCFStringEncodingUTF8))
        result = mk_string(sc, path);
      
      CFRelease(str);
    }
    
    CFRelease(url);
    
    return result;
  }

  return sc->NIL;
}
#endif

/* (sys:getenv key) => string */
pointer scm_getenv(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    
    char* value = getenv(name);
    if (value) {
      return mk_string(sc, value);
    }
  }
  
  return sc->F;
}


/* (sys:mkdir path permission) => bool */
pointer scm_mkdir(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    unsigned int perm = 0755;

    if (pair_cdr(args) != sc->NIL && is_number(pair_car(pair_cdr(args))))
      perm = (unsigned int)ivalue(pair_car(pair_cdr(args)));

    if (mkdir(name, perm) == 0)
      return sc->T;
  }

  return sc->F;
}


/* (sys:unlink path) => bool */
pointer scm_unlink(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);

    if (unlink(name) == 0)
      return sc->T;
  }

  return sc->F;
}


/* (sys:rmdir path) => bool */
pointer scm_rmdir(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);

    if (rmdir(name) == 0)
      return sc->T;
  }

  return sc->F;
}

  
/* (sys:symlink from to) */
pointer scm_symlink(scheme* sc, pointer args)
{
  pointer first_arg;
  pointer sec_arg;
  
  if ((args != sc->NIL) && pair_cdr(args) != sc->NIL &&
      is_string((first_arg = pair_car(args))) &&
      is_string((sec_arg = pair_car(pair_cdr(args)))) ) {
    char* from = string_value(first_arg);
    char* to = string_value(sec_arg);

    if (symlink(from, to) == 0)
      return sc->T;
  }

  return sc->F;
}


/* (sys:chdir path) => bool */
pointer scm_chdir(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);

    if (chdir(name) == 0)
      return sc->T;
  }

  return sc->F;
}


/* (sys:file-exists? path) => bool */
pointer scm_file_existsq(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0)
      return sc->T;
  }
  
  return sc->F;
}


/* (sys:file-directory? path) => bool */
pointer scm_file_directoryq(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0 && (fattr.st_mode & S_IFMT) == S_IFDIR)
      return sc->T;
  }
  
  return sc->F;
}


/* (sys:file-executable? path) => bool */
pointer scm_file_executableq(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0) {
      if (fattr.st_mode & (S_IEXEC | S_IXGRP | S_IXOTH) != 0)
        return sc->T;
    }

#if defined(OS_WIN)
    {
      int namelen = strlen(name);
      if (namelen > 4) {
        char* suffix = &name[namelen - 4];
        if (strcmp(suffix, ".cmd") == 0 ||
            strcmp(suffix, ".com") == 0 ||
            strcmp(suffix, ".exe") == 0 ||
            strcmp(suffix, ".bat") == 0) 
          return sc->T;
      }
    }
#endif
  }
  
  return sc->F;
}


/* (sys:current_time) => int */
pointer scm_current_time(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if (args == sc->NIL) {
    time_t now = time(NULL);
    
    return mk_integer(sc, (unsigned long)now);
  }
  
  return sc->F;
}


/* (sys:tempdir) => string */
pointer scm_tempdir(scheme* sc, pointer args)
{
#if defined(OSX)
  if (args == sc->NIL) {
    FSRef ref;
    OSErr err = FSFindFolder(kUserDomain, kChewableItemsFolderType, 
                             kCreateFolder, &ref);
    if (err == noErr) {
      return scm_fsref_to_path(sc, &ref);
    }
  }
#else
  pointer first_arg;
  
  if (args == sc->NIL) {
    char* tmppath = getenv("TMPDIR");
    if (tmppath)
      return mk_string(sc, tmppath);

    return mk_string(sc, "/tmp");
  }
#endif

  return sc->F;
}


/* (sys:homedir) => string */
pointer scm_homedir(scheme* sc, pointer args)
{
  if (args == sc->NIL) {
#if defined(OSX)
    FSRef ref;
    OSErr err = FSFindFolder(kUserDomain, kCurrentUserFolderType,
                             kDontCreateFolder, &ref);
    if (err == noErr) {
      return scm_fsref_to_path(sc, &ref);
    }

#elif defined(BEOS)

    char* home = getenv("HOME");
    if (home)
      return mk_string(sc, home);

    return mk_string(sc, "/boot/home");
#else

    char* home = getenv("HOME");
    if (home)
      return mk_string(sc, home);
#endif
  }

  return sc->F;
}


/* (sys:homedir) => string */
pointer scm_getcwd(scheme* sc, pointer args)
{
  if (args == sc->NIL) {
    char cwd[PATH_MAX];
    getcwd(cwd, PATH_MAX);

    return mk_string(sc, cwd);
  }

  return sc->F;
}


/* (sys:opendir dirname) => port */
pointer scm_opendir(scheme *sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    DIR* dir = opendir(name);

    if (dir) {
      port *pt = (port*)sc->malloc(sizeof(port));
      if (!pt)
        return sc->NIL;

      pt->kind = port_dir;
      pt->rep.dir = dir;
      
      return mk_port(sc, pt);
    }
  }

  return sc->F;
}


#define is_dirport(p) (type(p) == T_PORT && p->_object._port->kind & port_dir)

/* (sys:readdir port) => string */
pointer scm_readdir(scheme *sc, pointer args)
{
  pointer pt;
  
  if ((args != sc->NIL) && is_dirport((pt = pair_car(args)))) {
    struct dirent* dp = readdir(pt->_object._port->rep.dir);

    if (dp) {
      return mk_string(sc, dp->d_name);
    }
  }

  return sc->F;
}


/* (sys:closedir port) => string */
pointer scm_closedir(scheme *sc, pointer args)
{
  pointer pt;
  
  if ((args != sc->NIL) && is_dirport((pt = pair_car(args)))) {
    if (closedir(pt->_object._port->rep.dir) == 0) {
      pt->_object._port->kind = port_free;
      return sc->T;
    }
  }

  return sc->F;
}


/* (sys:getpid) => int */
pointer scm_getpid(scheme* sc, pointer args)
{
  if (args == sc->NIL) {
    return mk_integer(sc, getpid());
  }

  return sc->F;
}


/* (sys:utime path [actime [modtime]]) => boolean */
pointer scm_utime(scheme *sc, pointer args)
{
  pointer path_arg;
  pointer actime_arg;
  pointer modtime_arg;

  if ((args != sc->NIL) && is_string((path_arg = pair_car(args)))) {
    if (pair_cdr(args) != sc->NIL && 
        is_number((actime_arg = pair_car(pair_cdr(args))))) {

      if (pair_cdr(pair_cdr(args)) != sc->NIL && 
          is_number((modtime_arg = pair_car(pair_cdr(pair_cdr(args)))))) {
        char* path = string_value(path_arg);
        struct utimbuf buf;

        buf.actime = (time_t)ivalue(actime_arg);
        buf.modtime = (time_t)ivalue(modtime_arg);

        if (utime(path, &buf) == 0)
          return sc->T;
      }
    }
  }

  return sc->F;
}


/* (sys:stat path) => #(dev ino mode nlink uid gid rdev size atime mtime 
                        ctime blksize blocks) ) */
pointer scm_stat(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0) {
      pointer vec = mk_vector(sc, 15);
      set_vector_elem(vec, 0, mk_integer(sc, fattr.st_dev));
      set_vector_elem(vec, 1, mk_integer(sc, fattr.st_ino));
      set_vector_elem(vec, 2, mk_integer(sc, fattr.st_mode));
      set_vector_elem(vec, 3, mk_integer(sc, fattr.st_nlink));
      set_vector_elem(vec, 4, mk_integer(sc, fattr.st_uid));
      set_vector_elem(vec, 5, mk_integer(sc, fattr.st_gid));
      set_vector_elem(vec, 6, mk_integer(sc, fattr.st_rdev));
      set_vector_elem(vec, 7, mk_integer(sc, fattr.st_size));
      set_vector_elem(vec, 8, mk_integer(sc, fattr.st_atime));
      set_vector_elem(vec, 9, mk_integer(sc, fattr.st_mtime));
      set_vector_elem(vec, 10, mk_integer(sc, fattr.st_ctime));
      set_vector_elem(vec, 11, mk_integer(sc, fattr.st_blksize));
      set_vector_elem(vec, 12, mk_integer(sc, fattr.st_blocks));
/*       set_vector_elem(vec, 13, mk_integer(sc, fattr.st_type)); */
/*       set_vector_elem(vec, 14, mk_integer(sc, fattr.st_perms)); */

      return vec;
    }
  }
  
  return sc->F;
}


/* (sys:mtime path) => int ) */
pointer scm_mtime(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0) {
      return mk_integer(sc, fattr.st_mtime);
    }
  }
  
  return sc->F;
}


/* (sys:mtime path) => int ) */
pointer scm_file_size(scheme* sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    struct stat fattr;

    if (stat(name, &fattr) == 0) {
      return mk_integer(sc, fattr.st_size);
    }
  }
  
  return sc->F;
}


/* (sys:system command args) => int) */
pointer scm_system(scheme* sc, pointer args)
{
  pointer cmd_arg;
  pointer args_arg = sc->NIL;

  if ((args != sc->NIL) && is_string((cmd_arg = pair_car(args)))) {
    char* cmd = string_value(cmd_arg);
    int status = 0;
    pid_t pid = 0;
    int veclen = 0;
    int i = 0;
    char** cmd_args = NULL;
    int cmd_args_count = 0;
    int cmdn = 0;

    if (pair_cdr(args) != sc->NIL) {
      if (!is_vector((args_arg = pair_car(pair_cdr(args))))) {
        args_arg = sc->NIL;
      }
    }

    /* at least the command name itself */
    cmd_args_count = 1;

    if (args_arg != sc->NIL) {
      veclen = ivalue(pair_car(args));
      for (i = 0; i < veclen; i++) {
        if (!is_string(vector_elem(args_arg, i)))
          return sc->F;
        
        cmd_args_count++;
      }
    }

    /* +1 for the terminating NULL ptr */
    cmd_args_count++;
    cmd_args = calloc(cmd_args_count, sizeof(char*));
    cmd_args[cmdn++] = cmd;
    
    if (args_arg != sc->NIL) {
      for (i = 0; i < veclen; i++) {
        cmd_args[cmdn++] = string_value(vector_elem(args_arg, i));
      }
    }
    cmd_args[cmdn++] = NULL;
    

    pid = fork();
    if (pid == 0) {
      /* This is the child process.  Execute the shell command. */
      execvp(cmd, cmd_args);
      _exit(EXIT_FAILURE);
    }
    else if (pid < 0) {
      /* The fork failed.  Report failure.  */
      status = -1;
    }
    else {
      /* This is the parent process.  Wait for the child to complete.  */
      if (waitpid(pid, &status, 0) != pid)
        status = -1;
    }

    free(cmd_args);

    return mk_integer(sc, status);
  }

  return sc->F;
}


/* (sys:chmod path mode) => boolean */
pointer scm_chmod(scheme *sc, pointer args)
{
  pointer path_arg;
  pointer mode_arg;

  if ((args != sc->NIL) && is_string((path_arg = pair_car(args)))) {
    if (pair_cdr(args) != sc->NIL) {
      char* path = string_value(path_arg);
      int mode = 0;

      mode_arg = pair_car(pair_cdr(args));
      if (is_number(mode_arg)) {
        mode = ivalue(mode_arg);
      }
      else if (is_symbol(mode_arg)) {
        if (strcmp(symname(mode_arg), "file") == 0)
          mode = 0644;
        else if (strcmp(symname(mode_arg), "exec") == 0)
          mode = 0755;
      }
      else
        return sc->F;

      if (chmod(path, mode) == 0)
        return sc->T;
    }
  }

  return sc->F;
}


#define BUFFER_SIZE 4096

/* (sys:copy-file(from to) => boolean */
pointer scm_copy_file(scheme* sc, pointer args)
{
  pointer src_arg;
  pointer dst_arg;
  FILE* dst = NULL;
  FILE* src = NULL;
  char* buffer = NULL;
  int bytesread = 0;

  if ((args != sc->NIL) && is_string((src_arg = pair_car(args)))) {
    if (pair_cdr(args) != sc->NIL && 
        is_number((dst_arg = pair_car(pair_cdr(args))))) {
      char* src_path = string_value(src_arg);
      char* dst_path = string_value(dst_arg);
      
      src = fopen(src_path, "rb");
      if (!src)
        goto errhd;

      dst = fopen(dst_path, "wb");
      if (!dst)
        goto errhd;
    
      buffer = malloc(BUFFER_SIZE);

      while (1) {
        bytesread = fread(buffer, 1, BUFFER_SIZE, src);
        if (bytesread == 0) {
          if (ferror(src)) {
            free(buffer);
            goto errhd;
          }
          else
            break;
        }

        if (fwrite(buffer, 1, bytesread, dst) != bytesread)
          goto errhd;
      }
    }
  }
  
  free(buffer);

  if (fclose(dst) != 0) {
    dst = NULL;
    goto errhd;
  }
  fclose(src);

  return sc->T;

errhd:
  if (dst)
    fclose(dst);
  if (src)
    fclose(src);

  return sc->F;
}
