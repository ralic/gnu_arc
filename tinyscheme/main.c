#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <strings.h>
#include <scmenv.h>

#include "scheme.h"
#include "scheme-private.h"
#include "fsys.h"
#include "dirport.h"

#define BANNER "Arc 0.0.1"

#define DEFAULT_INIT_FILE "init.scm"

static pointer arc_home(scheme* sc, pointer args)
{
  char* home = getenv("ARC_HOME");
  if (home)
    return mk_string(sc, home);

  return mk_string(sc, ARC_HOME);
}


#if defined(__APPLE__) && !defined (OSX)
int main()
{
     extern MacTS_main(int argc, char **argv);
     char**    argv;
     int argc = ccommand(&argv);
     MacTS_main(argc,argv);
     return 0;
}
int MacTS_main(int argc, char **argv) {
#else
int main(int argc, char **argv)
{
#endif
  scheme sc;
  FILE *fin;
  char *file_name=NULL;
  int retcode;
  int isfile = 1;
  int run_arc = 1;
  char init_file_path[PATH_MAX];
  char ascinit_file_path[PATH_MAX];

  char* ascinitdir = getenv("ASC_INIT_DIR");

  if (ascinitdir) {
    strcpy(init_file_path, ascinitdir);
  }
  else {
    strcpy(init_file_path, ARC_HOME);
  }
  strcat(init_file_path, "/");
  strcat(init_file_path, DEFAULT_INIT_FILE);

  if (ascinitdir) {
    strcpy(ascinit_file_path, ascinitdir);
  }
  else {
    strcpy(ascinit_file_path, ARC_HOME);
  }
  strcat(ascinit_file_path, "/");
  strcat(ascinit_file_path, "arc-init.scm");
  printf("-> %s\n", ascinit_file_path);

  if (argc == 1) {
    printf(BANNER);
  }

  if (argc >= 2) {
    if (strcmp(argv[1], "-h") == 0) {
      printf("Usage: %s -h\n", argv[0]);
      printf("       %s -s <file> <arg1> <arg2> ...\n", argv[0]);
      printf("       %s <command>\n", argv[0]);
      return 1;
    }
    else if (strcmp(argv[1], "-s") == 0) {
      run_arc = 0;
      /* ignore the -s option */
      argv++;
    }
  }

  /* ignore program name */
  argv++;


  if (!scheme_init(&sc)) {
    fprintf(stderr,"Could not initialize!\n");
    return 2;
  }

  scheme_set_input_port_file(&sc, stdin);
  scheme_set_output_port_file(&sc, stdout);


#if USE_DL
  scheme_define(&sc, sc.global_env, mk_symbol(&sc, "load-extension"),
                mk_foreign_func(&sc, scm_load_ext));
#endif


#define SCHEME_DEFINE_FUNC(_nm, _proc)                      \
  scheme_define(&sc, sc.global_env, mk_symbol(&sc, (_nm)),  \
                mk_foreign_func(&sc, (_proc)))

  SCHEME_DEFINE_FUNC("arc:home",             arc_home);

  SCHEME_DEFINE_FUNC("sys:opendir",          dirport_opendir);
  SCHEME_DEFINE_FUNC("sys:readdir",          dirport_readdir);
  SCHEME_DEFINE_FUNC("sys:closedir",         dirport_closedir);
  SCHEME_DEFINE_FUNC("sys:dirport?",         dirport_is_dirport);

  SCHEME_DEFINE_FUNC("sys:getenv",           scm_getenv);
  SCHEME_DEFINE_FUNC("sys:mkdir",            scm_mkdir);
  SCHEME_DEFINE_FUNC("sys:unlink",           scm_unlink);
  SCHEME_DEFINE_FUNC("sys:rmdir",            scm_rmdir);
  SCHEME_DEFINE_FUNC("sys:symlink",          scm_symlink);
  SCHEME_DEFINE_FUNC("sys:chdir",            scm_chdir);
  SCHEME_DEFINE_FUNC("sys:current-time",     scm_current_time);
  SCHEME_DEFINE_FUNC("sys:file-exists?",     scm_file_existsq);
  SCHEME_DEFINE_FUNC("sys:file-directory?",  scm_file_directoryq);
  SCHEME_DEFINE_FUNC("sys:file-executable?", scm_file_executableq);
  SCHEME_DEFINE_FUNC("sys:tempdir",          scm_tempdir);
  SCHEME_DEFINE_FUNC("sys:homedir",          scm_homedir);
  SCHEME_DEFINE_FUNC("sys:getcwd",           scm_getcwd);
  SCHEME_DEFINE_FUNC("sys:getpid",           scm_getpid);
  SCHEME_DEFINE_FUNC("sys:utime",            scm_utime);
  SCHEME_DEFINE_FUNC("sys:mtime",            scm_mtime);
  SCHEME_DEFINE_FUNC("sys:file-size",        scm_file_size);
  SCHEME_DEFINE_FUNC("sys:stat",             scm_stat);
  SCHEME_DEFINE_FUNC("sys:system",           scm_system);
  SCHEME_DEFINE_FUNC("sys:chmod",            scm_chmod);
  SCHEME_DEFINE_FUNC("sys:copy-file",        scm_copy_file);

  if (run_arc) {
    pointer args = sc.NIL;
    for ( ; *argv; argv++) {
      pointer value = mk_string(&sc, *argv);
      args = cons(&sc, value, args);
    }
    args = reverse_in_place(&sc, sc.NIL, args);
    scheme_define(&sc, sc.global_env, mk_symbol(&sc, "*args*"), args);

    if ((fin = fopen(init_file_path, "r"))) {
      scheme_load_file(&sc, fin);
      fclose(fin);
    }
    
    if ((fin = fopen(ascinit_file_path, "r"))) {
      scheme_load_file(&sc, fin);
      fclose(fin);
    }
  }
  else {
    file_name = init_file_path;

    do {
      if (strcmp(file_name,"-") == 0) {
        fin = stdin;
      }
      else if (strcmp(file_name, "-1") == 0 || strcmp(file_name, "-c") == 0) {
        pointer args = sc.NIL;
        isfile = file_name[1] == '1';
        file_name = *argv++;
        if (strcmp(file_name, "-") == 0) {
          fin = stdin;
        } 
        else if (isfile) {
          fin = fopen(file_name, "r");
        }

        for ( ; *argv; argv++) {
          pointer value = mk_string(&sc, *argv);
          args = cons(&sc, value, args);
        }
        args = reverse_in_place(&sc, sc.NIL, args);
        scheme_define(&sc, sc.global_env, mk_symbol(&sc, "*args*"), args);
      }
      else {
        fin = fopen(file_name, "r");
      }
    
      if (isfile && fin == 0)
      {
        fprintf(stderr,"Could not open file %s\n",file_name);
      }
      else {
        if (isfile) {
          scheme_load_file(&sc,fin);
        }
        else {
          scheme_load_string(&sc,file_name);
        }
        
        if (!isfile || fin != stdin) {
          if (sc.retcode != 0) {
            fprintf(stderr, "Errors encountered reading %s\n", file_name);
          }
          if (isfile) {
            fclose(fin);
          }
        }
      }
      file_name = *argv++;
    } while (file_name != 0);

    if (argc == 1) {
      scheme_load_file(&sc, stdin);
    }
  }

  retcode = sc.retcode;
  scheme_deinit(&sc);
  
  return retcode;
}
