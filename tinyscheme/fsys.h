#ifndef fsys_h
#define fsys_h

pointer scm_getenv(scheme* sc, pointer args);
pointer scm_mkdir(scheme* sc, pointer args);
pointer scm_unlink(scheme* sc, pointer args);
pointer scm_rmdir(scheme* sc, pointer args);
pointer scm_symlink(scheme* sc, pointer args);
pointer scm_chdir(scheme* sc, pointer args);
pointer scm_current_time(scheme* sc, pointer args);
pointer scm_file_existsq(scheme* sc, pointer args);
pointer scm_file_directoryq(scheme* sc, pointer args);
pointer scm_file_executableq(scheme* sc, pointer args);
pointer scm_tempdir(scheme* sc, pointer args);
pointer scm_homedir(scheme* sc, pointer args);
pointer scm_getcwd(scheme* sc, pointer args);
pointer scm_getpid(scheme* sc, pointer args);
pointer scm_utime(scheme *sc, pointer args);
pointer scm_mtime(scheme* sc, pointer args);
pointer scm_file_size(scheme* sc, pointer args);
pointer scm_stat(scheme* sc, pointer args);
pointer scm_system(scheme* sc, pointer args);
pointer scm_chmod(scheme *sc, pointer args);
pointer scm_copy_file(scheme* sc, pointer args);


#endif
