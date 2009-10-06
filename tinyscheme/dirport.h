#ifndef dirport_h
#define dirport_h

pointer dirport_is_dirport(scheme* sc, pointer args);
pointer dirport_opendir(scheme *sc, pointer args);
pointer dirport_readdir(scheme *sc, pointer args);
pointer dirport_closedir(scheme *sc, pointer args);

#endif
