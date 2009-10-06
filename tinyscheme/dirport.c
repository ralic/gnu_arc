#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "sys/stat.h"
#include <stdio.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/wait.h>
#include <dirent.h>

#include "scheme-private.h"

static void dirport_finalize_func(scheme* sc, pointer p);
static void dirport_mark_func(scheme* sc, pointer p);
static void dirport_display_func(scheme* sc, char* outstr, pointer p);


typedef struct dirport_extobject dirport_extobject;
struct dirport_extobject
{
  extobject base;
  DIR* dir;
};


static extobject_class dirport_extobject_class = {
  dirport_finalize_func,
  dirport_mark_func,
  dirport_display_func
};


/* (open-dir-port dirname) => port */
pointer dirport_opendir(scheme *sc, pointer args)
{
  pointer first_arg;
  
  if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
    char* name = string_value(first_arg);
    DIR* dir = opendir(name);

    if (dir) {
      dirport_extobject* eo = (dirport_extobject*)sc->malloc(sizeof(dirport_extobject));
      if (!eo) {
        closedir(dir);
        return sc->NIL;
      }

      eo->base.isa = &dirport_extobject_class;
      eo->dir = dir;
      
      return mk_extobject(sc, (extobject*)eo);
    }
  }

  return sc->F;
}


static dirport_extobject* cast_dirport(scheme* sc, pointer p)
{
  if ((p != sc->NIL) && is_extobject(p)) {
    dirport_extobject* eo = (dirport_extobject*)p->_object._ext;
    return ( eo->base.isa == &dirport_extobject_class
             ? eo
             : NULL );
  }

  return NULL;
}


pointer dirport_is_dirport(scheme* sc, pointer args)
{
  if (args != sc->NIL) {
    dirport_extobject* eo = cast_dirport(sc, pair_car(args));

    if (eo)
      return sc->T;
  }
  return sc->F;
}


/* (read-dir-port port) => string */
pointer dirport_readdir(scheme *sc, pointer args)
{
  if (args != sc->NIL) {
    dirport_extobject* eo = cast_dirport(sc, pair_car(args));
    
    if (eo && eo->dir) {
      struct dirent* dp = readdir(eo->dir);
      
      if (dp)
        return mk_string(sc, dp->d_name);
    }
  }

  return sc->F;
}


/* (close-dir-port port) => string */
pointer dirport_closedir(scheme *sc, pointer args)
{
  if (args != sc->NIL) {
    dirport_extobject* eo = cast_dirport(sc, pair_car(args));
    
    if (eo) {
      if (eo->dir && closedir(eo->dir) == 0) {
        eo->dir = NULL;
        return sc->T;
      }
    }
  }
  
  return sc->F;
}


static void dirport_finalize_func(scheme* sc, pointer p)
{
  dirport_extobject* eo = cast_dirport(sc, p);
  
  if (eo) {
    if (eo->dir)
      closedir(eo->dir);
    eo->dir = NULL;
    free(eo);
  }
}


static void dirport_mark_func(scheme* sc, pointer p)
{
  /* NOP */
}

static void dirport_display_func(scheme* sc, char* outstr, pointer p)
{
  dirport_extobject* eo = cast_dirport(sc, p);

  if (eo && eo->dir)
    snprintf(outstr, STRBUFFSIZE, "#<DIRPORT %p>", eo->dir);
  else
    snprintf(outstr, STRBUFFSIZE, "#<DIRPORT:closed>");
}
