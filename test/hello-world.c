#include <stdio.h>

#include "shared.h"
#include "version.h"

int main (int argc, char **argv)
{
  printf ("test file vr. %s\n", VERSION);

  show_hello_world();

  return 0;
}
