#ifndef _Test_h
#define _Test_h

#include <objc/Object.h>
#include <objc/objc.h>


@interface Test : Object
{
  int x;
}

- init: (int)n;
+ test: (int)n;
- (int)x;

@end

#endif
