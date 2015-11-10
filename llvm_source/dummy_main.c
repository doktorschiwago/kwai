#include <stdio.h>
#include <stdlib.h>

/* weak_alias(missing_MAIN__,MAIN__);  <------- here lies the trick */

#pragma weak MAIN__ = missing_MAIN__

void missing_MAIN__ (void)
{
    fprintf(stderr, "The Fortran main program is missing.\n");
    abort();
}
