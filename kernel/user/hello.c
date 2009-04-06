#include "libc.h"

static char n[] = "HCSS Attendees";

main () {
  char *c = (char *) malloc(strlen(n+1));
  strcpy(c,n);
  printf("Hello %s!\n", c); 
  exit(6*7);
}

