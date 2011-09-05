#include <stdio.h>
#include <stdlib.h>

int main () {
int i=INIT;
void *d;

do {
  d=malloc(i);
  free(d);
  i+=INC;
} while (d != NULL);

printf("%d\n",i-INC);
return 0;
}
