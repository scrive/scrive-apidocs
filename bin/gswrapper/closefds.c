#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "fdcloser.h"
#include "common.h"

int main(int argc, char**argv) {
  if(argc < 2) {
     fprintf(stderr,"Usage: %s exec args",argv[0]);
     exit(1);
  }
  tryordie(close_stray_files(),"Failed closing stray files");
  argv++;
  tryordie(execvp(*argv,argv),"Couldn't execute the program");
  return 0;
}
