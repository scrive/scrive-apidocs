#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <limits.h>

int close_stray_files() {
  DIR *dp;
  struct dirent *ep, ent;
  char dir[PATH_MAX];
  int fd, dfd;
  int rdres;
  int rval = 0;
  
  sprintf(dir,"/proc/%d/fd/",getpid());
  dp = opendir (dir);
  if (dp != NULL) {
    dfd = dirfd(dp);
    while ((rdres = readdir_r(dp,&ent,&ep)) == 0 && ep != NULL) {
      if (*(ent.d_name)>= '1' && *(ent.d_name) <= '9') {
        switch (fd = atoi(ent.d_name)) {
          case STDIN_FILENO:
          case STDOUT_FILENO:
          case STDERR_FILENO:
            break;
          default:
            if(fd != dfd) {
              fprintf(stderr,"Closing %d\n",fd);
              if(close(fd)==-1) {
                perror("Error closing an fd");
                rval = -1;
              }
            }
        }
      }
    }
    if(closedir (dp) == -1) {
      perror ("Couldn't close the directory"); 
      return -1;
    }
  } else {
    perror ("Couldn't open the directory");
    return -1;
  }
  return rval;
}
