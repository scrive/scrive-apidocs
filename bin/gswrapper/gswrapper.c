#include <sys/types.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gswrapper.h"
#include "fdcloser.h"
#include "common.h"

#define tryordie(a,b) if ((a) == -1) { perror((b)); exit(1);}
#define dassert(a,b) if ((a)) { fprintf(stderr,"%s\n",(b)); exit(1);}
#define setlimit(a,b) { limit.rlim_cur = (b); limit.rlim_max = (b); tryordie(setrlimit((RLIMIT_##a),&limit),"Couldn't enforce limit " #b);}

int main(int argc, char **argv) {
  struct rlimit limit;
  /*Get current permissions*/
  dassert(geteuid()!=0,"I need to be root to do this!");

  /*Jail the process*/
  tryordie(chdir(JAIL),"Couldn't enter the jail at `"JAIL"'");
  tryordie(chroot(JAIL),"Couldn't chroot in the jail at `"JAIL"'");

  /*Drop privileges*/
  tryordie(setuid(DROPUID),"Couldn't drop privileges");
  dassert(geteuid()!=DROPUID,"I still have privileges! U_U");
  dassert(getuid() !=DROPUID,"I still have privileges! U_U");

  tryordie(close_stray_files(),"Couldn't close stray files");

  /*Limit resources*/
  setlimit(AS        ,ASSIZE  );
  setlimit(CORE      ,CORESIZE);
  setlimit(CPU       ,CPUTIME );
  setlimit(DATA      ,DATASIZE);
  setlimit(FSIZE     ,FILESIZE);
  setlimit(LOCKS     ,LOCKS   );
  setlimit(MEMLOCK   ,MEMLOCK );
  setlimit(MSGQUEUE  ,MQUEUES );
  setlimit(NICE      ,NICENESS);
  setlimit(NOFILE    ,FILES   );
  setlimit(NPROC     ,PROCESS );
  setlimit(RTPRIO    ,RTPRIO  );
  setlimit(SIGPENDING,SIGNALS );
  setlimit(STACK     ,STACK   );

  /*Execute Ghost Script*/
  argv[0]=GS;
  tryordie(execvp(argv[0],argv),"Couldn't execute `"GS"'");
  return 0;
}
