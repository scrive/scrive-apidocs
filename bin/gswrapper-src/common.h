#ifndef COMMON_H
#define COMMON_H

#define tryordie(a,b) if ((a) == -1) { perror((b)); exit(1);}
#define dassert(a,b) if ((a)) { fprintf(stderr,"%s\n",(b)); exit(1);}

#endif
