#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <signal.h>

volatile int top=INIT;
int i=INIT;
char buf[64];
struct sigaction act;
char asigstack[SIGSTKSZ];
stack_t ss;

void show_res(int signal) {
  snprintf(buf,64,"%d\n",top);
  write(STDERR_FILENO,buf,strnlen(buf,64));
  fsync(STDERR_FILENO);
  _exit(1);
}

int recurse(int limit) {
if (limit) return 1 + recurse(limit -1);
else return 0;
}

int main () {
memset (&act, 0, sizeof(act));
act.sa_handler = show_res;
act.sa_flags = SA_ONSTACK;
sigfillset(&act.sa_mask);
ss.ss_sp=asigstack;
ss.ss_size=SIGSTKSZ;
ss.ss_flags=0;
if(sigaltstack(&ss, NULL)) {
		perror ("Can't set stack for signals :(");
		return 2;
}
if (sigaction(SIGSEGV, &act, NULL)) {
		perror ("Can't set signals :(");
		return 2;
}

while (1) {
  recurse(i);
  top=i;
  i+=INC;
}
return 0;
}
