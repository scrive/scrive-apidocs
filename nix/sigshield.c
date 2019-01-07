#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

pid_t child_pid = 0;

// Send SIGTERM to the child process.
void term_handler(int sig) {
  int err = kill(child_pid, SIGTERM);
  if(err == -1) {
    perror("kill");
  }
}

int main(int argc, char **argv) {
  if(argc < 2) {
    printf("Usage: %s command [args]\n", argv[0]);
    return 2;
  }

  child_pid = fork();
  if(child_pid == -1) {
    perror("fork");
    return EXIT_FAILURE;
  }

  if(child_pid == 0) { // This process is the child.
    // The child process shall have its own process group to be shielded
    // from the parent's signals.
    (void)setsid();
    execv(argv[1], &argv[1]);
    // execv() is not supposed to return.
    perror("execv");
    return EXIT_FAILURE;

  } else { // This process is the parent.
    struct sigaction action = {0};

    // Kill the child process on SIGTERM.
    action.sa_handler = term_handler;
    sigaction(SIGTERM, &action, NULL);

    // Ignore SIGINT.
    action.sa_handler = SIG_IGN;
    sigaction(SIGINT, &action, NULL);

    child_pid = waitpid(child_pid, NULL, 0);
  }

  return EXIT_SUCCESS;
}
