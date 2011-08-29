/*JAIL*/
/*Jail directory*/
#define JAIL    "/"
/*Command name (arguments will be passed as is)*/
#ifndef GS
#define GS     "/usr/bin/gs"
#endif
/*UID to drop after the chroot*/
#define DROPUID 1000

/*LIMITS*/
/*TODO:To set any to unlimited use RLIM_INFINITY*/
/*Adress space size, maximum amount of memory the process can use*/
#define ASSIZE   500*1024*1024
/*Core dump file size: 0, no core dumps*/
#define CORESIZE 0
/*Cpu time in second*/
#define CPUTIME  60
/*Data segment size, same as address space*/
#define DATASIZE ASSIZE
/*File size (per file) in bytes: 20MB (will be zero in the future)*/
#define FILESIZE 20*1024*1024
/*File locks: 0 no file locks*/
#define LOCKS    0
/*Lock memory: 0 no locked memory*/
#define MEMLOCK  0
/*Message queues: 0 no queues*/
#define MQUEUES  0
/*Priority: 39 can't change priority*/
#define NICENESS 0
/*Open files (7 seems to be the minimum, 10 is a reasonable amount)*/
#define FILES    7
/*Process number 1: only this*/
#define PROCESS  1
/*Real time priority, use 0 unless we do need real time*/
#define RTPRIO   0
/*Queued signals, 0 means no signal queuing for this process but kill available*/
#define SIGNALS  0
/*Stack size, in bytes*/
#define STACK    8 * 1024 * 1024
