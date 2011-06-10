#include <stdio.h>
#include <linux/ptrace.h>
#include <linux/user.h>
#define PTRACE_GETREGS      12

main(int argc, char *argv[])
{
	int pid;
	int ret;
	int addr;
	int data;
	struct user u;
	int i;
	unsigned long *buf,*st;
	unsigned long start,end;
	
	if (argc < 2) {
		printf("Usage: %s <pid> [-r start end]\n", argv[0]);
		exit(1);
	}

	pid = atoi(argv[1]);
	if (pid <= 0) exit(-1);
	ret = ptrace(PTRACE_ATTACH, pid, 0, 0);
	waitpid(pid,&ret,0);
	for(i=0;i<10;i++) {
	    ret = ptrace(PTRACE_GETREGS, pid, 0, &u);
		if (ret != -1) break;
		sleep(1);
	}

	if (ret == -1) {
		perror("Can not get registers:");
	    ret = ptrace(PTRACE_DETACH, pid, 0, 0);
		exit(-1);
	}
	buf = (unsigned long *) &u;
	printf("r0 = %x\n", buf[0]);
	printf("r1 = %x\n", buf[1]);
	printf("r2 = %x\n", buf[2]);
	printf("r3 = %x\n", buf[3]);
	printf("r4 = %x\n", buf[4]);
	printf("r5 = %x\n", buf[5]);
	printf("r6 = %x\n", buf[6]);
	printf("r7 = %x\n", buf[7]);
	printf("r8 = %x\n", buf[8]);
	printf("r9 = %x\n", buf[9]);
	printf("r10 = %x\n", buf[10]);
	printf("fp = %x\n", buf[11]);
	printf("ip = %x\n", buf[12]);
	printf("sp = %x\n", buf[13]);
	printf("lr = %x\n", buf[14]);
	printf("pc = %x\n", buf[15]);
	st = (unsigned long *) buf[13];
	if (argc >= 4) {
		start = strtoul(argv[2],NULL,16);
		end = strtoul(argv[3],NULL,16);
	} else {
		start = 0;
		end = 0xffffffff;
	}
	printf("Dump the execution stack whose value is between[%u,%u]\n",start,end);
	for(i=0;i<1000;i++) {
		if ((data=ptrace(PTRACE_PEEKDATA, pid, st+i, NULL))==-1)
			break;
		if (data >=start && data <= end) 
			printf("[%d] %x\n", i, data);
	}

	ret = ptrace(PTRACE_DETACH, pid, 0, 0);
}	
	
	
