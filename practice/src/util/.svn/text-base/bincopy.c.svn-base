#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define COPY_BUFFER_SIZE 256

static void writen(int fd, char *buf, int count)
{
	int ret=0, rest=count;
	
	while((ret += write(fd, buf, rest)) < count){
		rest = count - ret;
	}
	return;
}

int main(int argc, char **argv)
{
	int rfd=0, wfd=0;
	int ret, count;
	char buf[COPY_BUFFER_SIZE];
	struct stat st;
	
	if(argc != 3){
		printf("Usage: %s <src> <dst>\n", argv[0]);
		exit(0);
	}
	if(stat(argv[1], &st)){
		printf("The src file(%s) is not existed\n", argv[1]);
		exit(0);
	}
	rfd = open(argv[1], O_RDONLY);
	if(rfd <=0){
		printf("can't open src file(%s) for read\n", argv[1]);
		goto out;
	}
	wfd = open(argv[2], O_WRONLY|O_CREAT|O_TRUNC, 0666);
	if(wfd <=0){
		printf("can't open dst file(%s) for write\n", argv[2]);
		goto out;
	}
	while((ret = read(rfd, buf, COPY_BUFFER_SIZE))>0){
		count += ret;
		writen(wfd, buf, ret);
	}
out:
	sync();
	if(rfd) close(rfd);
	if(wfd) close(wfd);
	return 0;
}
