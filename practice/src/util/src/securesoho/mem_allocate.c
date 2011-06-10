#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv)
{
	int size;
	char *buf;
	
	if(argc != 2){
		printf("Usage: %s <size K>\n", argv[0]);
		exit(0);
	}
	size = atoi(argv[1]);

	buf = malloc(size*1000);

	if(NULL == buf){
		printf("%s can't allocate %dK memory\n", argv[0], size);
		exit(0);
	}else{
		printf("%s success to allocate %dK memory \n", argv[0], size);
		while(1){
			sleep(1);
		}
	}
	return 0;
}
