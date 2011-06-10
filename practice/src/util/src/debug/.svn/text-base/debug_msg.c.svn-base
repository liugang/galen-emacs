#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include "debug_msg.h"

static unsigned int DMA_Debug_Mask=0;
static unsigned int DMA_Debug_Init=0;
/*
	argument2 should be larger than argument3
	when argument2 less than argument3 return non-zero value
*/
int Timeval_Sub(struct timeval *result, struct timeval *x, struct timeval *y)
{
	result->tv_sec=x->tv_sec - y->tv_sec;
	if ( x->tv_usec < y->tv_usec ) {
		result->tv_sec--;
		result->tv_usec = x->tv_usec + 1000000 - y->tv_usec;
	}else {
		result->tv_usec = x->tv_usec - y->tv_usec;
	}
	return result->tv_sec>=0?0:-1;
}

// return diff. in ms
int Difftime_ms(struct timeval *cur, struct timeval *past)
{
	struct timeval rt;
	Timeval_Sub(&rt, cur, past);
	rt.tv_usec+=(rt.tv_sec*1000000);
	return rt.tv_usec>=0?rt.tv_usec/1000:-1;
}


static void sig_readconfig(int no)
{
    FILE *fp = fopen("/tmp/dbg","r");
    char *buf;
    signal(30,sig_readconfig);
    if (fp == NULL){
		return;
	}

    buf = (char *) malloc(4000);
	if (!buf)
		return;
    while(fgets(buf,3999,fp)) {
        if (buf[0] == '#') continue;
        if (strncmp(buf,"mask=",5)==0) {
            DMA_Debug_Mask = strtoul(buf+5,NULL,16);
        }
    }
    free(buf);
    fclose(fp);
    DMA_Debug_Init = 1;
    printf("\n\n###new debug mask is %x\n", DMA_Debug_Mask);
}
void _Debug_Printf(int type, const char *fmt,...)
{
	va_list ap;
	if (DMA_Debug_Init==0) {
        sig_readconfig(0);
	}
	va_start(ap,fmt);
	if (type & DMA_Debug_Mask)
		vprintf(fmt, ap);
	va_end(ap);
}

