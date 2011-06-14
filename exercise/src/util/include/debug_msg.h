#ifndef _DEBUG_MSG_H_
#define _DEBUG_MSG_H_

#include <stdio.h>

int Difftime_ms(struct timeval *cur, struct timeval *past);
int Timeval_Sub(struct timeval *result, struct timeval *x, struct timeval *y);
#define MSGTYPE_PIPE 0x00000100
void _Debug_Printf(int type, const char *fmt,...);
#define PIPEDEBUG(args...) _Debug_Printf(MSGTYPE_PIPE, ## args)

#endif
