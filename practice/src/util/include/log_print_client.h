#ifndef __LOG_PRINT_H__
#define __LOG_PRINT_H__
#include  <stdlib.h>
#include  <string.h>
#include  <netinet/in.h> 
#include  <sys/socket.h> 

#define LS_FATAL "fatal"
#define LS_RENDERER "other"
#define LS_MEDIAPLAYER "other"
#define LS_NETWORK_DAEMON "other"
#define LS_REMOVABLE_DEVICE "other"

#ifdef  CONF_DMA_DEBUG_SYSTEM
#define DBGP(...)                       log_print(__FUNCTION__,__LINE__,__VA_ARGS__)
#define DBGP_FILE(...)                  log_print_file(__FILE__,__LINE__,__VA_ARGS__)
#define DBGP_TIME(...)                  log_print_time(__FUNCTION__,__LINE__,__VA_ARGS__)
#define DBGP_CUSTOM(...)                log_custom_print(__VA_ARGS__)
#define DBGP_DATA(module_name,data,len) log_print_data(__FILE__,__LINE__,module_name,data,len);

void log_print(const char *ffunction,int fline,const char * module_name, const char *format, ...);
void log_print_file(char *fname,int fline,const char * module_name, const char *format, ...);
void log_print_time(const char *ffunction,int fline,const char * module_name, const char *format, ...);
void log_print_data(const char * fname, int lineno,const char * module_name, const void * data, int len);
void log_custom_print(const char *module_name,const char *format, ...);

#else
#define  DBGP(...)
#define  DBGP_FILE(...)			   
#define  DBGP_TIME(...)
#define  DBGP_CUSTOM(...)
#define  DBGP_DATA(...)
#endif 

#define DBGP_FATAL(...)                 log_print_fatal(__FUNCTION__,__LINE__,__VA_ARGS__)
void log_print_fatal(char *ffunction,int fline, const char *format, ...);

#endif

