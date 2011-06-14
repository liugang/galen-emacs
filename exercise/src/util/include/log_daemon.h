#ifndef __LOG_DAEMON_H__
#define __LOG_DAEMON_H__
#include  <stdlib.h>
#include  <string.h>


typedef enum _dp_output_type_s{
    DP_CONSOLE,
    DP_FILE,
    DP_NET
}dp_output_type_t;

typedef struct _dp_module_s{
    char *name;
    char *color;
    struct _dp_module_s *next;
}dp_module_t;

typedef struct _network_processor_t{
    int connfd;
    struct _network_processor_t *next;
}network_processor_t;

typedef struct  _dp_plugin_s{
    dp_output_type_t type;
    void (*print)(char *color_or_filename,const char *format);
    struct _dp_plugin_s *next;
}dp_plugin_t;


#endif