#ifndef URL_H
#define URL_H

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "llist.h"

#define E_NOT_SUPPORT_RESUME -2
#define HTTP_ERROR_HEAD_REQ_NORMAL  -1
#define HTTP_ERROR_HEAD_REQ_TIMEOUT -2
#define HEADER_MAX_SIZE 2048
int doUnregister;
typedef struct _UrlResource UrlResource;
typedef struct _Url Url;

typedef struct url_arg_s{
        char *key;
        char *value;
}url_arg_t;

struct _Url {
    char *full_url;
    int   service_type;
    char *username;
    char *password;
    char *host;
    int   port;
    char *path;
    char *file;
    void *args;
};

struct _UrlResource {
    char status[4];
    Url *url;
    char *redirectionurl;
    char *outfile;
    char *proxy;
    char *proxy_username;
    char *proxy_password;
    int options;
    loff_t outfile_size;
    loff_t outfile_offset;
    char *mime_type;
    char *content_features;
    int sockfd;                 /* data transport socket */
    void *device;               /*device handler */
    int keepalive;
    int isChunked;
    int support_range;
    int redsonic;
    int imediashare;
    int wmc;                    /*nicle yang */
#ifdef __EASTFORK_SE__
	int tchelper;
#endif
    int nanocaster;             /*jiunming */
    int avg_bitrate;
    int max_bitrate;
    int max_msg;
#ifdef DATA_SOURCE_FRAMEWORK
    void *op;
#endif
};


/* Service types */
enum url_services {
    SERVICE_UNKNOWN = 0,
    SERVICE_HTTP = 1,
    SERVICE_FTP,
    SERVICE_GOPHER,
    SERVICE_FINGER,
    SERVICE_FILE,
    SERVICE_FILES_REMUX,
    SERVICE_RHAP,
    SERVICE_STREAM,
    SERVICE_UDP,     
    SERVICE_UVOX,
    SERVICE_LIVE365,
    SERVICE_WMC,
    SERVICE_TCHELPER
};


typedef struct _HttpHeader HttpHeader;
typedef struct _HttpHeaderEntry HttpHeaderEntry;

struct _HttpHeader {
    List *header_list;
};

struct _HttpHeaderEntry {
    char *key;
    char *value;
};


/* Error string */

extern char *url_error;

/* Funcs */
#define PROTOTYPES
#ifdef PROTOTYPES

Url *url_new (void);
void url_destroy (Url *);
Url *url_init (Url *, char *, enum url_services default_service);
void url_dump(Url *);
char* url_get_arg_value(Url *u, const char *key);
UrlResource *url_resource_new (void);
void url_resource_destroy (UrlResource *);
void url_check_and_set_proxy(Url * u);

HttpHeader *make_http_header (char *r);
char *get_header_value (char *key, HttpHeader * header, int ignore_case);
void free_http_header (HttpHeader * h);

//http connect.  block = 0, use non-block connect
int http_connect (char *remote_host, int port, int block);

//send request to server. test_cancel_func is used to interrupt send before timeout
//default timeout is 10s.
int http_send_request (int fd, char *buffer, int total, int timeout,
                       int (*test_cancel_func) (void));

//get data from server.  test_cancel_func is used to interrupt send before timeout
//default timeout is 10s.
int http_get_data (int fd, char *data, int total, int timeout,
                   int (*test_cancel_func) (void));

//get response header from server. test_cancel_func is used to interrupt send before timeout
//default timeout is 10s.
int http_get_header (int fd, char *header, int timeout,
                     int (*test_cancel_func) (void));

//close http socket.
int http_disconnect (int fd);

#endif /* PROTOTYPES */

#endif /* URL_H */
