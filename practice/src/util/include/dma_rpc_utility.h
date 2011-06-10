/* 
 * Copyright (C) 2008, ALPHA Networks, inc.
 */
#ifndef __DMA_RPC_UTIL_H
#define __DMA_RPC_UTIL_H

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/queue.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <unistd.h>
#include <netdb.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <pthread.h>

#include "event.h"
#include "evhttp.h"
#include "evrpc.h"

struct rpc_server_handle_s{
    int port;   /* server port */
    pthread_t pid;
    struct evhttp *http;
    struct evrpc_base *base;
    struct evrpc_pool *pool;
    struct event_base *evbase;
};

void* dma_rpc_server_init(int port);
void* dma_rpc_client_init(int port);
#endif
