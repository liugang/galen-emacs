/* Copyright (C) 2005, Alphanetworks, inc.
 * Author:	redsonic
 * $Header: /data/cvsroot/DMA/util/src/securesoho/securesoho_network2.c,v 1.1.2.22 2007-11-19 10:24:06 wchen Exp $
 * vim:cindent:ts=8:sw=8
 * Wills Yin: Group all functions related to network into this file.
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <linux/wireless.h>
#include <errno.h>
#include <time.h>
#include <semaphore.h>
#include <linux/sockios.h>

#include "securesoho.h"

#ifndef	__TARGET_H__
#error	"NO Target.h"
#endif

#define	   RETRY_CONNECT_COUNT	   5

struct network_session_t {
	int		   sockfd;
	struct sockaddr_in sa;
};

struct network_session_t *session=NULL;

int securesoho_network_init_host(char *host)
{
	int try;

	if(NULL == host) return -1;

	if(NULL == session){
		session = (struct network_session_t *)malloc(sizeof(struct network_session_t));
		memset(session, '\0', sizeof(struct network_session_t));
	}
	if((session->sockfd = socket(AF_INET, SOCK_STREAM, 0))<0)
		goto error;
	session->sa.sin_family = AF_INET;
	inet_aton(host, &(session->sa.sin_addr));
	session->sa.sin_port = htons(NETWORK_DAEMON_PORT);

	try = RETRY_CONNECT_COUNT;
	while((try>0) && (connect(session->sockfd, (struct sockaddr*)&(session->sa), sizeof(session->sa))<0)){
		printf("connected to netowrk daemon fail \n");
		try --;
		/* sleep 1 sec for connection again */
		sleep(1);
	}
	if (try <= 0){
		close(session->sockfd);
		session->sockfd = -1;
	}
error:
	return 0;

}

int securesoho_network_init()
{
	return securesoho_network_init_host(NETWORK_DAEMON_HOST);
}

static int send_network_cancel_cmd(void)
{
	char buf[16];
	int	 i;

	buf[0] = NETWORK_COMMAND_CANCEL;
	i = 4;
	memcpy(buf+1, &i, 4);

	if(session && session->sockfd > 0){
		i = write(session->sockfd, buf, 9);
		if(i==9){
			printf("send network stop command ok\n");
		}
		read(session->sockfd, buf, 5);
		if(buf[0] == NETWORK_COMMAND_CANCEL){
			memcpy(&i, buf+1, 4);
			printf("receive network stop command response:%d\n", i);
		}
	}
	return 0;
}


static int send_network_restart_cmd(int timeout, int inline_wireless)
{
	char buf[16];
	int	 i;
//	securesoho_int_set("INLINE_WIRELESS_PROFILE", inline_wireless);

	buf[0] = NETWORK_COMMAND_RESTART;
	i = 4;
	memcpy(buf+1, &i, 4);
	i = timeout;
	memcpy(buf+5, &i, 4);

	if(session && session->sockfd > 0){
		i = write(session->sockfd, buf, 9);
		if(i==9){
			printf("send network restart command ok\n");
		}
		read(session->sockfd, buf, 5);
		if(buf[0] == NETWORK_COMMAND_RESTART){
			memcpy(&i, buf+1, 4);
			printf("receive network restart command response:%d\n", i);
		}
	}
	return 0;
}

int network_cancel(void){
	send_network_cancel_cmd();
	return 0;
}

int network_restart(int timeout)
{
	printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);

	return send_network_restart_cmd(timeout, 0);
}

int network_restart2(int timeout)
{
	printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);

	return send_network_restart_cmd(timeout, 1);
}

int network_sync_restart( void )
{
	if (network_restart(0) == 0 )
		return 0;
	do {
		if(NETWORK_STATE_DONE == network_get_state()){
			printf("network setup finished\n");
			break;
		}else{
			printf("network setuping .....\n");
		}
		sleep(1);
	} while(1);
	return 0;
}

NETWORK_STATE network_get_state()
{
	char buf[16];
	int	 i;
	int	 state=NETWORK_STATE_DONE;
	
	buf[0] = NETWORK_COMMAND_STATE;
	i = 0;
	memcpy(buf+1, &i, 4);

	if(session && session->sockfd > 0){
		i = write(session->sockfd, buf, 5);
		if(i==5){
			//printf("send network state command ok\n");
		}
		read(session->sockfd, buf, 9);
		if(buf[0] == NETWORK_COMMAND_STATE){
			memcpy(&state, buf+5, 4);
			//printf("receive network state command response:%d\n", state);
		}else{
			//printf("receive network non-state command:%c\n", buf[0]);
		}
	}
	return	state;
}

int network_daemon_inactive(void)
{
	char buf[16];
	int	 i;
	
	buf[0] = NETWORK_COMMAND_DAEMON_INACTIVE;
	i = 0;
	memcpy(buf+1, &i, 4);

	if(session && session->sockfd > 0){
		i = write(session->sockfd, buf, 5);
		if(i==5){
			printf("send network inactive command ok\n");
		}
		read(session->sockfd, buf, 5);
		if(buf[0] == NETWORK_COMMAND_DAEMON_INACTIVE){
			memcpy(&i, buf+1, 4);
			printf("receive network inactive command response:%d\n", i);
		}
	}
	return 0;
}

int network_daemon_active(void)
{
	char buf[16];
	int	 i;
	
	buf[0] = NETWORK_COMMAND_DAEMON_ACTIVE;
	i = 0;
	memcpy(buf+1, &i, 4);

	if(session && session->sockfd > 0){
		i = write(session->sockfd, buf, 5);
		if(i==5){
			printf("send network active command ok\n");
		}
		read(session->sockfd, buf, 5);
		if(buf[0] == NETWORK_COMMAND_DAEMON_ACTIVE){
			memcpy(&i, buf+1, 4);
			printf("receive network active command response:%d\n", i);
		}
	}
	return 0;
}
void wlan_dongle_changed_notify_nd (int vid, int pid, int availble)
{
	char buf[16];
	int len;

	if(availble)
		buf[0] = COMMAND_DONGLE_INSERT;
	else
		buf[0] = COMMAND_DONGLE_REMOVE;
	memcpy(buf+1, &vid, 4);
	memcpy(buf+5, &pid, 4);
	memcpy(buf+9, &availble, 4);

	if(session && session->sockfd > 0){
		len = write(session->sockfd, buf, 13);
		if(len == 13){
			printf("send wlan dongle changed command ok\n");
		}
		read(session->sockfd, buf, 1);
		if(buf[0] == COMMAND_DONGLE_INSERT || buf[0] == COMMAND_DONGLE_REMOVE){
			printf("receive wlan dongle changed command response.\n");
		}
	}
}

int get_ifnamelist_proc(char **ifnamelist)
{
#define MAXIF	16
	FILE *fp;
	char *ptr;
	char buf[256], *tmplist, *pIflist;
	int count, line, i;

	*ifnamelist = 0;
	if ((fp = fopen("/proc/net/dev", "r")) == NULL) {
		perror("Open /proc/net/dev");
		return -1;
	}
	count = 0;
	line = 0;
	tmplist = (char *)malloc(IFNAMSIZ * MAXIF);
	bzero(tmplist, IFNAMSIZ * MAXIF);
	while (fgets(buf, sizeof(buf), fp)) {
		if (line++ < 2)
			continue;
		i = 0;
		while (isspace(buf[i]))
			i++;
		ptr = strstr(buf, ":");
		memcpy(tmplist + count * IFNAMSIZ, &buf[i], ptr - &buf[i]);
		count++;
	}

	*ifnamelist = (char *)malloc(IFNAMSIZ * count);
	pIflist = *ifnamelist;
	for (i = 0; i < count; i++) {
		memcpy(pIflist, tmplist + i * IFNAMSIZ, IFNAMSIZ); 
		pIflist += IFNAMSIZ;
	}
	free(tmplist);

	return count;
}

