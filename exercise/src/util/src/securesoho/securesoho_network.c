/* Copyright (C) 2005, Alphanetworks, inc.
 * Author:  redsonic
 * $Header: /data/cvsroot/DMA/util/src/securesoho/securesoho_network.c,v 1.2.2.19 2006-07-17 03:53:05 wyin Exp $
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
#include <pthread.h>
#include <time.h>
#include <semaphore.h>

#include "securesoho.h"

#ifndef	__TARGET_H__
#error	"NO Target.h"
#endif

#define IFCONFIG "/sbin/ifconfig"
#ifdef __EM86XX__
#define ROUTE "/sbin/route"
#else
#define ROUTE "/usr/bin/route"
#endif

#define  DHCPCD              "/sbin/dhcpcd"
#define  DHCP_CLIENT         "/usr/bin/dhcp-client"
#define  ZCIP                "/usr/sbin/zcip"

#define  DHCPCD_PID          "/etc/dhcpc/dhcpcd-%s.pid"
#define  DHCP_CLIENT_PID     "/etc/dhcpc/dhcp-client-%s.pid"
#define  WPA_PID             "/var/run/wpa_supplicant.pid"

#define  DHCPCD_INFO         "/etc/dhcpc/dhcpcd-%s.info"
#define  ZEROCONF_INFO       "/tmp/zcip_info"

#define  DHCP_FLAG_FILE      "/tmp/get_dhcpip"
#define  AUTO_FLAG_FILE      "/tmp/get_autoip"

#define IFCONFIG "/sbin/ifconfig"
#define IWCONFIG "/usr/bin/iwconfig"
#define IWPRIV "/usr/bin/iwpriv"

#define DEFAULT_TIMEOUT_SECS		60

static struct network_task_t {
	int 		timeout;
	pthread_t	pid;
	sem_t		lock;
	int 		thread_exit;
	time_t          start_time;
	NETWORK_STATE	state;
} network_task;

static int    first_init_flag  = 1;

int is_exit_thread( void ) 
{
	return network_task.thread_exit;
}

static void network_set_state(NETWORK_STATE state)
{
	sem_wait(&network_task.lock);
	network_task.state = state;
	sem_post(&network_task.lock);
}

NETWORK_STATE network_get_state( void )
{
	return network_task.state;
}

static void start_dhcp_client( int timeout )
{
	char lif[16], buf[256];

	lif[0] = '\0';
	securesoho_lif_get(lif);
	printf("start Ooooooooooooooooooooooo:%s\n", lif);
	if (timeout > 0)
		snprintf(buf, sizeof(buf), "%s %s %d %s&", 
				DHCP_CLIENT, lif,  timeout, "start");
	else 
		snprintf(buf, sizeof(buf), "%s %s %d %s&", 
				DHCP_CLIENT, lif,  60, "start");
	system(buf);
}

static void stop_dhcp_client( void )
{
	char lif[16], buf[256];

	securesoho_lif_get(lif);
	printf("stop Ooooooooooooooooooooooo:%s\n", lif);
	snprintf(buf, sizeof(buf), "%s %s 10 %s", DHCP_CLIENT, lif, "stop");
	my_system(buf);

	unlink(DHCP_FLAG_FILE);
	unlink(AUTO_FLAG_FILE);
}


static void stop_wpa_client( void )
{
	if ( ps_exist(WPA_PID) ) {
		kill_pidfile(WPA_PID, 15);
		unlink(WPA_PID);
	} else { 
		do_cmd(IFCONFIG, WLAN_PORT, "0.0.0.0", "down", NULL);
	}
}

static void start_wireless_thread( void )
{
	if (network_task.pid == 0 ) {
		network_task.thread_exit = 0;
		sem_wait(&network_task.lock);
		pthread_create(&network_task.pid, NULL, securesoho_set_wireless, NULL);
		sem_post(&network_task.lock);
	}
}

static void stop_wireless_thread( void )
{
	if (network_task.pid > 0) {
		network_task.thread_exit = 1;
		pthread_join(network_task.pid,  NULL);

		network_task.thread_exit = 0;
		network_task.pid = 0;
	}
}

static int network_reset( void )
{

	stop_dhcp_client();
	stop_wpa_client();
	stop_wireless_thread();


  	do_cmd(IFCONFIG, LAN_PORT, "0.0.0.0", "down", NULL);

	network_set_state(NETWORK_STATE_INIT);

	return 0;
}

static int network_save_autoinfo(const char* ifname)
{
	FILE *fp;
	char buf[256], value[64], key[64], info[30];

	if ( (fp = fopen(ZEROCONF_INFO, "r")) == NULL ) {
		fprintf(stderr, "%s:%d, open %s failed\n", 
				__FUNCTION__, __LINE__, info);
		return -1;
	}

	printf("Get ip from zeroconf successful.\n");
	while ( fgets(buf, sizeof(buf), fp) ) {

		decode_str(buf, key, value, "=");	
		if (!strcasecmp(key, "IPADDR")) {
			printf("New ip address: %s\n", value);
			securesoho_string_set("IP2", value);
		} 
	}
	securesoho_string_set("NETMASK2", "255.255.0.0");

	fclose(fp);
	return 0;
}

static int network_save_dhcpinfo( const char* ifname)
{
	FILE *fp;
	char *p, *q;
	char buf[256], value[64], key[64], info[30];
	char set_dns_srv[64], nameserver[3][16];

	snprintf(info, sizeof(info), DHCPCD_INFO, ifname);

	if ( (fp = fopen(info, "r")) == NULL ) {
		fprintf(stderr, "%s:%d, open %s failed\n", 
				__FUNCTION__, __LINE__, info);
		return -1;
	}

	securesoho_string_get( "SET_DNS_SRV", set_dns_srv);

	printf("Get ip from dhcp server successful.\n");
	while ( fgets(buf, sizeof(buf), fp) ) {

		decode_str(buf, key, value, "=");	

		if (!strcasecmp(key, "IPADDR")) {
			printf("New ip address: %s\n", value);
			securesoho_string_set("IP2", value);
		} else if (!strcasecmp(key, "NETMASK")) {
			printf("      netmask2: %s\n", value);
			securesoho_string_set( "NETMASK2", value);
		} else if (!strcasecmp(key, "GATEWAY")) {
			printf("       gateway: %s\n", value);
			securesoho_string_set( "GATEWAY2", value);
		} else if (!strcasecmp(key, "DNS")) {
			FILE* fpr;

			printf("    dns server: %s\n", value);

			if ( !value[0] ) 
				continue;

			p = value;
			q = strstr(p, ",");

			if (q) {
				*q = '\0';
				p = q + 1;
			} 
			strcpy(nameserver[0], p);

			if ( (fpr = fopen("/tmp/resolv.conf", "w")) != NULL ) {
				fprintf(fpr, "nameserver %s\n", nameserver[0]);
				fclose(fpr);
			}

			if (set_dns_srv[0] != 'm')
				securesoho_string_set( "LANDNS", nameserver[0] );
		} 
	}
	
	fclose(fp);
	return 0;
}

static void network_if_conf( void )
{
	FILE* fp;
	SecureSOHO_LAN lan;

	securesoho_lan_get(&lan);

	do_cmd(IFCONFIG, lan.lan_ifname, lan.ip, "netmask", lan.netmask, "up", NULL);
	if (strncmp(lan.ip, "169.254", 7))
		do_cmd(ROUTE, "add", "default", "gw", lan.gateway, NULL);

	if ((fp = fopen("/etc/resolv.conf", "w"))!=NULL) {
		fprintf(fp, "nameserver %s\n", lan.dns);
		fclose(fp);
	}
}

static void network_ifconf_static( void )
{
	network_if_conf();
}

static void network_ifconf_dhcp( int timeout )
{
	start_dhcp_client(timeout);
}

static void network_task_init( void )
{
	memset(&network_task, 0, sizeof(struct network_task_t));

	network_task.timeout = DEFAULT_TIMEOUT_SECS;
	network_task.pid     = 0;
	sem_init(&network_task.lock, 0, 1);	
	network_task.thread_exit = 0;
	network_set_state(NETWORK_STATE_INIT);
}

int network_update_wireless_state(int success)
{
	time_t tm = time(NULL) - network_task.start_time;
	char lan_type[64];

	if (success) {
		securesoho_string_get( "LAN_TYPE", lan_type);

		if (lan_type[0] == 'd') {
			if (tm >= network_task.timeout) {
				network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
				printf("%s:%d, wireless timeout\n", 
						__FUNCTION__, __LINE__);
			} else { 
				network_ifconf_dhcp( network_task.timeout - tm );
				network_set_state(NETWORK_STATE_REQUESTING_IP);
			}
		} else {
			network_ifconf_static();
			network_set_state(NETWORK_STATE_DONE);
		}
	} else {
		network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
	}

	sem_wait(&network_task.lock);
	network_task.pid = 0;
	sem_post(&network_task.lock);

	return 0;
}

int network_restart(int timeout)
{
	int dhcp = 0;
	char lan_type[64];
	char lif[16];

	if (first_init_flag) {
		first_init_flag = 0;
		network_task_init();
	}

	if (timeout > 0 )
		network_task.timeout = timeout;
	else
		network_task.timeout = DEFAULT_TIMEOUT_SECS;

	network_task.start_time = time(NULL);

	network_reset();

	securesoho_lif_get(lif);
	securesoho_string_get( "LAN_TYPE", lan_type);

	dhcp = (lan_type[0] == 'd' ? 1 : 0);

	if ( !strcmp(lif, WLAN_PORT) ) {
		printf("network_restart:start wireless thread\n");
		start_wireless_thread(); 
		network_set_state(NETWORK_STATE_WIRELESS_CONNECTING);
	} else {
		if (dhcp) {
			network_ifconf_dhcp(network_task.timeout);
			network_set_state(NETWORK_STATE_REQUESTING_IP);
		} else {
			network_ifconf_static();
			network_set_state(NETWORK_STATE_DONE);
		}

	}

	return dhcp;
}

NETWORK_STATE network_check_set_dhcp( void )
{
	int ready = 0;
	time_t tm;
	SecureSOHO_LAN lan;
	NETWORK_STATE  state = network_get_state();

	switch (state) {
	case NETWORK_STATE_WIRELESS_CONNECTING:
		tm = time(NULL) - network_task.start_time;

		if ( tm >= network_task.timeout ) {
			fprintf(stderr, "Wireless timeout\n");
			stop_wireless_thread();
			network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
		} 
		break;
	case NETWORK_STATE_REQUESTING_IP:
	case NETWORK_STATE_DONE:
		if (securesoho_lan_get(&lan)) {
			fprintf(stderr, "##%s;%d, failed to securesoho_lan_get\n",
				       	__FUNCTION__, __LINE__);
			return 0;
		}

		if (lan.lan_type == LAN_FIXEDIP) {
			ready = 1;
			break;
		}

		if ( exist(DHCP_FLAG_FILE) ) {
			fprintf(stderr, "##%s;%d, dhcp ip got!\n", 
					__FUNCTION__, __LINE__);
			unlink("/tmp/ftp_mode");
			unlink(DHCP_FLAG_FILE);
			unlink(AUTO_FLAG_FILE);

			network_save_dhcpinfo(lan.lan_ifname);

			ready = 1;
		} else if (exist(AUTO_FLAG_FILE) ) {
			fprintf(stderr, "##%s;%d, auto ip got!\n", 
					__FUNCTION__, __LINE__);
			unlink(AUTO_FLAG_FILE);

			network_save_autoinfo(lan.lan_ifname);

			ready = 1;
		}

		if (ready) {
			network_if_conf();
			network_set_state(NETWORK_STATE_DONE);
		}
	default:
		break;
	}

	return network_task.state;
}

int network_sync_restart( void )
{

	if (network_restart(0) == 0 )
		return 0;

	do {
		if ( network_check_set_dhcp() == NETWORK_STATE_DONE ) {
			fprintf(stderr, "Network Enabled\n");
			return 1;
		} else  {
			fprintf(stderr, "Detecting...\n");
			sleep(1);
		}
	} while(1);

	return 0;
}

int GetIfHwAddr(const char* ifname,int namelen,char* hwaddr)
{
	struct ifreq ifr;
	int s;
	int ok = 0;

	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s==-1) {
		return -1;
	}

	strcpy(ifr.ifr_name, ifname);
	if (ioctl(s, SIOCGIFHWADDR, &ifr) == 0)
		ok = 1;
	else
		printf("ioctl failed in function:%s\n",__FUNCTION__);
	close(s);
	if (ok)
		memcpy(hwaddr,ifr.ifr_hwaddr.sa_data, 6);
	else
		return -1;
	return 0;
}

int WaitInterfaceUp(const char *devname, int timeout)
{
	int up = 0, LocalSock;
	struct ifreq ifReq;

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		printf("FILE:%s,LINE:%d.Can't do that\r\n",__FILE__,__LINE__);
		return 0;
	}

	do {
		strcpy (ifReq.ifr_name, devname);

		if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
			printf("Can't get flags\r\n");
			close(LocalSock);
			exit(1);
		}

		if ((ifReq.ifr_flags & IFF_UP)){
			up = 1;
			break;
		}           

		if (!timeout)  
			break;
		--timeout;
		sleep(1);
	} while (timeout);

	close(LocalSock);
	return up;
}

int WaitInterfaceReady(const char *devname, int timeout)
{
	char szBuffer[16*sizeof(struct ifreq)];
	struct ifconf ifConf;
	struct ifreq ifReq;
	int nResult;
	int LocalSock;
	struct sockaddr_in LocalAddr;
	int i;

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		printf("FILE:%s,LINE:%d.Can't do that\r\n",__FILE__,__LINE__);
		return 0;
	}
	ifConf.ifc_len = sizeof szBuffer;
	ifConf.ifc_ifcu.ifcu_buf = (caddr_t)szBuffer;
	nResult = ioctl(LocalSock, SIOCGIFCONF, &ifConf);
	if (nResult < 0) {
		printf("FILE:%s,LINE:%d.ioctl error\r\n",__FILE__,__LINE__);
		exit(1);
	}
	for (;;) {
		for (i = 0;(i < ifConf.ifc_len);) {
			struct ifreq *pifReq = (struct ifreq *)((caddr_t)ifConf.ifc_req + i);
			i += sizeof *pifReq;
			strcpy (ifReq.ifr_name, pifReq -> ifr_name);
			if (strcmp (devname, pifReq -> ifr_name)==0) {
				if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0) {
					printf("FILE:%s,LINE:%d.Can't get flags\r\n",__FILE__,__LINE__);
					exit(1);
				}
				if (!(ifReq.ifr_flags & IFF_UP)) {
					break;
				}
				if (pifReq -> ifr_addr.sa_family == AF_INET) {
					memcpy (&LocalAddr, &pifReq -> ifr_addr, sizeof pifReq -> ifr_addr);
					if (LocalAddr.sin_addr.s_addr != 0) {
						close(LocalSock);
						return 1;
					}
				}
			}
		}
		if (!timeout)  break;
		--timeout;
		sleep(1);
	}
	close(LocalSock);
	return 0;
}

int WaitHostInterfaceReady(int timeout)
{
	char dev[64];

	securesoho_lif_get(dev);
	if(dev[0] == '\0')
		return 0;
	return WaitInterfaceReady(dev, timeout);
}
