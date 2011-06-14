/* Copyright (C) 2005, Alphanetworks, inc.
 * Author:  redsonic
 * $Header: /data/cvsroot/DMA/network/dmacfg/dmacfg-2.0/network_util.c,v 1.1.2.22 2007-12-10 06:47:47 ken Exp $
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

#include "wireless_setup.h"
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "network_util.h"
#include "wireless_profile_access.h"
#include "dbus_service.h"

#define ROUTE "/sbin/route"

#define	 DHCPCD		     "/sbin/dhcpcd"
#define	 DHCP_CLIENT	     "/usr/bin/dhcp-client"
#define	 ZCIP		     "/usr/sbin/zcip"
#define	 PPPOE_CLIENT	      "/usr/bin/pppoe-client"

#define	 DHCPCD_PID	     "/etc/dhcpc/dhcpcd-%s.pid"
#define	 DHCP_CLIENT_PID     "/etc/dhcpc/dhcp-client-%s.pid"
#define	 WPA_PID	     "/var/run/wpa_supplicant.pid"

#define	 DHCPCD_INFO	     "/etc/dhcpc/dhcpcd-%s.info"
#define	 ZEROCONF_INFO	     "/tmp/zcip_info"

#define	 DHCP_FLAG_FILE	     "/tmp/get_dhcpip"
#define	 AUTO_FLAG_FILE	     "/tmp/get_autoip"
#define	 PPPOE_FLAG_FILE     "/tmp/pppoe_connected"

#define	 RESOLV_CONF_FILE	"/etc/resolv.conf"

#define SCRIPT_NETWORK_UP	"/etc/init.d/network_up.sh"
#define SCRIPT_NETWORK_DOWN	"/etc/init.d/network_down.sh"

#define IFCONFIG "/sbin/ifconfig"
#define IWCONFIG "/usr/bin/iwconfig"
#define IWPRIV "/usr/bin/iwpriv"

#define DEFAULT_TIMEOUT_SECS		80

static struct network_task_t {
	int		timeout;
	pthread_t	pid;
	sem_t		lock;
	int		restart;
	int		thread_exit;
	time_t		start_time;
	NETWORK_STATE	state;
} network_task;

int is_nfsroot_running(void)
{
	/* first, read /proc/cmdline into memory */
	char	      buffer[1024];
	int	      len;
	int	      fd = open("/proc/cmdline",O_RDONLY);

	if (fd < 0) {
		fprintf(stderr, "Failed to open /proc/cmdline\n");
		return 0;
	}

	do {
		len = read(fd,buffer,sizeof(buffer)); 
	}while (len == -1 && errno == EINTR);

	if (len < 0) {
		close(fd);
		fprintf(stderr, "Failed to read /proc/cmdline\n");
		return 0;
	}
	close(fd);

	return strstr(buffer, "nfsroot")?1:0;
}

int is_exit_thread( void ) 
{
	return network_task.thread_exit;
}

void cfg_network_set_state(NETWORK_STATE state)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	sem_wait(&network_task.lock);
	network_task.state = state;
	nm_dbus_signal_network_status(state);
	sem_post(&network_task.lock);
}

NETWORK_STATE cfg_network_get_state( void )
{
//	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	NETWORK_STATE state;
	
	sem_wait(&network_task.lock);
	state = network_task.state;
	sem_post(&network_task.lock);
//	printf("\033[1;45m ===============  %s(), %d state = %d...================== \033[0m  \n", __FUNCTION__, __LINE__, state);

	return state;
}

static void cfg_start_dhcp_client( int timeout )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	char lif[16], buf[256];

	lif[0] = '\0';
	securesoho_lif_get(lif);
	DBGP(LS_NETWORK_DAEMON,"start Ooooooooooooooooooooooo:%s\n", lif);
	snprintf(buf, sizeof(buf), "%i", timeout);
	if (timeout > 0)
		do_cmd_bg(DHCP_CLIENT, lif, buf, "start", NULL);
	else 
		do_cmd_bg(DHCP_CLIENT, lif, "60", "start", NULL);
}

static void cfg_start_pppoe_client( int timeout )
{
	char lif[16], buf[256];

	lif[0] = '\0';
	securesoho_lif_get(lif);
	printf("start PPPOE Ooooooooooooooooooooooo:%s\n", lif);
	if (timeout > 0)
		snprintf(buf, sizeof(buf), "%s %s %d %s&", PPPOE_CLIENT, lif,  timeout, "start");
	else
		snprintf(buf, sizeof(buf), "%s %s %d %s&", PPPOE_CLIENT, lif,  60, "start");

	my_system(buf);
}

void cfg_stop_dhcp_client( void )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	char lif[16];
 
	securesoho_lif_get(lif);
	DBGP(LS_NETWORK_DAEMON,"stop Ooooooooooooooooooooooo:%s\n", lif);
	do_cmd_bg(DHCP_CLIENT, lif, "60", "stop", NULL);

	unlink(ZEROCONF_INFO);
	unlink(DHCP_FLAG_FILE);
	unlink(AUTO_FLAG_FILE);
}

#ifdef CONF_PPPOE
static void cfg_stop_pppoe_client( void )
{
	char lif[16], buf[256];

	securesoho_lif_get(lif);
	printf("stop PPPOE Ooooooooooooooooooooooo:%s\n", lif);
	snprintf(buf, sizeof(buf), "%s %s 10 %s", PPPOE_CLIENT, lif, "stop");
	my_system(buf);
	unlink(PPPOE_FLAG_FILE);
}
#endif

static void cfg_stop_wpa_client( void )
{
	if ( ps_exist(WPA_PID) ) {
		kill_pidfile(WPA_PID, 15);
		unlink(WPA_PID);
	} 
}

#define MIN_WIRELESS_SETUP_TIMEOUT     40
#define MAX_WIRELESS_SETUP_TIMEOUT     30000
static void* cfg_set_wireless( void* args )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	int ret = 0;
	SecureSOHO_Wireless sw;
	/* use the inline wireless profile */
	securesoho_wireless_get(&sw);
	ret = wireless_setup_set_profile(&sw, network_task.timeout);
	cfg_network_update_wireless_state(ret);
	pthread_exit(NULL);
	return NULL;
}

static void cfg_start_wireless_thread( void )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	if (network_task.pid == 0 ) {
		network_task.thread_exit = 0;
		sem_wait(&network_task.lock);
		pthread_create(&network_task.pid, NULL, cfg_set_wireless, NULL);
		sem_post(&network_task.lock);
	}
}

static void cfg_stop_wireless_thread( void )
{
	if (network_task.pid > 0) {
		network_task.thread_exit = 1;
		pthread_join(network_task.pid,	NULL);

		network_task.thread_exit = 0;
		network_task.pid = 0;
	}
}

int cfg_network_reset( void )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

#ifdef CONF_DHCPCD
	cfg_stop_dhcp_client();
#endif
#ifdef CONF_PPPOE
	cfg_stop_pppoe_client();
#endif
	cfg_stop_wpa_client();
	cfg_stop_wireless_thread();

	if (!is_nfsroot_running()){
		do_cmd(IFCONFIG, LAN_PORT, "0.0.0.0", "up", NULL);
		if(wlan && wlan->wlan_name[0]!=0){
			/* no wireless driver existed */
			do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "up", NULL);
		}
		/* product independent network init script */
		my_system("/etc/init.d/network_init.sh");
	}
	cfg_network_set_state(NETWORK_STATE_INIT);
	return 0;
}

static int cfg_network_save_autoinfo(const char* ifname)
{
	FILE *fp;
	char buf[256], value[64], key[64];
	SecureSOHO_LAN laninfo;

	securesoho_lan_get(&laninfo);
	if ( (fp = fopen(ZEROCONF_INFO, "r")) == NULL ) {
		DBGP(LS_NETWORK_DAEMON, "%s:%d, open failed\n", __FUNCTION__, __LINE__);
		return -1;
	}

	DBGP(LS_NETWORK_DAEMON,"Get ip from zeroconf successful.\n");
	while ( fgets(buf, sizeof(buf), fp) ) {

		decode_str(buf, key, value, "=");	
		if (!strcasecmp(key, "IPADDR")) {
			DBGP(LS_NETWORK_DAEMON,"New ip address: %s\n", value);
			sprintf(laninfo.ip, "%s", value);
		} 
	}
	fclose(fp);

	sprintf(laninfo.netmask, "%s", "255.255.0.0");
	securesoho_lan_set(&laninfo, 0);
	return 0;
}

static int cfg_network_save_dhcpinfo( const char* ifname)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	FILE *fp;
	char *p, *q;
	char buf[256], value[64], key[64], info[64];
	SecureSOHO_LAN laninfo;
	int update=0;

	securesoho_lan_get(&laninfo);

	snprintf(info, sizeof(info), DHCPCD_INFO, ifname);
	if ( (fp = fopen(info, "r")) == NULL ) {
		DBGP(LS_NETWORK_DAEMON, "%s:%d, open %s failed\n", __FUNCTION__, __LINE__, info);
		return -1;
	}

	DBGP(LS_NETWORK_DAEMON,"Get ip from dhcp server successful.\n");
	while ( fgets(buf, sizeof(buf), fp) ) {

		decode_str(buf, key, value, "=");	

		if (!strcasecmp(key, "IPADDR")) {
			DBGP(LS_NETWORK_DAEMON,"New ip address: %s\n", value);
			sprintf(laninfo.ip, "%s", value);
			update = 1;
		} else if (!strcasecmp(key, "NETMASK")) {
			DBGP(LS_NETWORK_DAEMON,"      netmask2: %s\n", value);
			sprintf(laninfo.netmask, "%s", value);
			update = 1;
		} else if (!strcasecmp(key, "GATEWAY")) {
			DBGP(LS_NETWORK_DAEMON,"       gateway: %s\n", value);
			sprintf(laninfo.gateway, "%s", value);
			update = 1;
		} else if (!strcasecmp(key, "DNS")) {
			FILE* fpr;

			DBGP(LS_NETWORK_DAEMON,"    dns server: %s\n", value);

			if ( !value[0] ) 
				continue;

			p = value;
			q = strstr(p, ",");

			if ( (fpr = fopen("/tmp/resolv.conf", "w")) != NULL ) {
				int first=1;
				int second=1;
				while (p) {
					if(q) {
						*q='\0';
					}
					if(first==0 && second){
						sprintf(laninfo.dns1, "%s", p);
						second = 0;
					}
					if(first){
						sprintf(laninfo.dns, "%s", p);
						update = 1;
						first = 0;
					}
					fprintf(fpr, "nameserver %s\n", p);
					if(q){
						p = q+1;
						q = strstr(p, ",");
					}else{
						p = NULL;
					}
				} 
				fclose(fpr);
			}
		} 
	}
	if(update) securesoho_lan_set(&laninfo, 0);
	fclose(fp);
	return 0;
}

static int get_host_ip( char * dev_name , char * ip)
{
	int sockfd;
	struct ifreq ifr;

	sockfd = socket( AF_INET, SOCK_STREAM, 0 );
	if ( sockfd < 0 ) {
		strcpy( ip, "0.0.0.0" );
		return 0;
	}
	memset( &ifr, 0, sizeof(ifr) );
	strcpy( ifr.ifr_name, dev_name );

	if ( ioctl( sockfd, SIOCGIFADDR, &ifr ) < 0 ) {
		strcpy( ip, "0.0.0.0" );
		return 0;
	}

	strcpy( ip, inet_ntoa(((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr) );
	close(sockfd);

	return 1;
}

static int get_host_netmask( char * dev_name , char * ip)
{
	int sockfd;
	struct ifreq ifr;

	sockfd = socket( AF_INET, SOCK_STREAM, 0 );
	if ( sockfd < 0 ) {
		strcpy( ip, "127.0.0.1" );
		return 0;
	}
	memset( &ifr, 0, sizeof(ifr) );
	strcpy( ifr.ifr_name, dev_name );

	if ( ioctl( sockfd, SIOCGIFNETMASK, &ifr ) < 0 ) {
		strcpy( ip, "0.0.0.0" );
		return 0;
	}

	strcpy( ip, inet_ntoa(((struct sockaddr_in *)&ifr.ifr_netmask)->sin_addr) );
	close(sockfd);

	return 1;
}

static int get_host_destaddr( char * dev_name , char * ip)
{
	int sockfd;
	struct ifreq ifr;

	sockfd = socket( AF_INET, SOCK_STREAM, 0 );
	if ( sockfd < 0 ) {
		strcpy( ip, "0.0.0.0" );
		return 0;
	}
	memset( &ifr, 0, sizeof(ifr) );
	strcpy( ifr.ifr_name, dev_name );

	if ( ioctl( sockfd, SIOCGIFDSTADDR, &ifr ) < 0 ) {
		strcpy( ip, "0.0.0.0" );
		return 0;
	}

	strcpy( ip, inet_ntoa(((struct sockaddr_in *)&ifr.ifr_dstaddr)->sin_addr) );
	close(sockfd);

	return 1;
}

static int cfg_network_save_pppoeinfo( const char* ifname)
{
	/* TODO */
	FILE *fp = NULL;
	char ip[16];
	char netmask[16];
	char gateway[16];
	SecureSOHO_LAN laninfo;
	int update = 0;

	securesoho_lan_get(&laninfo);

	memset(ip, 0, 16);
	memset(netmask, 0, 16);
	memset(gateway, 0, 16);
	if (get_host_ip(PPPOE_IF, ip))
	{
		sprintf(laninfo.ip, "%s", ip);
		update = 1;
	}
	if (get_host_netmask(PPPOE_IF, netmask))
	{
		sprintf(laninfo.netmask, "%s", netmask);
		update = 1;
	}
	if (get_host_destaddr(PPPOE_IF, gateway))
	{
		sprintf(laninfo.gateway, "%s", gateway);
		update = 1;
	}

	if ((fp = fopen(RESOLV_CONF_FILE, "r")) != NULL)
	{
		char buf[256];

		memset(buf, 0, 256);
		if (fgets(buf, sizeof(buf), fp) != NULL) {
			if (strlen(buf) > strlen("nameserver "))
			{
				char dns[16];
				memset(dns, 0, 16);
				snprintf(dns, sizeof(dns), "%s", buf+strlen("nameserver "));
				snprintf(laninfo.dns, strlen(dns), "%s", dns); /*removes the last '\n' */
				laninfo.dns[strlen(laninfo.dns)] = '\0';
				update = 1;
			}
		}
		memset(buf, 0, 256);
		if (fgets(buf, sizeof(buf), fp) != NULL) {
			if (strlen(buf) > strlen("nameserver "))
			{
				char dns1[16];
				memset(dns1, 0, 16);
				snprintf(dns1, sizeof(dns1), "%s", buf+strlen("nameserver "));
				snprintf(laninfo.dns1, strlen(dns1), "%s", dns1); /*removes the last '\n' */
				laninfo.dns1[strlen(laninfo.dns1)] = '\0';
				update = 1;
			}
		}
		fclose(fp);
	}

	if (update) securesoho_lan_set(&laninfo, 0);

	return 0;
}

static void cfg_network_if_conf( void )
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

static void cfg_network_ifconf_static( void )
{
	cfg_network_if_conf();
}

static void cfg_network_ifconf_dhcp( int timeout )
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	cfg_start_dhcp_client(timeout);
}

static void cfg_network_ifconf_pppoe( int timeout )
{
	cfg_start_pppoe_client(timeout);
}

static void cfg_network_task_init( void )
{
	memset(&network_task, 0, sizeof(struct network_task_t));

	network_task.timeout = DEFAULT_TIMEOUT_SECS;
	network_task.pid     = 0;
	sem_init(&network_task.lock, 0, 1);	
	network_task.restart = 0;
	network_task.thread_exit = 0;
	cfg_network_set_state(NETWORK_STATE_INIT);
}

int cfg_network_update_wireless_state(int success)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	time_t tm = time(NULL) - network_task.start_time;
	SecureSOHO_LAN laninfo;

	if (success) {
		securesoho_lan_get(&laninfo);
		if (LAN_DHCP == laninfo.lan_type){
			if (tm >= network_task.timeout) {
				cfg_network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
				DBGP(LS_NETWORK_DAEMON,"%s:%d, wireless timeout\n", __FUNCTION__, __LINE__);
			} else {
				cfg_network_ifconf_dhcp( network_task.timeout - tm );
				cfg_network_set_state(NETWORK_STATE_REQUESTING_IP);
			}
		} else {
			cfg_network_ifconf_static();
			cfg_network_set_state(NETWORK_STATE_DONE);
			my_system(SCRIPT_NETWORK_UP);
		}
	} else {
		cfg_network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
	}

	sem_wait(&network_task.lock);
	network_task.pid = 0;
	sem_post(&network_task.lock);

	return 0;
}

int cfg_network_init(void)
{
	cfg_network_task_init();
	return 0;
}

int cfg_network_restart(int timeout)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	SecureSOHO_LAN laninfo;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if (timeout > 0 )
		network_task.timeout = timeout;
	else
		network_task.timeout = DEFAULT_TIMEOUT_SECS;

	network_task.start_time = time(NULL);

	my_system(SCRIPT_NETWORK_DOWN);
	cfg_network_reset();

	securesoho_lan_get(&laninfo);

	/* printf("======================= laniffo.conf_type = %d ======================######################	\n", laninfo.con_type); */

	if ( laninfo.con_type == CON_WIRELESS ) {
		DBGP(LS_NETWORK_DAEMON,"======================= network_restart:start wireless thread ======================######################  \n");
		cfg_network_set_state(NETWORK_STATE_WIRELESS_CONNECTING);
		cfg_start_wireless_thread(); 
	} else {
		if(wlan && wlan->wlan_name[0]!=0){
			if (!is_nfsroot_running()){
				do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
			}
		}
		
		if (LAN_DHCP == laninfo.lan_type) {
			cfg_network_ifconf_dhcp(network_task.timeout);
			cfg_network_set_state(NETWORK_STATE_REQUESTING_IP);
		} else if (LAN_PPPOE == laninfo.lan_type) {
			cfg_network_ifconf_pppoe(network_task.timeout);
			cfg_network_set_state(NETWORK_STATE_REQUESTING_IP);
		} else {
			cfg_network_ifconf_static();
			cfg_network_set_state(NETWORK_STATE_DONE);
			my_system(SCRIPT_NETWORK_UP);
		}
	}

	return 0;
}

int cfg_network_cancel(void)
{
	my_system(SCRIPT_NETWORK_DOWN);

#ifdef CONF_DHCPCD
	cfg_stop_dhcp_client();
#endif
#ifdef CONF_PPPOE
	cfg_stop_pppoe_client();
#endif
	if (!is_nfsroot_running()){
		do_cmd(IFCONFIG, LAN_PORT, "0.0.0.0", "down", NULL);
	}
	cfg_network_set_state(NETWORK_STATE_DONE);
	return 0;
}

NETWORK_STATE cfg_network_check_set_dhcp( void )
{
	/* printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__); */

	int ready = 0;
	time_t tm;
	SecureSOHO_LAN lan;
	NETWORK_STATE  state = cfg_network_get_state();
	int dma_ip_type = -1;
	
	switch (state) {
	case NETWORK_STATE_WIRELESS_CONNECTING:
		tm = time(NULL) - network_task.start_time;

		if ( tm >= network_task.timeout ) {
			DBGP(LS_NETWORK_DAEMON, "Wireless timeout\n");
			cfg_stop_wireless_thread();
			cfg_network_set_state(NETWORK_STATE_WIRELESS_TIMEOUT);
		} 
		break;
	case NETWORK_STATE_REQUESTING_IP:
	case NETWORK_STATE_DONE:
		if (securesoho_lan_get(&lan)) {
			DBGP(LS_NETWORK_DAEMON, "##%s;%d, failed to securesoho_lan_get\n", __FUNCTION__, __LINE__);
			return 0;
		}
//		printf("\033[1;45m ===============  %s(), %d lan type = %d...================== \033[0m	 \n", __FUNCTION__, __LINE__, lan.lan_type);
		
		if (lan.lan_type == LAN_FIXEDIP) {
			securesoho_ip_type_get(&dma_ip_type);
			if( TYPE_STATIC != dma_ip_type)
				securesoho_ip_type_set(TYPE_STATIC);			
			ready = 1;
			break;
		}
		if ( exist(DHCP_FLAG_FILE) ) {
			securesoho_ip_type_get(&dma_ip_type);
			if( TYPE_DHCP != dma_ip_type)
				securesoho_ip_type_set(TYPE_DHCP);			
			DBGP(LS_NETWORK_DAEMON, "##%s;%d, dhcp ip got!\n", __FUNCTION__, __LINE__);
			unlink(DHCP_FLAG_FILE);
			unlink(AUTO_FLAG_FILE);
			unlink(ZEROCONF_INFO);
			cfg_network_save_dhcpinfo(lan.lan_ifname);

			ready = 1;
		} else if (exist(AUTO_FLAG_FILE) ) {
			securesoho_ip_type_get(&dma_ip_type);
			if( TYPE_AUTO_IP != dma_ip_type)
				securesoho_ip_type_set(TYPE_AUTO_IP);			
			DBGP(LS_NETWORK_DAEMON, "##%s;%d, auto ip got!\n", __FUNCTION__, __LINE__);
			unlink(AUTO_FLAG_FILE);

			cfg_network_save_autoinfo(lan.lan_ifname);

			ready = 1;
		} else if (exist(PPPOE_FLAG_FILE) ) {
			securesoho_ip_type_get(&dma_ip_type);
			if( TYPE_PPPOE != dma_ip_type)
				securesoho_ip_type_set(TYPE_PPPOE);			
			DBGP(LS_NETWORK_DAEMON, "##%s;%d, pppoe ip got!\n", __FUNCTION__, __LINE__);
			unlink(PPPOE_FLAG_FILE);

			cfg_network_save_pppoeinfo(lan.lan_ifname);

			ready = 1;
		}
		if (ready) {
			network_task.restart = 0;
			//cfg_network_if_conf();
			cfg_network_set_state(NETWORK_STATE_DONE);
			my_system(SCRIPT_NETWORK_UP);
		}
	default:
		break;
	}
//	printf("\033[1;45m ===============  %s(), %d network_task.state = %d ...================== \033[0m  \n", __FUNCTION__, __LINE__, network_task.state);

	return network_task.state;
}

int cfg_network_sync_restart( void )
{
	printf("\033[1;45m ===============##################   %s(), %d... restart ()================== \033[0m	 \n", __FUNCTION__, __LINE__);
	if (cfg_network_restart(0) == 0 )
		return 0;
	securesoho_ip_type_set(TYPE_UNKNOWN);
	do {
		if (cfg_network_check_set_dhcp() == NETWORK_STATE_DONE ) {
			DBGP(LS_NETWORK_DAEMON, "Network Enabled\n");
			return 1;
		} else	{
			DBGP(LS_NETWORK_DAEMON, "Detecting...\n");
			sleep(1);
		}
	} while(1);

	return 0;
}
