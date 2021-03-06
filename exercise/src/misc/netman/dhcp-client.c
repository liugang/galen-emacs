#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include "config_access.h"
#include "securesoho.h"

#define DHCPCD	"/sbin/dhcpcd"
#define ZCIP	"/usr/sbin/zcip"

#define DHCPCD_TIMEOUT_FLAG   "/tmp/dhcpcd_timeout"
#define DHCPIP_FLAG	      "/tmp/get_dhcpip"
#define AUTOIP_FLAG	      "/tmp/get_autoip"

#define DHCPCD_PID_FILE	      "/etc/dhcpc/dhcpcd.pid"
#define CUR_PID_FILE	      "/var/run/dhcp-client.pid"
#define ZCIP_PID_FILE	      "/var/run/zcip.pid"

#define DEFAULT_DEVICE_NAME		    "My MediaPlayer"
#define DEFAULT_DHCPCD_RETRY_TIMEOUT	    300
#define DEFAULT_DHCPCD_REQUEEST_TIMEOUT	    60

struct dhcp_client_conf_t {
	char *debug;
	char *set_dns_srv;
};

static char IFCONF_NIC[16] = {0};
static struct dhcp_client_conf_t dhcp_client_conf;

static void usage()
{
	DBGP(LS_NETWORK_DAEMON, "dhcp-client v1.0 - REDSonic SecureSOHO\n");
	DBGP(LS_NETWORK_DAEMON,
		"Copyright (c) 2001 REDSonic Inc. All rights reserved.\n\n");
	DBGP(LS_NETWORK_DAEMON, "Usage: dhcp-client {interface} {timeout} {start|stop}\n");
}


static void get_dhcp_client_config(struct dhcp_client_conf_t *conf)
{
	char buf1[BUFSIZ];
	char buf2[BUFSIZ];

	tuple_t tp[3] = {
		{"DEBUG", buf1, TUPLE_STRING},
		{"SET_DNS_SRV", buf2, TUPLE_STRING},
		{NULL, NULL, 0}
	};
	buf1[0] = buf2[0] = '\0';

	securesoho_values_get(tp);
	conf->debug = strdup(buf1);
	conf->set_dns_srv = strdup(buf2);
}

static void destroy_dhcp_client_config(struct dhcp_client_conf_t *conf)
{
	if(conf->debug) free(conf->debug);
	if(conf->set_dns_srv) free(conf->set_dns_srv);
}

static void do_start(const char *iface, 
		     const char *timeout)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	int ret, dhcp_timeout;
	char device_name[512] = { 0 };

	securesoho_string_get("DMA_NAME", device_name);
	if (device_name[0] == 0)
		strcpy(device_name, DEFAULT_DEVICE_NAME);

	set_pid_to_file(CUR_PID_FILE);

	/* kill dhcpcd and zcip firstly */
	kill_pidfile(DHCPCD_PID_FILE, 1);
	kill_pidfile(ZCIP_PID_FILE, SIGTERM);

	if(NULL != timeout)
		dhcp_timeout = atoi(timeout);
	else
		dhcp_timeout = DEFAULT_DHCPCD_REQUEEST_TIMEOUT;
	while (1) {
		char buf[6];

		/* delete all tag file */
		unlink(DHCPIP_FLAG);
		unlink(AUTOIP_FLAG);

		sprintf(buf, "%d", dhcp_timeout);

		/* start dhcpcd process */
		ret = do_cmd(DHCPCD, iface, "-l", "2592000", "-t", buf, "-h", device_name, NULL);
		printf("\033[1;45m =============== do_cmd  start dhcpcd process ...================== \033[0m  %s(), %d\n", __FUNCTION__, __LINE__);

		DBGP(LS_NETWORK_DAEMON, "\033[1;45m The dhcpcd exited...\033[0m\n");
#if 0
		/*
		 * For EM86xx platform, invoke the dhcpcd too much times may 
		 * case memory fragment, so we made some changes to avoid the
		 * exit of the dhcpcd.
		 */
		if(exist(DHCPCD_TIMEOUT_FLAG)){
			/* kill zcip process firstly */
			kill_pidfile(ZCIP_PID_FILE, SIGTERM);

			/* dhcpcd timeout, we need to do auto ip */
			DBGP(LS_NETWORK_DAEMON, "\033[1;45m Requesting Auto IP...\033[0m\n");
			do_cmd_bg(ZCIP, "-i", iface, NULL);
			unlink(DHCPCD_TIMEOUT_FLAG);
		}else{
			DBGP(LS_NETWORK_DAEMON, "\033[1;45m DHCPCD is Aborted... %d\033[0m\n", ret);
			break;
		}
#endif
		ret = DEFAULT_DHCPCD_RETRY_TIMEOUT;
		while(ret > 0){
			DBGP(LS_NETWORK_DAEMON, "\033[1;45m To call DHCPCD timer remain... %d\033[0m\n", ret);
			sleep(10);
			ret -= 10;
		}
	}
	return;
}

static int do_stop()
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	kill_pidfile_nowait(ZCIP_PID_FILE, SIGTERM);
	kill_pidfile_nowait(DHCPCD_PID_FILE, 1);
	kill_pidfile_nowait(CUR_PID_FILE, 15);

	/* remove some flag */
	unlink(DHCPCD_TIMEOUT_FLAG);
	unlink(DHCPIP_FLAG);
	unlink(AUTOIP_FLAG);
	return 0;
}

static void sig_term(int signo)
{
	unlink(CUR_PID_FILE);
	destroy_dhcp_client_config(&dhcp_client_conf);
	exit(signo);
}

static void sig_dhcpcd_handler(int signo)
{
	if (signo == SIGUSR1) {
		//fprintf(stderr, "F:%s,%d, get SIGUSR1\n", __FUNCTION__, __LINE__);
		/* timed out to get dhcp ip, invoke the zcip process */
		if( exist(DHCPCD_TIMEOUT_FLAG) ){
			if ( !exist(ZCIP_PID_FILE) ) {
				DBGP(LS_NETWORK_DAEMON, "\033[1;45m Requesting Auto IP...\033[0m\n");
				do_cmd_bg(ZCIP, "-i", IFCONF_NIC, NULL);
			}
			unlink(DHCPIP_FLAG);	       
			unlink(DHCPCD_TIMEOUT_FLAG);
		}
	} else {
		//fprintf(stderr, "F:%s,%d, get SIGUSR2\n", __FUNCTION__, __LINE__);
		/* got dhcp IP address, kill the zcip process */
		if ( exist(ZCIP_PID_FILE) ) {
			DBGP(LS_NETWORK_DAEMON, "\033[1;45m Kill zcip...\033[0m\n");
			kill_pidfile(ZCIP_PID_FILE, SIGTERM);
			unlink(AUTOIP_FLAG);
		}
		touch(DHCPIP_FLAG);
	}
}

int main(int argc, char *argv[])
{
	char str_timeout[16];
	char str_mode[16];

#ifdef __DISABLE_DEBUG_OUTPUT__
	freopen("/dev/null", "a", stdout);
	freopen("/dev/null", "a", stderr);
#endif

	if(argc != 4){
		usage();
		exit(0);
	}
	
	signal(SIGUSR1, sig_dhcpcd_handler);
	signal(SIGUSR2, sig_dhcpcd_handler);
	signal(SIGTERM, sig_term);

	sprintf(IFCONF_NIC, "%s", argv[1]);
	sprintf(str_timeout, "%s", argv[2]);
	sprintf(str_mode, "%s", argv[3]);

	DBGP(LS_NETWORK_DAEMON, "dhcp-client xxxxxxxxxxxxxxxxx, %s, %s, %s\n", 
		argv[1], argv[2], argv[3]);
	get_dhcp_client_config(&dhcp_client_conf);

	if (strcmp(str_mode, "start") == 0) {
		while(ps_exist(CUR_PID_FILE)){
			DBGP(LS_NETWORK_DAEMON,"the current %s is not deed, please kill it firsty\n", argv[0]);
			do_stop();
			sleep(1);
		}
		do_start(IFCONF_NIC, str_timeout);
	}
	
	if (strcmp(str_mode, "stop") == 0) {
		do_stop();
	}

	destroy_dhcp_client_config(&dhcp_client_conf);
	return 0;
}
