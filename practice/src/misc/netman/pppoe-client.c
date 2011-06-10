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

#define PPPOE_START	"/usr/sbin/pppoe-start"
#define PPPOE_STOP	"/usr/sbin/pppoe-stop"

#define PPPOE_TIMEOUT_FLAG   "/tmp/pppoe_timeout"
#define PPPOE_FLAG	      "/tmp/pppoe_connected"

#define PPPD_PID_FILE		"/var/run/pppoe.conf-pppoe.pid.pppd"
#define PPPOE_CONNECT_PID_FILE	"/var/run/pppoe.conf-pppoe.pid"
#define PPPOE_PID_FILE		"/var/run/pppoe.conf-pppoe.pid.pppoe"
#define PPPOE_START_PID_FILE	"/var/run/pppoe.conf-pppoe.pid.start"	
#define CUR_PID_FILE	      "/var/run/pppoe-client.pid"

#define DEFAULT_DEVICE_NAME		    "My MediaPlayer"
#define DEFAULT_PPPOE_RETRY_TIMEOUT	    300
#define DEFAULT_PPPOE_REQUEEST_TIMEOUT	    60

#define PPPOE_CONFIG_FILE	"/etc/ppp/pppoe.conf"
#define PPPOE_PAP_SECRETS_FILE	"/etc/ppp/pap-secrets"
#define PPPOE_CHAP_SECRETS_FILE	"/etc/ppp/chap-secrets" 

static char g_interface[16] = {0};
static char g_timeout[16] = {0};

static void usage()
{
	DBGP(LS_NETWORK_DAEMON, "pppoe-client v1.0 - Alphanetworks SecureSOHO\n");
	DBGP(LS_NETWORK_DAEMON,
		"Copyright (c) 2009 Alphanetworks Inc. All rights reserved.\n\n");
	DBGP(LS_NETWORK_DAEMON, "Usage: pppoe-client {interface} {timeout} {start|stop}\n");
}


static void set_pppoe_client_config()
{
	char username[31];
	char password[31];
	char buf[256];
	int  i = 0;
        const char *pppoe_conf[] = {
                "DEMAND=no",
                "DNSTYPE=SERVER",
                "DEFAULTROUTE=yes",
                "CONNECT_POLL=2",
                "ACNAME=",
                "SERVICENAME=",
                "PING=\\\".\\\"",
                "CF_BASE=`basename $CONFIG`",
                "PIDFILE=\\\"/var/run/camplayer_pppoe.pid\\\"",
                "SYNCHRONOUS=no",
                "CLAMPMSS=1412",
                "LCP_INTERVAL=20",
                "LCP_FAILURE=3",
                "FIREWALL=NONE",
                "LINUX_PLUGIN=",
                "PPPOE_EXTRA=\\\"\\\"",
                "PPPD_EXTRA=\\\"\\\"",
                NULL
        };

	tuple_t tp[3] = {
		{"PPPOE_USERNAME", username, TUPLE_STRING},
		{"PPPOE_PASSWORD", password, TUPLE_STRING},
		{NULL, NULL, 0}
	};
	username[0] = password[0] = '\0';

	/* 1. Get username/password */
	securesoho_values_get(tp);

	/* 2. Set pppoe.conf */
	memset(buf, 0, 256);
	sprintf(buf, "echo \"#PPPOE Config\" > %s", PPPOE_CONFIG_FILE);
	system(buf);

	memset(buf, 0, 256);
	sprintf(buf, "echo \"USER=%s\" >> %s", username, PPPOE_CONFIG_FILE);
	system(buf);

	memset(buf, 0, 256);
	sprintf(buf, "echo \"ETH=%s\" >> %s", LAN_PORT, PPPOE_CONFIG_FILE);
	system(buf);
	
	memset(buf, 0, 256);
	sprintf(buf, "echo \"CONNECT_TIMEOUT=%s\" >> %s", g_timeout, PPPOE_CONFIG_FILE);
	system(buf);

        while (pppoe_conf[i]) {
                memset(buf, 0, sizeof(buf));
                snprintf(buf, sizeof(buf)-1, "echo \"%s\" >> %s", pppoe_conf[i], PPPOE_CONFIG_FILE);
                system(buf);
                i++;
        }

	/* 3. Set username/password */
        memset(buf, 0, 256);
        sprintf(buf, "echo \"%s\t*\t\t%s \" > %s", username, password, PPPOE_PAP_SECRETS_FILE);
        system(buf);
        memset(buf, 0, sizeof(buf));
        sprintf(buf, "echo \"%s\t*\t\t%s \" > %s", username, password, PPPOE_CHAP_SECRETS_FILE);
        system(buf);

	system("sync");
}

static void do_start(const char *iface, 
		     const char *timeout)
{
	int ret, pppoe_timeout;

	set_pid_to_file(CUR_PID_FILE);

	if(NULL != timeout)
		pppoe_timeout = atoi(timeout);
	else
		pppoe_timeout = DEFAULT_PPPOE_REQUEEST_TIMEOUT;
	while (1) {
		/* delete all tag file */
		unlink(PPPOE_FLAG);
		/* start pppoecd process */
		system(PPPOE_START);

		ret = DEFAULT_PPPOE_RETRY_TIMEOUT;
		while(ret > 0){
			DBGP(LS_NETWORK_DAEMON, "\033[1;45m To call PPPOE timer remain... %d\033[0m\n", ret);
			sleep(10);
			ret -= 10;
		}
	}
	return;
}

static int do_stop()
{
	kill_pidfile_nowait(CUR_PID_FILE, 15);

	/* remove some flag */
	unlink(PPPOE_TIMEOUT_FLAG);

	system(PPPOE_STOP);

	return 0;
}

static void sig_term(int signo)
{
	unlink(CUR_PID_FILE);
	exit(signo);
}

int main(int argc, char *argv[])
{
	char g_timeout[16];
	char command[16];

#ifdef __DISABLE_DEBUG_OUTPUT__
	freopen("/dev/null", "a", stdout);
	freopen("/dev/null", "a", stderr);
#endif

	if(argc != 4){
		usage();
		exit(0);
	}
	
	signal(SIGTERM, sig_term);

	sprintf(g_interface, "%s", argv[1]);
	sprintf(g_timeout, "%s", argv[2]);
	sprintf(command, "%s", argv[3]);

	DBGP(LS_NETWORK_DAEMON, "pppoe-client xxxxxxxxxxxxxxxxx, %s, %s, %s\n", 
		argv[1], argv[2], argv[3]);

	set_pppoe_client_config();

	if (strcmp(command, "start") == 0) {
		while(ps_exist(CUR_PID_FILE)){
			DBGP(LS_NETWORK_DAEMON,"the current %s is not deed, please kill it firsty\n", argv[0]);
			do_stop();
			sleep(1);
		}
		do_start(g_interface, g_timeout);
	}
	
	if (strcmp(command, "stop") == 0) {
		do_stop();
	}

	return 0;
}
