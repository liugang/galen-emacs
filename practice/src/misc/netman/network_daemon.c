#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/mount.h>
#include <linux/wireless.h>
#include <linux/rtnetlink.h>
#include <netinet/in.h>
#include <time.h>
#include <pthread.h>
#include <semaphore.h>

#include "iwevent.h"
#include "network_util.h"
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "securesoho_config.h"
#include "wireless_setup.h"
#include "event_proxy.h"
#include "udev_util.h"
#include "nm_rtnl.h"
#include <dbus/dbus.h>
#include "dbus_service.h"
#if !defined(CONF_LOG4C) && defined(CONF_GLIB_MAINLOOP)
#include <glib.h>
#endif

#define MYLOG_CATEGORY_NAME "net_daemon"
#include "mylog.h"

#define NETWORK_PID_FILE		   "/var/run/network_daemon.pid"
#define DEFAULT_NETWORK_PORT		   4455	
#define IFCONFIG			   "/sbin/ifconfig"

#ifndef CONF_ROOTFS_MODE
#define CONF_ROOTFS_MODE "cramfs"
#endif

typedef struct _network_processor_t{
	int connfd;
	struct _network_processor_t *next;
}network_processor_t;

sem_t g_cmd_lock;
int g_restart_network_timeout;
char g_network_cmd;
int g_network_daemon_inactive = 0;
int g_saved_network_link_status = 1;
int g_network_daemon_started = 0;
network_processor_t *processor_list=NULL;

int create_listen_server(int port)
{
	int fd, ra, flags;
	struct sockaddr_in sa;

	sa.sin_family = AF_INET;
	sa.sin_addr.s_addr = htonl(INADDR_ANY);
	sa.sin_port = htons(port);

	if ((fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		DBGP(LS_NETWORK_DAEMON, "Failed to get socket fd.\n");
		goto error_exit;
	}
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
		       (char*)&ra, sizeof(ra)) < 0) {
		DBGP(LS_NETWORK_DAEMON, "Set SockOpt SO_REUSEADDR failed!\n");
		goto error_exit;
	}
	if (bind(fd, (struct sockaddr *) &(sa), sizeof(sa)) < 0) {
		DBGP(LS_NETWORK_DAEMON, "network_daemon bind port %d failed!\n", port);
		goto error_exit;
	}
	if (listen(fd, 5) < 0) {
		DBGP(LS_NETWORK_DAEMON, "listen failed: %s.\n", strerror(errno));
		goto error_exit;
	}

	flags = fcntl(fd, F_GETFL, 0);
	fcntl(fd, F_SETFL, O_NONBLOCK|flags);

	return fd ;
error_exit:
	if (fd >= 0)
		close(fd);
	return -1;
}

static int insert_processor_list(int fd)
{
	network_processor_t *p=NULL, *node;
	
	node = (network_processor_t *)malloc(sizeof(network_processor_t));
	memset(node, '\0', sizeof(network_processor_t));
	
	node->connfd = fd;
	
	if(NULL == processor_list)
		processor_list = node;
	else{
		p = processor_list;
		while(p->next){ 
			p = p->next;
		}
		p->next = node;
	}
	return 0;
}

static int delete_processor_list(int fd)
{
	int found = 0;
	network_processor_t *p, *q=NULL;
	
	p = processor_list;
	while(p){
		if(p->connfd == fd){
			found = 1;
			break;
		}
		q = p;
		p = p->next;
	}
	if(found){
		if(NULL == q)
			processor_list = processor_list->next;
		else
			q->next = p->next;
		if(p) free(p);
	}
	return 0;
}

static int do_network_processor(int listenfd)
{
	int connfd;
	
	if ((connfd = accept(listenfd, NULL, NULL)) >= 0) {
		/* don't require to set the socket to no-block 
		 * flags = fcntl(connfd, F_GETFL, 0);
		 * fcntl(connfd, F_SETFL, O_NONBLOCK|flags);
		 */
		insert_processor_list(connfd);
		DBGP(LS_NETWORK_DAEMON, "a client connected:%d\n", connfd);
	} else {
		perror("accept error");
		return -1;
	}
	return connfd;
}

static int preselect_processor(fd_set *rset, int fd)
{
	FD_SET(fd, rset);

	return fd;
}

static int postselect_processor(fd_set *rset, int fd)
{
	char buf[16]={0};
	int ret, len, state, timeout;

	if(FD_ISSET(fd, rset)){
		ret = read(fd, buf, 5);
		if(0 == ret){
			DBGP(LS_NETWORK_DAEMON,"delete a client\n");
			delete_processor_list(fd);
			close(fd);
		}
		if(ret>0){
			/* printf("\033[1;45m ===============  %s(), %d... daemon command = %c ================== \033[0m	\n", __FUNCTION__, __LINE__, buf[0]); */

			switch(buf[0]){
			case NETWORK_COMMAND_RESTART: /* start network settings */
				memcpy(&len, buf+1, 4);
				if((len > 0) && (4 == len)){
					ret = read(fd, buf, 4);
					memcpy(&timeout, buf, 4);
				}
				DBGP(LS_NETWORK_DAEMON,"network_daemon) start network start timeout:%d\n", timeout);
				sem_wait(&g_cmd_lock);
				g_network_cmd = NETWORK_COMMAND_RESTART;
				g_restart_network_timeout = timeout;
				cfg_network_set_state(NETWORK_STATE_UNKNOWN);
				sem_post(&g_cmd_lock);

				/* set the network daemon started */
				g_network_daemon_started = 1;
				
				/* response */
				buf[0] = NETWORK_COMMAND_RESTART;
				len = 0;
				memcpy(buf+1, &ret, 4);
				ret = write(fd, buf, 5);
				DBGP(LS_NETWORK_DAEMON,"network_daemon) start network start response:%d\n", ret);
				break;
			case NETWORK_COMMAND_CANCEL: /* cancel network settings */
				cfg_network_cancel();
				/* response */
				buf[0] = NETWORK_COMMAND_CANCEL;
				len = 0;
				memcpy(buf+1, &ret, 4);
				ret = write(fd, buf, 5);
				DBGP(LS_NETWORK_DAEMON,"network_daemon) start network cancel response:%d\n", ret);
				break;
			case NETWORK_COMMAND_STATE: /* get network state */
				state = cfg_network_get_state();
				if(NETWORK_STATE_DONE == state){
					int ret;
					char lif[32];	
					securesoho_lif_get(lif);
					if(!strcmp(lif, LAN_PORT)){
						/* wired connection */
						if(g_saved_network_link_status == 0){
							/* wired cable is plug-off */
							state = NETWORK_STATE_CABLE_PLUGOFF;
						}else{
							/* wired cable is plug-in, so need more checking for the NIC interface */
							ret = securesoho_nic_ifc_status(lif);
							switch(ret){
							case 1:
								/* inteface down */
								state = NETWORK_STATE_INTEFACE_DOWN;
								break;
							case 2:
								/* invalid ip */
								state = NETWORK_STATE_INVALID_IP;
								break;
							default:
								break;
							}
						}
					}else /* if(!g_saved_network_link_status) */{
						/* wireless connection */
						
						/*TODO: need to check the wireless AP association 
						 * status in this case, and set state to 
						 * NETWORK_STATE_CABLE_PLUGOFF when the link is down
						 */
						int ap_status = -1;
						struct wireless_driver_ops * wlan_driver = securesoho_get_wireless_card();
						wlan_driver->wlan_get_status(&ap_status);
						if(ap_status == 0) {
							state = NETWORK_STATE_WIRELESS_TIMEOUT;
						}else if(ap_status == 2){//network interface is down
							state = NETWORK_STATE_INTEFACE_DOWN;
						}
					}
				}
				/* response */
				buf[0] = NETWORK_COMMAND_STATE;
				len = 4;
				memcpy(buf+1, &len, 4);
				memcpy(buf+5, &state, 4);
				ret = write(fd, buf, 9);
				//printf("network_daemon) start network state:%d\n", state);
				break;
			case NETWORK_COMMAND_DAEMON_ACTIVE:
				g_network_daemon_inactive = 0;
				/* response */
				buf[0] = NETWORK_COMMAND_DAEMON_ACTIVE;
				len = 0;
				memcpy(buf+1, &ret, 4);
				ret = write(fd, buf, 5);
				break;
			case NETWORK_COMMAND_DAEMON_INACTIVE:
				g_network_daemon_inactive = 1;
				/* response */
				buf[0] = NETWORK_COMMAND_DAEMON_INACTIVE;
				len = 0;
				memcpy(buf+1, &ret, 4);
				ret = write(fd, buf, 5);
				/* stop the dhcp client */
				cfg_stop_dhcp_client();
				break;
			default:
				fprintf(stdout, "network_daemon) read command:%d\n", (int)buf[0]);
				break;
			}
		}
	}
	return 0;
}

static void print_usage(int argc, char **argv)
{
	DBGP(LS_NETWORK_DAEMON,"Usage: %s [-s]\n", argv[0]);
	DBGP(LS_NETWORK_DAEMON,"  Option: NULL	-> no network setup at begin, only network daemon server \n");
	DBGP(LS_NETWORK_DAEMON,"	  -s	-> setup network firstly then network daemon server\n");
	return;
}

static void *nd_processor_thread(void *vp)
{
	securesoho_ip_type_set(TYPE_UNKNOWN);
	while(1){
		/* sleep 1 second */
		sleep(1);
		if(0 == g_network_daemon_inactive){
			/* check network settings */
			cfg_network_check_set_dhcp();
			switch(g_network_cmd){
			case NETWORK_COMMAND_RESTART:
				printf("\033[1;45m ===============  %s(), %d... start ()================== \033[0m  \n", __FUNCTION__, __LINE__);

				cfg_network_restart(g_restart_network_timeout);
				sem_wait(&g_cmd_lock);
				g_network_cmd = (char)0;
				sem_post(&g_cmd_lock);
				break;
			default:
				break;
			}
		}
	}
	return NULL;
}

void do_init1 (void) 
{
#ifndef __x86__
	int mass_product_test_procedure = 0;
#endif
	do_cmd("/sbin/ifconfig", "lo", "127.0.0.1", "up", NULL);
	/* mount /lib/modules */
	do_mount("/lib.modules.bin", "/lib/modules", CONF_ROOTFS_MODE, MS_MGC_VAL |MS_RDONLY, 0, 0, 1);
	/* mount /osd */
	do_mount("/osd.bin", "/osd", CONF_ROOTFS_MODE, MS_MGC_VAL |MS_RDONLY, 0, 0, 1);
	/* Increate the socket receive buffer from 64K to 1M" */
	my_system("echo 1048576 >/proc/sys/net/core/rmem_max");
	my_system("echo 1048576 >/proc/sys/net/core/rmem_default");
#ifndef __x86__
	securesoho_mass_product_test_procedure_get(&mass_product_test_procedure);
	if (mass_product_test_procedure)
		system("/usr/sbin/utelnetd -l /bin/sh &"); /* enable telnet for mass product. */
	else 
		system("killall -9 utelnetd"); /* disable telnet in normal running. */
#endif
}

void do_init2 (void)
{
}

int main(int argc, char **argv)
{
	int n, listenfd, current;
	fd_set rset;
	struct timeval tv;
	network_processor_t *p=NULL;
	pthread_t pid;
	struct udev_handle uh;
	struct nm_rtnl_handle rth;
	CONNECTION con;

#ifdef __DISABLE_DEBUG_OUTPUT__
	freopen("/dev/null", "a", stdout);
	freopen("/dev/null", "a", stderr);
#endif
	/*
	 * The mylog.h has used te glib function g_logv, which is not 
	 * thread-safe, so, we should call the g_thread_init to avoid 
	 * the random crash casued by the multi-thread race condition.
	 *
	 * Note: The original log4c function is thread safe.
	 */
#if !defined(CONF_LOG4C) && defined(CONF_GLIB_MAINLOOP)
	g_thread_init(NULL);
#endif

	/* init the debug utility */
	if (mylog_init()){
		printf("mylog_init() failed\n");
	}
	mylog_fatal("mylog_init() done\n");

	nm_dbus_srv_init();

	wlan_driver_init();

	/* Open rtnl handle */
	if(nm_rtnl_handle_init(&rth) < 0) {
		perror("Can't initialize rtnetlink handle");
		return(1);
	}

	/* Open udev handle */
	if(udev_handle_init(&uh) < 0) {
		perror("Can't initialize udev handle");
		return(1);
	}

	print_usage(argc, argv);

	if(ps_exist(NETWORK_PID_FILE)){
		DBGP(LS_NETWORK_DAEMON,"can't run %s twice\n", argv[0]);
	}
	
	do_init1();
	do_init2();
	
	set_pid_to_file(NETWORK_PID_FILE);
	listenfd = create_listen_server(DEFAULT_NETWORK_PORT);

	if(listenfd <= 0){
		DBGP(LS_NETWORK_DAEMON, "create listen server error\n");
		exit(0);
	}

	if(2 == argc){
		if (0 == strcmp(argv[1], "-s")){
			/* start network firstly */
			DBGP(LS_NETWORK_DAEMON,"start network ......\n");
			cfg_network_sync_restart();
			DBGP(LS_NETWORK_DAEMON,"start network ok!");
		} 
	}

	/* init the network task */
	cfg_network_init();

	/* init the cmd lock */
	sem_init(&g_cmd_lock, 0, 1);

	/* create network daemon processor thread */
	pthread_create(&pid, NULL, nd_processor_thread, NULL);
	
	/* network reset */
	cfg_network_reset();

	while (1){
		FD_ZERO(&rset);
		tv.tv_sec = 1; tv.tv_usec = 0;
		int nm_dbus_fd = nm_dbus_srv_get_fd();

		p = processor_list;
		while(p){
			preselect_processor(&rset, p->connfd);
			p = p->next;
		}
		
		FD_SET(listenfd, &rset);
		FD_SET(nm_rtnl_handle_get_fd(&rth), &rset);
		FD_SET(udev_handle_get_fd(&uh), &rset);
		if (nm_dbus_fd > 0) {
			FD_SET(nm_dbus_fd, &rset);
		}

		n = select(FD_SETSIZE, &rset, NULL, NULL, &tv);
		if (n < 0) {
			DBGP(LS_NETWORK_DAEMON, "select:%s\n", strerror(errno));
			goto error;
		}

		p = processor_list;
		while(p){
			postselect_processor(&rset, p->connfd);
			p = p->next;
		}
		if (FD_ISSET(listenfd, &rset)) {
			if (do_network_processor(listenfd) < 0){
				goto error;
			}
		}
		if(FD_ISSET(nm_rtnl_handle_get_fd(&rth), &rset)) {
			printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);
			nm_rtnl_handle_events(&rth);

		}

		if(FD_ISSET(udev_handle_get_fd(&uh), &rset))
			udev_handle_events(&uh);

		if (FD_ISSET(nm_dbus_fd, &rset)){
			mylog_info("handle dbus fd %d message", nm_dbus_fd);
			nm_dbus_srv_handler();
		}

		/* set the g_saved_network_link_status */
		current = securesoho_nic_check_ifc_link(LAN_PORT, strlen(LAN_PORT));
		if (1 == current && 0 == g_saved_network_link_status) {
			mylog_info("Ethernet Cable in\n", nm_dbus_fd);
#ifdef CONF_IO_DAEMON
			nm_dbus_signal_ioevents(IO_EVENT_ETHERNET_CABLE_IN);
			nm_dbus_signal_device_status(LAN_PORT,0x80|74);
#endif
		}else if (0 == current && 1 == g_saved_network_link_status){
			nm_dbus_signal_device_status(LAN_PORT, 0x80|75); //m for osde_network_link_down event
		}
		g_saved_network_link_status = current;
		
		/* to send wireless signal */
		securesoho_lif_type_get(&con);
		if ((CON_WIRELESS == con) && (NETWORK_STATE_DONE == cfg_network_get_state())){
			int signal = -1;
			signal = securesoho_get_wireless_signal();
			nm_dbus_signal_wireless_signal(signal);
		}
	}
	/* Explicitly call the mylog cleanup routine */
	if (mylog_fini()){
		printf("mylog_fini() failed");
	}
	nm_rtnl_handle_fini(&rth);
	udev_handle_fini(&uh);
	nm_dbus_srv_close();

	return 0;
error:
	nm_rtnl_handle_fini(&rth);
	udev_handle_fini(&uh);
	nm_dbus_srv_close();
	DBGP(LS_NETWORK_DAEMON, "network daemon error\n");
	/* Explicitly call the mylog cleanup routine */
	if (mylog_fini()){
		printf("mylog_fini() failed");
	}

	return -1;
}
