#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <net/route.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <linux/wireless.h>
#include <net/if_arp.h>
#include <linux/if_ether.h>
#include <errno.h>

#include "wireless_setup.h"
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "wireless_profile_access.h"
#include "config_access.h"
#include "network_util.h"
#include "dbus_service.h"

static int wireless_fail(int delay)
{
	int ret=-1, loop;
	int associated_try_count=3;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	int status;
	
	loop = delay;
	if(NULL == wlan) goto out;
	do{
		sleep(1);

		wlan->wlan_get_status(&status);
		if (status == wlan->wlan_associated){
			associated_try_count --;
			if(associated_try_count < 0){
				ret = 0;
				break;
			}
		}
		if(is_exit_thread())
                        break;
		ret = -1;
		loop --;
	}while (loop > 0);
out:
	return ret;
}

int wireless_setup_get_link_status(char *lif, int len)
{
	int ret;
	if(wireless_fail(2)==0)
		ret = 1;
	else
		ret = 0;
	return ret;
}

int wireless_setup_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	int fail = -1;
	int state = -1;
	int setup_timeout = 0;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if(NULL == wlan) 
		goto out;

	if (conf->wps_mode == WIRELESS_WPS_NONE)
		setup_timeout = timeout + time(NULL);
	else
		setup_timeout = 120 + time(NULL);

	if (conf->encrypt_type == WIRELESS_ENCRYPT_TKIP ||
			conf->encrypt_type == WIRELESS_ENCRYPT_AES)
		wlan->wlan_set_profile_wpa(conf, 0);
	else
		wlan->wlan_set_profile_wep(conf, 0);
	do {
		if (is_exit_thread()) {
			DBGP(LS_NETWORK_DAEMON,"F:%s,%d, Exit the set_wireless function \n", __FUNCTION__, __LINE__);
			break;
		}
		wlan->wlan_get_status(&state); //-1 for iface error, 1 for connected, 0 for discoonected
		if (state == 1){

			fail = 0;
			nm_dbus_signal_wireless_status("Connected");
			//FIXME, the new flow will save the profile from OSD
#if 0
			int count;
			wp_node head, *wpnode;
			/* add item to profile xml lists */	
			wireless_profile_init(&head, &count);
			if ((wpnode = (wp_node*)malloc(sizeof(wp_node))) == NULL) {
				fprintf(stderr, "malloc fail: %s\n", __FUNCTION__);
				break;;
			}
			bzero(wpnode, sizeof(wp_node));
			memcpy(&wpnode->w, conf, sizeof(SecureSOHO_Wireless));
			wireless_profile_add(&head, wpnode);
			wireless_profile_save(&head);
			wireless_profile_destroy(&head);
#endif
			break;
		}
		nm_dbus_signal_wireless_status("Connecting");

		sleep(1); //do the query every seconds

		if(setup_timeout < time(NULL)){
			fail = 1;
			nm_dbus_signal_wireless_status("Disconnected");
			break;
		}
	} while (1);

out:	
	DBGP(LS_NETWORK_DAEMON, ">>>>>>>>>>>>>> %s:%d, Wireless configuration:%s \n", __FUNCTION__, __LINE__, fail ? "Fail" : "Succeed");
	return !fail;
}
