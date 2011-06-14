#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "securesoho.h"
#include "securesoho_wireless.h"

static void wireless_driver_dummy_down(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_dummy_remove(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}


static void wireless_driver_dummy_insert()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_dummy_init()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

int wireless_driver_dummy_wpa(int mode, char *key)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

static int wireless_driver_dummy_getlinkstatus(int *status)
{
//	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

static int wireless_driver_dummy_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

static int wireless_driver_dummy_scan(const char *ssid, int ssid_len)
{
//	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

struct SITE_OBJ *wireless_driver_dummy_get_scan_results(int *cnt, WIRELESS_MODE mode)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return NULL;
}

void wireless_driver_dummy_reset()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_dummy_get_wpa_linkstate(int *state)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

int wireless_driver_dummy_get_signal_strength(int *signal)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
	return -1;
}

void wireless_driver_dummy_linkup()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_dummy_linkdown()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_dummy_set_scan_completed_cb(void (*cb)(void *user_data), void *user_data)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_dummy_scan_completed()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}
struct usb_id_s dummy_id[] = {
	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_dummy_ops = {

	.wlan_vendor		= "Dummy",
	.wlan_chip_id		= "",
	.wlan_usb_dongle	= 0,
	.wlan_addr		= dummy_id,
	.wlan_name		= "",
	.wlan_p2p_name		= "",
	.wlan_scan_delay	= 1,
	.wlan_associated	= 1,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 0,
	.wlan_rsn_ie_offset	= 0,
	.wlan_scan_event	= 0,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_dummy_insert,
	.wlan_remove		= wireless_driver_dummy_remove,
	.wlan_down		= wireless_driver_dummy_down,
	.wlan_init		= wireless_driver_dummy_init,
	.wlan_reset		= wireless_driver_dummy_reset,
	.wlan_scan		= wireless_driver_dummy_scan,
	.wlan_set_profile_wep	= wireless_driver_dummy_set_profile,
	.wlan_set_profile_wpa	= wireless_driver_dummy_set_profile,
	.wlan_get_status	= wireless_driver_dummy_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_dummy_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_dummy_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_dummy_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_dummy_linkup,
	.wlan_linkdown		= wireless_driver_dummy_linkdown,
	.wlan_scan_completed	= wireless_driver_dummy_scan_completed,
	.wlan_set_scan_completed_cb = wireless_driver_dummy_set_scan_completed_cb,
};
