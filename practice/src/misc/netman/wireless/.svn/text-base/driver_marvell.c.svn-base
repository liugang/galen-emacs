#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#ifdef __x86__
#include <linux/if.h>
#else
#include <linux/if_ether.h>
#endif
#include <linux/wireless.h>
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "util_wireless.h"
#include "driver_wext.h"

static void wireless_driver_marvell_insert()
{
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ap8x-2.3.2.p5A-PIKA-s-8622.o", NULL);
}

static void wireless_driver_marvell_down(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_marvell_remove(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_marvell_init()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	wlan->wlan_linkstatus = 0;
	/* 
	 * Set Operation band
	 * iwpriv <ifname> opmode <mode>
	 * <mode> =  7: 802.11 b/g/n mixed
	 *<mode> = 12: 802.11 a/n mixed
	 */
	do_cmd(IWPRIV, "wdev0", "opmode", "7", NULL);

	/*
	 * Set WMM Mode
	 * iwpriv <ifname> wmm <wmm mode>
	 * <wmm mode> = 0: Disable WMM Support
	 * <wmm mode> = 1: Enable WMM Support
	 */
	do_cmd(IWPRIV, "wdev0", "wmm", "1", NULL);

	/*
	 * Set Channel Bandwidth(11n only)
	 * iwpriv <ifname> htbw <bandwidth>
	 * <bandwidth> = 0: Auto
	 * <bandwidth> = 1: Reserved
	 * <bandwidth> = 2: 20 MHz
	 * <bandwidth> = 3: 40 MHz
	 */
	do_cmd(IWPRIV, "wdev0", "htbw", "0", NULL);

	/*
	 * Set WMM Mode
	 * iwpriv <ifname> wmm <wmm mode>
	 * <wmm mode> = 0: Disable WMM Support
	 * <wmm mode> = 1: Enable WMM Support
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "wmm", "1", NULL);

	/*
	 * Set Client Operation mode
	 * iwpriv <ifname> stamode <mode>
	 * <mode> = 0: Client mode disable
	 * <mode> = 6: Auto 2.4 & 5 Ghz client mode
	 * <mode> = 7: Auto 2.4 Ghz client mode
	 * <mode> = 8: Auto 5 Ghz client mode
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "stamode", "6", NULL);

	/*
	 * Set Mac clone control
	 * iwpriv <ifname> macclone <value>
	 * <value> = 0: Disable
	 * <value> = 1: Enable(Default)
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "macclone", "0", NULL);
	
	/*
	 * Set Aggregation (A-MSDU)
	 * iwpriv <ifname> amsdu <aggregation mode>
	 * <aggregation mode> = 0: Disable Aggregation Support
	 * <aggregation mode> = 2: Auto Aggregaton (Max 8K/4K)
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "amsdu", "0", NULL);

	/*
	 * Turn on DFS mode to conform FCC requirement.
	 * iwpriv <ifname> 11hstamode <value>
	 * <value> = 0: turn off
	 * <value> = 1: turn on
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "11hstamode", "1", NULL);

	/*
	 * Set Rxpathopt for legacy 11A/G
	 * iwpriv <ifname> rxpathopt <value>
	 * <value> = 0: turn off
	 * <value> = 1 ~ 1500: turn on this feature. (500us is the optimized value after the testing)
	 */
	do_cmd(IWPRIV, wlan->wlan_name, "rxpathopt", "500", NULL);

	do_cmd(IFCONFIG, "wdev0", "up", NULL);
}

int wireless_driver_marvell_wpawpa2mode(int wpamode)
{
	int skfd, ret;
	int count = 1;
	int args[] = {1};

	if (( skfd = iw_sockets_open() ) < 0 ) {
		perror("socket error");
		return -1;
	}

	switch (wpamode) {
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
	case WIRELESS_ENCRYPT_WPAPSK_AES:
		args[0] = 1;
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
		args[0] = 2;
		break;
	default:
		args[0] = 0;
		break;
	}
	printf("(%s:%d) wpawpa2mode=\"%d\"\n", __FUNCTION__, __LINE__, (int)args[0]);
	ret = set_private(skfd, "wdev0sta0", "wpawpa2mode", NULL, count, (void *)args);
	close(skfd);
	
	return ret;
}

int wireless_driver_marvell_ciphersuite(int wpamode)
{
	int skfd, ret;
	int count = 1;
	char *args[] = {""};

	if (( skfd = iw_sockets_open() ) < 0 ) {
		perror("socket error");
		return -1;
	}

	switch (wpamode) {
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
		args[0] = "wpa tkip";
		break;
	case WIRELESS_ENCRYPT_WPAPSK_AES:
		args[0] = "wpa aes-ccmp";
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
		args[0] = "wpa2 tkip";
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
		args[0] = "wpa2 aes-ccmp";
		break;
	default:
		break;
	}
	printf("(%s:%d) ciphersuite=\"%s\"\n", __FUNCTION__, __LINE__, args[0]);
	ret = set_private(skfd, "wdev0sta0", "ciphersuite", NULL, count, (void *)args);
	close(skfd);
	
	return ret;
}

int wireless_driver_marvell_passphrase(int wpamode, char *key)
{
	int skfd, ret;
	int count = 1;
	char buf[128];
	char *args[] = {""};

	if (( skfd = iw_sockets_open() ) < 0 ) {
		perror("socket error");
		return -1;
	}

	switch (wpamode) {
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
	case WIRELESS_ENCRYPT_WPAPSK_AES:
		snprintf(buf, 128, "wpa %s", key);
		buf[strlen(key) + 4] = 0;
		args[0] = buf;
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
		snprintf(buf, 128, "wpa2 %s", key);
		buf[strlen(key) + 5] = 0;
		args[0] = buf;
		break;
	default:
		break;
	}
	printf("(%s:%d) passphrase=\"%s\"\n", __FUNCTION__, __LINE__, args[0]);
	ret = set_private(skfd, "wdev0sta0", "passphrase", NULL, count, (void *)args);
	close(skfd);
	
	return ret;
}

int wireless_driver_marvell_wpa(int mode, char *key)
{
	wireless_driver_marvell_wpawpa2mode(mode);
	wireless_driver_marvell_ciphersuite(mode);
	wireless_driver_marvell_passphrase(mode, key);

	return 0;
}

static int wireless_driver_marvell_getlinkstatus(int *status)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	*status = wlan->wlan_linkstatus;
	
	return 0;
}

static int wireless_driver_marvell_get_signal_strength(int *signal)
{
	return wireless_driver_wext_get_signal_strength(signal);
}

static int wireless_driver_marvell_set_ssid(const char *ssid, int ssid_len)
{
	return wireless_driver_wext_set_ssid(ssid, ssid_len);
}

static int wireless_driver_marvell_scan(const char *ssid, int ssid_len)
{
	return wireless_driver_wext_scan(ssid, ssid_len);
}

static int wireless_driver_marvell_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	int setup_timeout = timeout+time(NULL);
	int *status = 0, ret = 0, bmode = 0;
	char buf[128];
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	char *authmode[] = {"open", "restricted"};

	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
	memset(buf, 0, 128);

	/* Set SSID */
	PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) SSID=%s\n", __FUNCTION__, __LINE__, conf->ssid);
	if (strlen(conf->ssid) < 32 && strlen(conf->ssid) > 0) {
		wireless_driver_marvell_set_ssid(conf->ssid, strlen(conf->ssid));
	} else {
		PRINTDEBUGMSG(MSG_ERROR, "(%s:%d) length of ssid = %d\n", __FUNCTION__, __LINE__, strlen(conf->ssid));
	}

	/* Set Authentication mode and Encryption type */
	PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) security method=%d\n", __FUNCTION__, __LINE__, conf->security_method);
	switch (conf->security_method) {
	case WIRELESS_ENCRYPT_NONE:
		PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) WIRELESS_ENCRYPT_NONE\n", __FUNCTION__, __LINE__);
		/*
		 * Set WPA/WPA2 mode
		 * iwpriv <ifname> wpawpa2mode <mode>
		 * <mode> = 0: Disable WPA/WPA2
		 * <mode> = 1: WPA-PSK
		 * <mode> = 2: WPA2-PSK
		 */
		do_cmd(IWPRIV, wlan->wlan_name, "wpawpa2mode", "0", NULL);
		do_cmd(IWCONFIG, wlan->wlan_name, "key", "off", NULL);
		break;
	case WIRELESS_ENCRYPT_WEP:
		PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) WIRELESS_ENCRYPT_WEP\n", __FUNCTION__, __LINE__);
		/*
		 * Set WPA/WPA2 mode
		 * iwpriv <ifname> wpawpa2mode <mode>
		 * <mode> = 0: Disable WPA/WPA2
		 * <mode> = 1: WPA-PSK
		 * <mode> = 2: WPA2-PSK
		 */
		do_cmd(IWPRIV, wlan->wlan_name, "wpawpa2mode", "0", NULL);

		memcpy(buf, "[1]", 3);
		buf[3] = 0;
		do_cmd(IWCONFIG, wlan->wlan_name, "key", authmode[bmode], buf, conf->wep_hexkey[0], NULL);
		break;
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
	case WIRELESS_ENCRYPT_WPAPSK_AES:
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
		do_cmd(IWCONFIG, wlan->wlan_name, "key", "off", NULL);
		wireless_driver_marvell_wpa(conf->security_method, conf->wpa_psk);
		break;
	default:
		PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) WIRELESS_ENCRYPT_NONE\n", __FUNCTION__, __LINE__);
		break;
	}

	do_cmd(IWCONFIG, wlan->wlan_name, "commit", NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "up", NULL);
	wireless_driver_marvell_scan("", 0);

	do {
		sleep(1);
		if(setup_timeout >= time(NULL)){
			if ((ret = wireless_driver_marvell_getlinkstatus((void *)&status)) < 0) {
				status = 0;
			}
			PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) status=%d\n", __FUNCTION__, __LINE__, status);
		}
		/* Switch authentication mode between OPEN and Shared Key evey 10 seconds */
		if(conf->security_method == WIRELESS_ENCRYPT_WEP && (setup_timeout - time(NULL)) % 10 == 0){
			bmode = !bmode;
			do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
			do_cmd(IWCONFIG, wlan->wlan_name, "key", authmode[bmode], buf, conf->wep_hexkey[0], NULL);
			do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "up", NULL);
		}
	} while (ret == 0 && status == 0 && (setup_timeout >= time(NULL)));

	fprintf(stderr, ">>>>>>>>>>>>>> %s:%d, Wireless configuration:%s \n", __FUNCTION__, __LINE__, status? "Succeed":"Fail");
	return (int)status;
}

struct SITE_OBJ *wireless_driver_marvell_get_scan_results(int *cnt, WIRELESS_MODE mode)
{
	return wireless_driver_wext_get_scan_results(cnt, mode);
}

void wireless_driver_marvell_reset()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "up", NULL);
}

void wireless_driver_marvell_get_wpa_linkstate(int *state)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	if (wlan->wlan_linkstatus == 1 && (time(NULL) - wlan->wlan_linkup_time) > 3)
	{
		*state = 1;
	} else {
		*state = 0;
	}
}

void wireless_driver_marvell_linkup()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	wlan->wlan_linkstatus = 1;
	wlan->wlan_linkup_time = time(NULL);
}

void wireless_driver_marvell_linkdown()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	wlan->wlan_linkstatus = 0;
}

void wireless_driver_marvell_scan_completed()
{
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	wireless_driver_wext_scan_completed();

	if (wlan->wlan_scan_completed_cb)
		wlan->wlan_scan_completed_cb(wlan->wlan_user_data);
}

void wireless_driver_marvell_set_scan_completed_cb(void (*cb)(void *user_data), void *user_data)
{
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	wlan->wlan_scan_completed_cb 	= (void *)cb;
	wlan->wlan_user_data 		= user_data;
}
struct usb_id_s marvell_pci_id[] = {
	{0x11ab,0x002a},
	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_marvell_ops = {
	.wlan_vendor		= "Marvell",
	.wlan_chip_id		= "8363",
	.wlan_usb_dongle	= 0,
	.wlan_addr		= marvell_pci_id,
	.wlan_name		= "wdev0sta0",
	.wlan_p2p_name		= "",
	.wlan_scan_delay	= 10,
	.wlan_associated	= 1,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 9,
	.wlan_rsn_ie_offset	= 10,
	.wlan_scan_event	= 1,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_marvell_insert,
	.wlan_remove		= wireless_driver_marvell_remove,
	.wlan_down		= wireless_driver_marvell_down,
	.wlan_init		= wireless_driver_marvell_init,
	.wlan_reset		= wireless_driver_marvell_reset,
	.wlan_scan		= wireless_driver_marvell_scan,
	.wlan_set_profile_wep	= wireless_driver_marvell_set_profile,
	.wlan_set_profile_wpa	= wireless_driver_marvell_set_profile,
	.wlan_get_status	= wireless_driver_marvell_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_marvell_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_marvell_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_marvell_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_marvell_linkup,
	.wlan_linkdown		= wireless_driver_marvell_linkdown,
	.wlan_scan_completed	= wireless_driver_marvell_scan_completed,
	.wlan_set_scan_completed_cb = wireless_driver_marvell_set_scan_completed_cb,
};

#if 0
int main(int argc, char *args[])
{
	int i, cnt;
	int *status;
	struct SITE_OBJ *siteobj = NULL;
	
	if (securesoho_marvell_getlinkstatus((int *)&status)<0){
		printf("Get link status fail\n");
	} else {
		printf("Status = %d\n", (int)status);
	}

	if (securesoho_marvell_stascan() < 0){
		printf("do sta scan fail\n");
	} else {
		for (i = 0; i < 1; i++){
			printf("Sleep %d seconds\n", i+1);
			sleep(1);
		}
	}


	if ((siteobj = securesoho_marvell_getstascan(&cnt, WIRELESS_INFRASTRUCTURE)) == NULL){
		printf("Get sta scan fail\n");
	} else {
		printf("cnt = %d\n", cnt);
#if 1
		cnt = 1;
		while (siteobj != NULL) {
			printf("#%d\n", cnt++);
			printf("\tsiteobj->essid = @%s@\n", siteobj->essid);
			printf("\tsiteobj->bssid = %s\n", siteobj->bssid);
			printf("\tsiteobj->channel = %d\n", siteobj->channel);
			printf("\tsiteobj->signal_dbm = %d\n", siteobj->signal_dbm);
			printf("\tsiteobj->signal_percentage = %d\n", siteobj->signal_percentage);
			printf("\tsiteobj->encrypt= 0x%08x\n\n", siteobj->crypt);
			siteobj = siteobj->next;
		}
#endif
	}
	return 0;
}
#endif

