#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <asm/types.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include "securesoho.h"
#include "util_wireless.h"
#include "driver_wext.h"

/**
 * Please refer to http://www.cisco.com/en/US/products/hw/routers/ps272/prod_configuration_basics09186a008073f6ec.html#wp1095363
 * for the frequency/channel table
 * But the wireless 11a freequency is 5.500~5.825Ghz, for example, D-Link 7100AP is up to 5.825/165
 */

static void wireless_driver_atheros_insert()
{
#ifdef CONFIG_WIRELESS_DEVICE_AR5413
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
#endif

	do_cmd("/sbin/insmod", "-f", "/lib/modules/wlan.o", NULL);
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ath_hal.o", NULL);
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ath_rate_atheros.o", NULL);
#ifdef CONF_WLAN_80211A_PASSIVE_SCAN 
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ath_dfs.o", NULL);
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ath_pci.o", "countrycode=840", NULL);
#else
	do_cmd("/sbin/insmod", "-f", "/lib/modules/ath_pci.o", NULL);
#endif
	do_cmd("/sbin/insmod", "-f", "/lib/modules/wlan_wep.o", NULL);
	do_cmd("/sbin/insmod", "-f", "/lib/modules/wlan_tkip.o", NULL);
	do_cmd("/sbin/insmod", "-f", "/lib/modules/wlan_ccmp.o", NULL);
#ifdef CONFIG_WIRELESS_DEVICE_AR5413
	do_cmd("/sbin/insmod", "-f", "/lib/modules/wlan_scan_sta.o",NULL);
	do_cmd("/usr/bin/wlanconfig", wlan->wlan_name, "create", "wlandev","wifi0","wlanmode","sta",NULL);
#endif
	//do_cmd(IWPRIV, wlan->wlan_name, "burst", "0", NULL);
	//do_cmd(IWPRIV, wlan->wlan_name, "wmm", "0", NULL);
	//do_cmd(IWPRIV, wlan->wlan_name, "xr", "0", NULL);
}
static void wireless_driver_atheros_down(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_atheros_remove(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_atheros_init()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	do_cmd(IWPRIV, wlan->wlan_name, "bgscan", "0", NULL);
}

static int wireless_driver_atheros_getlinkstatus(int *status)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	*status = wlan->wlan_linkstatus;
	
	return 0;
}

static int wireless_driver_atheros_get_signal_strength(int *signal)
{
	return wireless_driver_wext_get_signal_strength(signal);
}

static int do_run_wpa_supplicant(SecureSOHO_Wireless *conf, int timeout)
{
	FILE *fp;
	int ret=-1;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	/* Setting /etc/wpa_supplicant.conf */
	if ((fp = fopen("/tmp/wpa_supplicant.conf", "w+")) == NULL){
		printf("F:%s,%d, wpa_supplicant.conf can't be opened\n", __FUNCTION__, __LINE__);
		goto out;
	}

	printf("F:%s,%d, #Set Wpa_supplicant.conf\n", __FUNCTION__, __LINE__);
	fprintf(fp, "eapol_version=1\n");
	fprintf(fp, "ap_scan=1\n");
	fprintf(fp, "network={\n");
	fprintf(fp, "\t\tssid=\"%s\"\n", conf->ssid);
	fprintf(fp, "\t\tscan_ssid=1\n");
	if (strlen(conf->wpa_psk ) == 64){
		fprintf(fp, "\t\tpsk=%s\n", conf->wpa_psk);
	}else{
		fprintf(fp, "\t\tpsk=\"%s\"\n", conf->wpa_psk);
	}
	fprintf(fp, "}\n");
	fclose(fp);

	printf("F:%s,%d, # Start Wpa_Supplicant\n", __FUNCTION__, __LINE__);
	if ( exist("/tmp/wpaok") )
		unlink("/tmp/wpaok");
	
	do_cmd_bg(WPA_SUPPLICANT,"-i", wlan->wlan_name, "-c/tmp/wpa_supplicant.conf", NULL);
	
out:
	return ret;
}

static int wireless_driver_atheros_set_profile_wpa(SecureSOHO_Wireless *conf, int timeout)
{
	int setup_timeout = timeout+time(NULL);
        char *wireless_type[] = {"0", "1", "2", "3"}; //0 for automode, 1 for 11a, 2 for 11b, 3 for 11g
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if (ps_exist("/var/run/wpa_supplicant.pid")){
		printf("F:%s,%d, Kill wpa_supplicant \n", __FUNCTION__, __LINE__);
		kill_pidfile("/var/run/wpa_supplicant.pid", 15);
	}
		
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "mode", wireless_type[conf->wireless_type], NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "up", NULL);

	printf("# It's WPA-PSK \n ");
	printf("F:%s,%d, # *****Encryption WPA-PSK*****\n", __FUNCTION__, __LINE__);
	do_cmd(IWCONFIG, wlan->wlan_name, "mode", "Managed", NULL);
			
	do_cmd(IWPRIV, wlan->wlan_name, "authmode", "1", NULL);
			
	if((setup_timeout-time(NULL)) > 30)
		do_run_wpa_supplicant(conf, 30);
	else
		do_run_wpa_supplicant(conf, (setup_timeout-time(NULL)));

	return 0;
}

static int wireless_driver_atheros_set_profile_wep(SecureSOHO_Wireless *conf, int timeout)
{
	char buf[10];
	char key[4][32];
	int i;
	static int authmode=1;
        char *wireless_type[] = {"0", "1", "2", "3"}; //0 for automode, 1 for 11a, 2 for 11b, 3 for 11g
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if (ps_exist("/var/run/wpa_supplicant.pid")){
		printf("F:%s,%d, Kill wpa_supplicant \n", __FUNCTION__, __LINE__);
		kill_pidfile("/var/run/wpa_supplicant.pid", 15);
	}
		
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "mode", wireless_type[conf->wireless_type], NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "up", NULL);

	/* No-Encryption or WEP */
	printf("# It's not WPA-PSK Set Work mode\n");
	switch(conf->mode){
	case WIRELESS_INFRASTRUCTURE:
		do_cmd(IWCONFIG, wlan->wlan_name, "mode", "Managed", NULL);
		break;
	case WIRELESS_ADHOC:
		do_cmd(IWCONFIG, wlan->wlan_name, "mode", "Ad-Hoc", NULL);
		break;
	default:
		do_cmd(IWCONFIG, wlan->wlan_name, "mode", "Master", NULL);
		break;
	}

	do_cmd(IWCONFIG, wlan->wlan_name, "rate", "auto", NULL);

	if(conf->security_method == WIRELESS_ENCRYPT_NONE) {
		/* Disable  */
		printf("F:%s, %d, # ******Encryption Disable******\n", __FUNCTION__, __LINE__);
		do_cmd(IWCONFIG, wlan->wlan_name, "key", "off", NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "authmode", "1", NULL);
		if(conf->ssid != '\0') {
			/* 
			 * NOTICE: need set essid in last step,
			 *         because it will send request immediately 
			 */
			do_cmd(IWCONFIG, wlan->wlan_name, "essid", conf->ssid, NULL);
		}
	}else{
		/* WEP Encrytpion */
		printf("F:%s,%d, # *****Encryption WEP******\n", __FUNCTION__, __LINE__);
		for (i=0;i<4;i++)
			key[i][0]=0;
		if(conf->wep_hexkey[0] != '\0')
			strcpy(key[0], conf->wep_hexkey[0]);
		if(conf->wep_hexkey[1] != '\0')
			strcpy(key[1], conf->wep_hexkey[1]);
		if(conf->wep_hexkey[2] != '\0')
			strcpy(key[2], conf->wep_hexkey[2]);
		if(conf->wep_hexkey[3] != '\0')
			strcpy(key[3], conf->wep_hexkey[3]);
				
		printf("F:%s,%d, # Set DEF_KEY [#%d:%s]\n", __FUNCTION__, __LINE__, conf->wep_key_index, key[conf->wep_key_index]);
		
		if(authmode==1){
			/* open key */
			do_cmd(IWPRIV, wlan->wlan_name, "authmode", "1", NULL);
			authmode = 2;
		}else{
			/* share key */
			do_cmd(IWPRIV, wlan->wlan_name, "authmode", "2", NULL);
			authmode = 1;
		}

		if (strlen(key[conf->wep_key_index]) != 0){
			sprintf(buf, "[%d]", conf->wep_key_index + 1);
			do_cmd(IWCONFIG, wlan->wlan_name, "key", key[conf->wep_key_index], buf, NULL);
					
			do_cmd(IWCONFIG, wlan->wlan_name, "key", buf, NULL);
			/* 
			 * NOTICE: need set essid in last step,
			 *         because it will send request immediately 
			 */
			do_cmd(IWCONFIG, wlan->wlan_name, "essid", conf->ssid, NULL);
		}
	}

	return 0;
}

static int wireless_driver_atheros_scan(const char *ssid, int ssid_len)
{
	return wireless_driver_wext_scan(ssid, ssid_len);
}

struct SITE_OBJ *wireless_driver_atheros_get_scan_results(int *cnt, WIRELESS_MODE mode)
{
	return wireless_driver_wext_get_scan_results(cnt, mode);
}

void wireless_driver_atheros_reset()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	/* 
	 * wpa_supplicant is only started up by network_daemon in wpa-psk mode
	 * in wep mode, it will not startup.
	 * note: kill wpa_supplicant will cause network disconnect in wpa-psk mode 
	 */
	if (ps_exist("/var/run/wpa_supplicant.pid")){
		kill_pidfile("/var/run/wpa_supplicant.pid", 15);
	}

	/* use 0.0.0.0 to down wireless is a workaround, need more comments */
	do_cmd(IFCONFIG, wlan->wlan_name, "0.0.0.0", "down", NULL);
	/* set wireless mode to auto (11bg & 11a) */
	do_cmd(IWPRIV, wlan->wlan_name, "mode", "0", NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "up", NULL); /* after "up", IP address is still 0.0.0.0 */

	/* note: change wireless work mode will cause network disconnect */
	do_cmd(IWCONFIG, wlan->wlan_name, "mode", "Managed", NULL);
}

void wireless_driver_atheros_get_wpa_linkstate(int *state)
{
	if(exist("/tmp/wpaok")){
		printf("wpaok\n");
		*state = 1;
	} else {
		*state = 0;
	}
}

void wireless_driver_atheros_linkup()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	wlan->wlan_linkstatus = 1;
	wlan->wlan_linkup_time = time(NULL);
}

void wireless_driver_atheros_linkdown()
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	wlan->wlan_linkstatus = 0;
}

void wireless_driver_atheros_scan_completed()
{
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	wireless_driver_wext_scan_completed();

	if (wlan->wlan_scan_completed_cb)
		wlan->wlan_scan_completed_cb(wlan->wlan_user_data);
}

void wireless_driver_atheros_set_scan_completed_cb(void (*cb)(void *user_data), void *user_data)
{
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	wlan->wlan_scan_completed_cb 	= (void *)cb;
	wlan->wlan_user_data 		= user_data;
}
struct usb_id_s atheros_pci_id[] = {
	{0x168c,0x0000},
	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_atheros_ops = {
	.wlan_vendor		= "Atheros",
	.wlan_chip_id		= "5413",
	.wlan_usb_dongle	= 0,
	.wlan_addr		= atheros_pci_id,
	.wlan_name		= "eth1",
	.wlan_p2p_name		= "",
	.wlan_scan_delay	= 10,
	.wlan_associated	= 1,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 7,
	.wlan_rsn_ie_offset	= 7,
	.wlan_scan_event	= 1,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_atheros_insert,
	.wlan_remove		= wireless_driver_atheros_remove,
	.wlan_down		= wireless_driver_atheros_down,
	.wlan_init		= wireless_driver_atheros_init,
	.wlan_reset		= wireless_driver_atheros_reset,
	.wlan_scan		= wireless_driver_atheros_scan,
	.wlan_set_profile_wep	= wireless_driver_atheros_set_profile_wep,
	.wlan_set_profile_wpa	= wireless_driver_atheros_set_profile_wpa,
	.wlan_get_status	= wireless_driver_atheros_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_atheros_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_atheros_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_atheros_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_atheros_linkup,
	.wlan_linkdown		= wireless_driver_atheros_linkdown,
	.wlan_scan_completed	= wireless_driver_atheros_scan_completed,
	.wlan_set_scan_completed_cb = wireless_driver_atheros_set_scan_completed_cb,
};
