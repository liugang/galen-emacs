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
#include <linux/if_ether.h>
#include <linux/wireless.h>
#include <assert.h>
#include "securesoho.h"
#include "securesoho_network2.h"
#include "securesoho_wireless.h"
#include "util_wireless.h"
#include "driver_wext.h"
#include "driver_ralink.h"

#ifndef min
#define min(x,y) ((x)<(y))?(x):(y)
#endif

/*
 * got from MODULE/ap/ap_cfg.c 
 */
#define STR_P2p_CLIENT_PINCODE			"P2P Client PinCode		  "
#define STR_P2P_CLIENT_WPS_PROFILE_COUNT	"P2P Client WPS Profile Count	  = "

#define STR_PROFILE_INDEX_PREFIX		"Profile["

void wireless_driver_ralink_down(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
	do_cmd("/sbin/ifconfig", wlan->wlan_name, "down", NULL);
	if(wlan->wlan_p2p_name && wlan->wlan_p2p_name[0] != '\0') 
		do_cmd("/sbin/ifconfig", wlan->wlan_p2p_name, "down", NULL);
}

void wireless_driver_ralink_init(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	wlan->wlan_linkstatus = 0;
	
	if ((wlan->wlan_skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("%s: create socket fail\n", __FUNCTION__);
		return;
	}
	if (securesoho_nic_ifc_up(wlan->wlan_name, 20) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->wlan_name);
		return ;
	}
	if(wlan->wlan_p2p_name && wlan->wlan_p2p_name[0] != '\0') 
		do_cmd("/sbin/ifconfig", wlan->wlan_p2p_name, "up", NULL);
}

static int wireless_driver_ralink_getlinkstatus_wifi_direct(int *status)
{
	char buffer[IW_PRIV_SIZE_MASK];
	struct iwreq wrq;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	char *p2p_stat = NULL;
	char *p = NULL;
	char *end = NULL;
	char str_profile_count[32] = {0};

	sprintf(wrq.ifr_name, wlan->wlan_p2p_name);
	wrq.u.data.pointer = (caddr_t) buffer;
	if (ioctl(wlan->wlan_skfd, RTPRIV_IOCTL_STATISTICS, &wrq) < 0) {
		printf("%s: wlan_p2p_name=[%s]\n", __func__, wlan->wlan_p2p_name);
		perror("wireless_driver_ralink_getlinkstatus_wifi_direct");
		return -1;
	}
	if ((p2p_stat = strstr(buffer, STR_P2P_CLIENT_WPS_PROFILE_COUNT)) != NULL){
		p = p2p_stat + strlen(STR_P2P_CLIENT_WPS_PROFILE_COUNT);
		if( (end = strstr(p, "\n")) != NULL ) {
			strncpy(str_profile_count, p, end - p);
		}
		if (atoi(str_profile_count) != 0) {
			*status = 1;
		} else
			*status = 0;
		return 0;
	}
	return -1;
}

int wireless_driver_ralink_getlinkstatus(int *status)
{
	char buffer[IW_PRIV_SIZE_MASK];
	struct iwreq wrq;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if(securesoho_wireless_is_wifi_direct()) {
		return wireless_driver_ralink_getlinkstatus_wifi_direct(status);
	} else {
		sprintf(wrq.ifr_name, wlan->wlan_name);
		wrq.u.data.pointer = (caddr_t) buffer;
		wrq.u.data.flags   = SHOW_CONN_STATUS;
		if (ioctl(wlan->wlan_skfd, RTPRIV_IOCTL_SHOW, &wrq) < 0) {
			*status = 2;//network interface is down
			printf("wireless_driver_ralink_getlinkstatus: wlan_name=[%s]\n", wlan->wlan_name);
			perror("wireless_driver_ralink_getlinkstatus");
			*status = 2;
			return -1;
		}
		printf("%s : %d , buffer=%s \n", __FUNCTION__, __LINE__, buffer);
		if (strncmp(buffer, "Connected", strlen("Connected")) == 0)
			*status = 1;
		else
			*status = 0;
		return 0;
	}
}

int wireless_driver_ralink_getwpsstatus(int *status)
{
	printf("\033[1;45m ===============  %s(), %d... ================== \033[0m  \n", __FUNCTION__, __LINE__);

	char buffer[IW_PRIV_SIZE_MASK];
	struct iwreq wrq;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	sprintf(wrq.ifr_name, wlan->wlan_name);
	wrq.u.data.pointer = (caddr_t) buffer;
	if (ioctl(wlan->wlan_skfd, RTPRIV_IOCTL_STATISTICS, &wrq) < 0) {
		printf("wireless_driver_ralink_getwpsstatus: wlan_name=[%s]\n", wlan->wlan_name);
		perror("wireless_driver_ralink_getwpsstatus");
		return -1;
	}
	else if (strstr(buffer, "WPS Proceed. Please wait... \n"))
		*status = STATUS_WSC_IN_PROGRESS;
	else if (strstr(buffer, "WPS messages exchange successfully !!!\n"))
		*status = STATUS_WSC_CONFIGURED;
	else if (strstr(buffer, "WPS not used.\n"))
		*status = STATUS_WSC_NOTUSED;
	else if (strstr(buffer, "Too many PBC AP. Please wait... \n"))
		*status = STATUS_WSC_PBC_TOO_MANY_AP;
	else if (strstr(buffer, "No available PBC AP. Please wait... \n"))
		*status = STATUS_WSC_PBC_NO_AP;
	else if (strstr(buffer, "Proceed to get the Registrar profile. Please wait... \n"))
		*status = STATUS_WSC_PROCEED;
	else if (strstr(buffer, "WPS didn't complete !!!\n"))
		*status = STATUS_WSC_EAP_FAILED;
	else 
		*status = STATUS_WSC_UNKNOWN;
	printf("\033[1;45m ===============  %s(), %d... status = %s ================== \033[0m	\n", __FUNCTION__, __LINE__, buffer);

	return 0;
}

int wireless_driver_ralink_getpincode(char *pin, int size)
{
	char buffer[IW_PRIV_SIZE_MASK];
	struct iwreq wrq;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	char *pin_start = NULL;

	sprintf(wrq.ifr_name, wlan->wlan_name);
	wrq.u.data.pointer = (caddr_t) buffer;
	if (ioctl(wlan->wlan_skfd, RTPRIV_IOCTL_STATISTICS, &wrq) < 0) {
		printf("%s: wlan_name=[%s]\n", __func__, wlan->wlan_name);
		perror("wireless_driver_ralink_getpincode");
		return -1;
	}
	if ((pin_start = strstr(buffer, "RT2860 Linux STA PinCode\t")) != NULL){
		strncpy(pin, pin_start + strlen("RT2860 Linux STA PinCode\t"), min(size, 8));
		return 0;
	}
	return -1;
}

int wireless_driver_ralink_get_signal_strength(int *signal)
{
	securesoho_get_wireless_card();

	return wireless_driver_wext_get_signal_strength(signal);
}

void wireless_driver_ralink_reset(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	//do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "up", NULL);
	sleep(2);
}

int RTPRIV_SET_IOCTL(char *cmd)
{
	struct iwreq wrq;
	int skfd;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	if ((skfd = iw_sockets_open()) < 0) {
		perror("iw_sockets_open");
		return -1;
	}

	printf("(%s:%d) cmd=%s\n", __FUNCTION__, __LINE__, cmd);
	strcpy(wrq.ifr_name, wlan->wlan_name);
	wrq.u.data.length = strlen(cmd);
	wrq.u.data.pointer = (caddr_t)cmd;
	wrq.u.data.flags = 0;
	if (ioctl(skfd, RTPRIV_IOCTL_SET, &wrq) < 0) {
		perror("RTPRIV_IOCTL_SET");
		close(skfd);
		return -1;
	}

	close(skfd);
	return 0;
} 
  

int wireless_driver_ralink_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	int status = 0;
	char key[128], authmode[32], encryptype[32], ssid[128], wirelessmode[32];
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	printf("(%s:%d) ", __FUNCTION__, __LINE__);
	if(0 == securesoho_wireless_is_wifi_direct()) {
		do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
		if (securesoho_nic_ifc_up(wlan->wlan_name, 60) < 0) {
			printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->wlan_name);
			return -1;
		}
	}
	memset(authmode, 0, 32);
	memset(encryptype, 0, 32);
	memset(key, 0, 128);
	memset(ssid, 0, 128);
	memset(wirelessmode, 0, 32);
	if (conf->wireless_mode >= WIRELESS_LEGACY_11BG_MIXED && conf->wireless_mode <= WIRELESS_11AGN_MIXED)
		snprintf(wirelessmode, sizeof(wirelessmode), "WirelessMode=%d", conf->wireless_mode);
	else 
		snprintf(wirelessmode, sizeof(wirelessmode), "WirelessMode=%d", WIRELESS_11ABGN_MIXED);

	if (conf->wps_mode == WIRELESS_WPS_NONE){
		switch (conf->encrypt_type) {
		case WIRELESS_ENCRYPT_NONE:
			snprintf(authmode, 32, "%s", "AuthMode=OPEN");
			snprintf(encryptype, 32, "%s", "EncrypType=NONE");
			break;
		case WIRELESS_ENCRYPT_WEP:
			snprintf(authmode, 32, "%s", "AuthMode=WEPAUTO");
			snprintf(encryptype, 32, "%s", "EncrypType=WEP");
			snprintf(key, 128, "Key1=%s", conf->wep_hexkey[0]);
			break;
		case WIRELESS_ENCRYPT_TKIP:
			if (conf->auth_mode == WIRELESS_AUTH_WPAPSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
			}else if (conf->auth_mode == WIRELESS_AUTH_WPA2PSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
			}
			snprintf(encryptype, 32, "%s", "EncrypType=TKIP");
			snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
			break;
		case WIRELESS_ENCRYPT_AES:
			if (conf->auth_mode == WIRELESS_AUTH_WPAPSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
			}else if (conf->auth_mode == WIRELESS_AUTH_WPA2PSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
			}
			snprintf(encryptype, 32, "%s", "EncrypType=AES");
			snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
			break;
		default:
			snprintf(authmode, 32, "%s", "AuthMode=OPEN");
			snprintf(encryptype, 32, "%s", "EncrypType=NONE");
			break;
		}
		snprintf(ssid, 128, "SSID=%s", conf->ssid);
		do_cmd(IWPRIV, wlan->wlan_name, "set", "NetworkType=Infra", NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", authmode, NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", encryptype, NULL);
		if (conf->encrypt_type == WIRELESS_ENCRYPT_WEP){
			do_cmd(IWPRIV, wlan->wlan_name, "set", "DefaultKeyID=1", NULL);
			do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
		}else if (conf->encrypt_type == WIRELESS_ENCRYPT_TKIP || 
				conf->encrypt_type == WIRELESS_ENCRYPT_AES)
		{
			/**
			 * FIXME:
			 * From the ralink driver document, section 4.4.1
			 * 2009_0521_RT3572_Linux_STA_v2.1.2.0_ReleaseNote_Alpha.pdf
			 * The ssid is set twice for WPA auth mode, the reason is still
			 * unknown. We set ssid twice for safety.
			 */
			do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
			do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
		}
		do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
	} else if (conf->wps_mode == WIRELESS_WPS_PIN){
		char buff[256];
		char wsc_ssid[32 + 3]; //32 + "" + \0
		snprintf(wsc_ssid, sizeof(wsc_ssid), "%s%s%s", "\"", conf->ssid, "\"");
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_stop");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_conf_mode 1");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_mode 1");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s %s", wlan->wlan_name, "wsc_ssid", wsc_ssid);
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_start");
		system(buff);
	} else if (conf->wps_mode == WIRELESS_WPS_PBC){
		char buff[256];

		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_stop");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_conf_mode 1");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_mode 2");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_name, "wsc_start");
		system(buff);
	} else if (conf->wps_mode == WIRELESS_WIFI_DIRECT_WPS_PBC){
		char buff[256];

		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_p2p_name, "set p2pScan=0");
		system(buff);
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_p2p_name, "set p2pScan=1");
		system(buff);
	} else {
		fprintf(stderr, "Wrong WPS mode %d\n", conf->wps_mode);
	}

	return (int)status;
}
		     
int check_wep_key(char *str, int len)
{	    
	int i = 0;

	for (i=0; i < len; i++) {
		if ((str[i] < 0x21) ||
				(str[i] > 0x7E))
			return 0;
	}

	return 1;
}

static void str_ascii_to_hex(const char *ascii_str, char *hex_str)
{			
	int len, i;	

	hex_str[0] = '\0';	
	if(ascii_str && hex_str){
		len = strlen(ascii_str);

		for(i=0; i<len; i++){
			sprintf(hex_str+2*i, "%02x", (unsigned char)ascii_str[i]);
		}	
	}			       
}   

static int parse_profile_line(const char *data, const char *lable, char *value)
{
	char *p = NULL;
	char key[128], buffer[256];

	value[0] = '\0';

	if (!data) 
		return -1;

	if ((p = strstr(data, "\n")) != NULL) {
		strncpy(buffer, data, p - data);	
		buffer[p -data] = '\0';
	} else {
		strncpy(buffer, data, sizeof(buffer));
	}

	decode_str(buffer, key, value, "=");
	if (!strncmp(key, lable, sizeof(lable)))
		return 0;

	return -1;
}

/*
 * Sample:
 * sh-3.00# iwpriv p2p0 stat
 * p2p0	     stat:
 * Tx success			     = 1510
 * Tx retry count		   = 0
 * Tx fail to Rcv ACK after retry  = 0
 * RTS Success Rcv CTS		   = 0
 * RTS Fail Rcv CTS		   = 0
 * Rx success			   = 5797
 * Rx with CRC			   = 19082
 * Rx drop due to out of resource  = 0
 * Rx duplicate frame		   = 3
 * False CCA (one second)	   = 0
 * RSSI-A			   = -15
 * RSSI-B (if available)	   = 0
 * RSSI-C (if available)	   = 0

 * WPS Information:
 * P2P Client PinCode		    52434891
 * P2P Client WPS Profile Count	    = 1
 * Profile[0]:
 * SSID				   = DIRECT-Mc
 * AuthType			   = WPA2PSK
 * EncrypType			   = AES
 * KeyIndex			   = 1
 * Key				   = 0FC74398947FDEFCD85D047499A23293CC1FC6624CA0B42BDF3E6F4763F59393@
 */
int wireless_driver_ralink_get_profile(SecureSOHO_Wireless *conf)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	int ret;
	int arg_count = 0, profile_count = 0, i = 0;;
	char *data = NULL, *p = NULL;
	char *delim = "\n";
	int skfd;
	char value[128];
	int is_wifi_direct = 0;
	SecureSOHO_Wireless w;
	char str_wps_profile_count[128] = {0};
	char buff[256];

	fprintf(stderr, "start %s\n", __func__);
	if (!conf){
		fprintf(stderr, "%s:%d the conf is null\n", __func__, __LINE__);
		return -1;
	}
	memset(conf, '\0', sizeof(SecureSOHO_Wireless));

	memset(&w, 0, sizeof(SecureSOHO_Wireless));
	securesoho_wireless_get(&w);

	is_wifi_direct = securesoho_wireless_is_wifi_direct();

	if(is_wifi_direct) {
		conf->wps_mode = w.wps_mode;
	}

	if ((skfd = iw_sockets_open()) < 0) {
		printf("(%s:%d) iw_sockets_open() fail.\n", __FUNCTION__, __LINE__);
		return -1;
	}
	if(is_wifi_direct) {
		ret = set_private(skfd, (char *)wlan->wlan_p2p_name, "stat", (void *)&data, arg_count, NULL);
	} else {
		ret = set_private(skfd, (char *)wlan->wlan_name, "stat", (void *)&data, arg_count, NULL);
	}
	if (ret == -1) {
		fprintf(stderr, "wrong stat ioctl command\n");
		close(skfd);
		return -1;
	}

	/*
	 * Sample:
	 * sh-3.00# iwpriv ra0 stat
	 * ra0	     stat:
	 *
	 * Tx success			   = 19200
	 * Tx retry count			     = 4
	 * Tx fail to Rcv ACK after retry  = 0
	 * RTS Success Rcv CTS		   = 0
	 * RTS Fail Rcv CTS		   = 0
	 * Rx success			   = 41268
	 * Rx with CRC			   = 284595
	 * Rx drop due to out of resource  = 0
	 * Rx duplicate frame		   = 2
	 * False CCA (one second)	   = 0
	 * RSSI-A			   = -15
	 * RSSI-B (if available)	   = -99
	 * RSSI-C (if available)	   = -99
	 *
	 * WpaSupplicantUP		   = 0
	 *
	 * RT2860 Linux STA PinCode	   03021859
	 * WPS Information(Driver Auto-Connect is Enabled - 2):
	 * PIN - WPS Proceed. Please wait... 
	 *
	 * WPS Profile Count		   = 1
	 * Profile[0]:
	 * SSID				   = dlink12345678 111
	 * MAC				   = 00:1B:11:69:FE:3D
	 * AuthType			   = OPEN
	 * EncrypType			   = WEP
	 * KeyIndex			   = 1
	 * Key				   = 1111111111
	 */

	if(is_wifi_direct) {
		snprintf(str_wps_profile_count, sizeof(str_wps_profile_count), "%s", "P2P Client WPS Profile Count");
	} else {
		snprintf(str_wps_profile_count, sizeof(str_wps_profile_count), "%s", "WPS Profile Count");
	}
	p = strtok(data, (char *)delim); /* skip the 'ra0/p2p0 stat' message */
	while (p) {
		p = strtok(NULL, (char *)delim); /* skip next line */
		if (p && strstr(p, str_wps_profile_count)) {
			break;
		}
	}
	if (!p){
		free(data);
		close(skfd);
		fprintf(stderr, "No profile found\n");
		return -1; /* no wireless profile found */

	}
	fprintf(stderr, "profile data:%s\n", (const char *)p);
	parse_profile_line(p, str_wps_profile_count, value);
	profile_count = atoi(value);

	if (profile_count <= 0 || profile_count > 4){
		free(data);
		close(skfd);
		fprintf(stderr, "Not valid profile count:%d\n", profile_count);
		return -1; /* no wireless profile found */
	}

	fprintf(stderr, "profile count:%d\n", profile_count);
	p = strtok(NULL, (char *)delim); /* skip the  profile count */

	if ( NULL != strncmp(p, STR_PROFILE_INDEX_PREFIX, strlen(STR_PROFILE_INDEX_PREFIX))) {
		while (p) {
			p = strtok(NULL, (char *)delim); /* skip next line */
			if (p && strstr(p, STR_PROFILE_INDEX_PREFIX)) {
				break;
			}
		}
	}
	/* 
	 * FIXME: the device now can only support one profile
	 */
	for (i = 0; i < profile_count && i < 1; i++){
		fprintf(stderr, "%s:%d %s\n", __func__, __LINE__, p);
		if (!strncmp(p, STR_PROFILE_INDEX_PREFIX, strlen(STR_PROFILE_INDEX_PREFIX))) {
			int key_idx = 0;

			fprintf(stderr, "Start new profile\n");

			/* parse the ssid line */
			p = strtok(NULL, (char *)delim); 
			if (p && parse_profile_line(p, "SSID", value) == 0) {
				fprintf(stderr, "The ssid is %s\n", value);
				strncpy(conf->ssid, value, sizeof(conf->ssid));
			}

			if(0 == is_wifi_direct) {
				/* parse the MAC line */
				p = strtok(NULL, (char *)delim); 
				if ( p && parse_profile_line(p, "MAC", value) == 0) {
					fprintf(stderr, "The mac is %s\n", value);
					strncpy(conf->bssid, value, sizeof(conf->bssid));
				}
			}

			/* parse the AuthType line */
			p = strtok(NULL, (char *)delim); 
			if ( p && parse_profile_line(p, "AuthType", value) == 0) { 
				fprintf(stderr, "The auth type is %s\n", value);
				if (strstr(value, "OPEN")) {
					conf->auth_mode = WIRELESS_AUTH_OPEN;
				} else if (strstr(value, "SHARED")) {
					conf->auth_mode = WIRELESS_AUTH_SHARED; //prefer to shared auth
				} else if (strstr(value, "WPAPSK")) {
					conf->auth_mode = WIRELESS_AUTH_WPAPSK;
				} else if (strstr(value, "WPA2PSK")) {
					conf->auth_mode = WIRELESS_AUTH_WPA2PSK;
				} 
			}

			/* parse the EncrypType line */
			p = strtok(NULL, (char *)delim); 
			if ( p && parse_profile_line(p, "EncrypType", value) == 0) { 
				if (strstr(value, "NONE")) {
					conf->encrypt_type = WIRELESS_ENCRYPT_NONE;
				} else if (strstr(value, "WEP")) {
					conf->encrypt_type = WIRELESS_ENCRYPT_WEP;
				} else if (strstr(value, "AES")) {
					conf->encrypt_type = WIRELESS_ENCRYPT_AES;
				} else if (strstr(value, "TKIP")) {
					conf->encrypt_type = WIRELESS_ENCRYPT_TKIP;
				} 
				fprintf(stderr, "The encrypt type is %s\n", value);
			}

			/* parse the KeyIndex line */
			p = strtok(NULL, (char *)delim); 
			if ( p && parse_profile_line(p, "KeyIndex", value) == 0) {
				key_idx = atoi(value);
				fprintf(stderr, "The key index is %d\n", key_idx);
			}

			/*
			 * Parse the Key line:
			 * FIXME: The key index of some APS are start with 0, 
			 * the others start with 1
			 */
			p = strtok(NULL, (char *)delim); 
			if ( p && parse_profile_line(p, "Key", value) == 0) { 
				fprintf(stderr, "The Key is %s\n", value);
				if (conf->auth_mode == WIRELESS_AUTH_WPAPSK ||
					conf->auth_mode == WIRELESS_AUTH_WPA2PSK){
					strncpy(conf->wpa_psk, value, sizeof(conf->wpa_psk));
				} else if (conf->auth_mode == WIRELESS_AUTH_OPEN ||
					conf->auth_mode == WIRELESS_AUTH_SHARED) {
					int len = strlen(value);
					if (len == 10) {	/* WEP 64HEX */
						conf->wep_encrypt_level = WIRELESS_WEP_ENCRYPT_LEVEL_64_HEX;
						strncpy(conf->wep_hexkey[0], value, 10);
						strncpy(conf->wep_hexkey[1], value, 10);
						strncpy(conf->wep_hexkey[2], value, 10);
						strncpy(conf->wep_hexkey[3], value, 10);
					} else if (len == 26) { /* WEP 128HEX */
						conf->wep_encrypt_level = WIRELESS_WEP_ENCRYPT_LEVEL_128_HEX;
						strncpy(conf->wep_hexkey[0], value, 26);
						strncpy(conf->wep_hexkey[1], value, 26);
						strncpy(conf->wep_hexkey[2], value, 26);
						strncpy(conf->wep_hexkey[3], value, 26);
					} else if (len == 5) {	/* WEP 64ASCII */
						conf->wep_encrypt_level = WIRELESS_WEP_ENCRYPT_LEVEL_64_ASCII;
						strncpy(conf->wep_asciikey[0], value, 5);
						strncpy(conf->wep_asciikey[1], value, 5);
						strncpy(conf->wep_asciikey[2], value, 5);
						strncpy(conf->wep_asciikey[3], value, 5);
						str_ascii_to_hex(conf->wep_asciikey[0], conf->wep_hexkey[0]);
						str_ascii_to_hex(conf->wep_asciikey[1], conf->wep_hexkey[1]);
						str_ascii_to_hex(conf->wep_asciikey[2], conf->wep_hexkey[2]);
						str_ascii_to_hex(conf->wep_asciikey[3], conf->wep_hexkey[3]);
					} else if (len == 13) {/* WEP 128ASCII */
						conf->wep_encrypt_level = WIRELESS_WEP_ENCRYPT_LEVEL_128_ASCII;
						strncpy(conf->wep_asciikey[0], value, 13);
						strncpy(conf->wep_asciikey[1], value, 13);
						strncpy(conf->wep_asciikey[2], value, 13);
						strncpy(conf->wep_asciikey[3], value, 13);
						str_ascii_to_hex(conf->wep_asciikey[0], conf->wep_hexkey[0]);
						str_ascii_to_hex(conf->wep_asciikey[1], conf->wep_hexkey[1]);
						str_ascii_to_hex(conf->wep_asciikey[2], conf->wep_hexkey[2]);
						str_ascii_to_hex(conf->wep_asciikey[3], conf->wep_hexkey[3]);
					} else {
						fprintf(stderr, "%s:%d Wrong key:%s", __func__, __LINE__, value);
					}
				}
				p = strtok(NULL, (char *)delim); /* skip key line */
			}
		}
	}

	printf("success to get wireless profile\n");
	free(data);
	close(skfd);
	if(is_wifi_direct) {
		snprintf(buff, sizeof(buff), "/usr/bin/iwpriv %s %s", wlan->wlan_p2p_name, "set p2pScan=0");
		system(buff);
	}
	return 0;
}

#if 0
int wireless_driver_ralink_do_assocate(void)
{
	char conf_essid[33], authmode[32], encryptype[32], ssid[128], key[128];
	int encrypt_type, auth_mode;
	char conf_key[65];
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	printf("(%s:%d) ", __FUNCTION__, __LINE__);

	bzero(authmode, sizeof(authmode));
	bzero(encryptype, sizeof(encryptype));
	bzero(ssid, sizeof(ssid));
	bzero(key, sizeof(key));

#ifdef CONF_WIRELESS_PROFILE_SUPPORT
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_SSID_%d", wlan->profile_index);
	securesoho_string_get(keyBuf, conf_essid);
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_ENCRYPT_TYPE_%d", wlan->
			profile_index);
	encrypt_type = securesoho_int_get(keyBuf);
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_AUTH_MODE_%d", wlan->
			profile_index);
	auth_mode = securesoho_int_get(keyBuf);
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_KEY_%d", wlan->profile_index);
	securesoho_string_get(keyBuf, conf_key);
#else
	securesoho_string_get("WIRELESS_SSID", conf_essid);
	encrypt_type = securesoho_int_get("WIRELESS_ENCRYPT_TYPE");
	auth_mode = securesoho_int_get("WIRELESS_AUTH_MODE");
	securesoho_string_get("WIRELESS_KEY", conf_key);
#endif

	do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
	if (securesoho_nic_ifc_up(wlan->wlan_name, 60) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->
				wlan_name);
		return -1;
	}

	printf("(%s:%d) encrypt_type=%d\n", __FUNCTION__, __LINE__, encrypt_type);
	switch (encrypt_type) {
		case WIRELESS_ENCRYPT_NONE:
			snprintf(authmode, 32, "%s", "AuthMode=OPEN");
			snprintf(encryptype, 32, "%s", "EncrypType=NONE");
			break;
		case WIRELESS_ENCRYPT_WEP:
			snprintf(authmode, 32, "%s", "AuthMode=WEPAUTO");
			snprintf(encryptype, 32, "%s", "EncrypType=WEP");
			snprintf(key, 128, "Key1=%s", conf->wep_hexkey[0]);
			break;
		case WIRELESS_ENCRYPT_TKIP:
			if (auth_mode == WIRELESS_AUTH_WPAPSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
			}else if (conf->auth_mode == WIRELESS_AUTH_WPA2PSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
			}
			snprintf(encryptype, 32, "%s", "EncrypType=TKIP");
			snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
			break;
		case WIRELESS_ENCRYPT_AES:
			if (auth_mode == WIRELESS_AUTH_WPAPSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
			}else if (conf->auth_mode == WIRELESS_AUTH_WPA2PSK){
				snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
			}
			snprintf(encryptype, 32, "%s", "EncrypType=AES");
			snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
			break;
		default:
			snprintf(authmode, 32, "%s", "AuthMode=OPEN");
			snprintf(encryptype, 32, "%s", "EncrypType=NONE");
			break;
	}

	snprintf(ssid, sizeof(ssid), "SSID=%s", conf_essid);
	printf("(%s:%d) authmode=[%s]\n", __FUNCTION__, __LINE__, authmode);
	printf("(%s:%d) encryptype=[%s]\n", __FUNCTION__, __LINE__, encryptype);
	printf("(%s:%d) ssid=[%s]\n", __FUNCTION__, __LINE__, ssid);
	printf("(%s:%d) key=[%s]\n", __FUNCTION__, __LINE__, key);
#if 1
	do_cmd(IWPRIV, wlan->wlan_name, "set", "NetworkType=Infra", NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "set", authmode, NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "set", encryptype, NULL);
#else
	RTPRIV_SET_IOCTL(wlan, "NetworkType=Infra");
	RTPRIV_SET_IOCTL(wlan, authmode);
	RTPRIV_SET_IOCTL(wlan, encryptype);
#endif

	if (encrypt_type == WIRELESS_ENCRYPT_WEP) {
#if 1
		do_cmd(IWPRIV, wlan->wlan_name, "set", "DefaultKeyID=1", NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
#else
		RTPRIV_SET_IOCTL(wlan, "DefaultKeyID=1");
		RTPRIV_SET_IOCTL(wlan, key);
#endif
	}
#if 1
	do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
#else
	RTPRIV_SET_IOCTL(wlan, ssid);
#endif

	if (encrypt_type == WIRELESS_ENCRYPT_WPAPSK_TKIP || 
			encrypt_type == WIRELESS_ENCRYPT_WPAPSK_AES ||
			encrypt_type == WIRELESS_ENCRYPT_WPA2PSK_TKIP || 
			encrypt_type == WIRELESS_ENCRYPT_WPA2PSK_AES) 
	{
#if 1
		do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
#else
		RTPRIV_SET_IOCTL(wlan, key);
		RTPRIV_SET_IOCTL(wlan, ssid);
#endif
	}

	return 0;
}
#endif

int wireless_driver_ralink_scan(const char *ssid, int ssid_len)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	if (securesoho_nic_ifc_up(wlan->wlan_name, 60) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->
				wlan_name);
		return -1;
	}
	return wireless_driver_wext_scan(ssid, ssid_len);
}

static int clear_string_end_space(char* str)
{
	int i = 0;
	if(NULL == str)
		return -1;
	for(i=strlen(str)-1;str[i]==' '&&i>=0;i--)
		str[i] = '\0';
	return 0;
}

struct SITE_OBJ *wireless_driver_ralink_get_scan_results(int *cnt, 
		WIRELESS_MODE mode)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	struct SITE_OBJ *site = NULL, *retval = NULL;
	int ret;
	int count = 0, arg_count = 0;
	char *data = NULL, *p = NULL;
	char *delim = "\n";
	int skfd;

	if ((skfd = iw_sockets_open()) < 0) {
		printf("(%s:%d) iw_sockets_open() fail.\n", __FUNCTION__, __LINE__);
		return NULL;
	}
	ret = set_private(skfd, (char *)wlan->wlan_name, "get_site_survey", (void *)&data, arg_count, NULL);
	if(ret == -1)
		return NULL;
	if (strlen(data) > IW_SCAN_MAX_DATA) {
		printf("(%s:%d) buffer is not big enough!\n", __FUNCTION__, __LINE__);
		close(skfd);
		return NULL;
	}

	/* skip the 'ra0 get_site_survey' message */
	p = strtok(data, (char *)delim); 
	/* skip the title */
	p = strtok(NULL, (char *)delim); 
	while (p && count < MAX_SCAN_RESULT) {
		/* 
		 * 1   BUF2G 00:16:01:6f:06:af	 WPAPSK/TKIP		94	 11b/g/n NONE	In YES 
		 */
		char b1[4]={0}, b2[64]={0}, b3[32]={0}, b4[32]={0}, b5[32]={0},
			b6[32]={0}, b7[32]={0}, b8[8]={0}, b10[8]={0};

		/*
		 * string p is a AP info,the format is as follows:  
		 * property: 
		 * |channel|SSID|MAC|auth_mode|percentage|mode|not use|type|wps| | | | | | | | | | | 
		 * sample p: 
		 * |6|alpha-guest|00:22:b0:49:05:08|WPAPSK/TKIPAES|83|11b/g|NONE|In|NO| | | | | | | | | |
		 * length:   
		 * | 3 | 32 | 19 | 22 | 8 | 7 | 6 | 3 | 6 | | | | | | | | | | |
		 * start pos:
		 * | 0 | 4  | 37 | 57 |80 |89 |97 |104|108|115
		 */


		/*
		 * p+4 is the start pos of SSID, if the first character of SSID is space,
		 * the SSID is hidden, abandon it.
		 */
		if (p[4] == ' ') {	
			p = strtok(NULL, (char *)delim); 
			continue;
		}

		strncpy(b1,p,3);	/* get channel */
		strncpy(b2,p+4,32);	/* get SSID */
		strncpy(b3,p+37,19);	/* get MAC */
		strncpy(b4,p+57,22);	/* get auth_mode */
		strncpy(b5,p+80,8);	/* get percentage */
		strncpy(b6,p+89,7);	/* get mode */
		strncpy(b10,p+97,6);	/* not use */
		strncpy(b7,p+104,3);	/* get type */
		strncpy(b8,p+107,6);	/* get wps */

		
		clear_string_end_space(b1);
		clear_string_end_space(b2);
		clear_string_end_space(b3);
		clear_string_end_space(b4);
		clear_string_end_space(b5);
		clear_string_end_space(b6);
		clear_string_end_space(b10);
		clear_string_end_space(b7);
		clear_string_end_space(b8);

		if ((site = (struct SITE_OBJ *)malloc(sizeof(struct SITE_OBJ))) < 0) {
			printf("(%s:%d) Out of memory.\n", __FUNCTION__, __LINE__);
			break;
		}
		memset(site, 0, sizeof(struct SITE_OBJ));

		site->channel = atoi(b1);
		strncpy(site->essid, b2, sizeof(site->essid));
		strncpy(site->bssid, b3, sizeof(site->bssid));

		/*
		 * Encryption and Auth-mode selection
		 */
		{
			if (strstr(b4, "NONE")) {
				site->encrypt_type = WIRELESS_ENCRYPT_NONE;
				site->auth_mode = WIRELESS_AUTH_OPEN;
			} else if (strstr(b4, "WEP")) {
				site->encrypt_type = WIRELESS_ENCRYPT_WEP;
				site->auth_mode = WIRELESS_AUTH_SHARED; /* prefer shared auth */
			} else if (strstr(b4, "AUTOWEP")) {
				site->encrypt_type = WIRELESS_ENCRYPT_WEP;
				site->auth_mode = WIRELESS_AUTH_SHARED; /* prefer shared auth */
			} else if (strstr(b4, "WPAPSK/AES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES;
				site->auth_mode = WIRELESS_AUTH_WPAPSK;
			} else if (strstr(b4, "WPAPSK/TKIPAES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES; /* prefer AES to TKIP if auto */
				site->auth_mode = WIRELESS_AUTH_WPAPSK;
			} else if (strstr(b4, "WPAPSK/TKIP")) {
				site->encrypt_type = WIRELESS_ENCRYPT_TKIP;
				site->auth_mode = WIRELESS_AUTH_WPAPSK;
			} else if (strstr(b4, "WPA2PSK/AES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES;
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;
			} else if (strstr(b4, "WPA2PSK/TKIPAES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES; /* prefer AES to TKIP if auto */
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;
			} else if (strstr(b4, "WPA2PSK/TKIP")) {
				site->encrypt_type = WIRELESS_ENCRYPT_TKIP;
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;
			} else if (strstr(b4, "WPA1PSKWPA2PSK/AES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES;
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;
			} else if (strstr(b4, "WPA1PSKWPA2PSK/TKIPAES")) {
				site->encrypt_type = WIRELESS_ENCRYPT_AES; /* prefer AES to TKIP if auto */
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;   /* prefer WPA2 to WPA1 if audo */
			} else if (strstr(b4, "WPA1PSKWPA2PSK/TKIP")) {
				site->encrypt_type = WIRELESS_ENCRYPT_TKIP;
				site->auth_mode = WIRELESS_AUTH_WPA2PSK;
			}
		}
		site->signal_percentage = atoi(b5);


		/*
		 * wireless mode
		 */
		{
			if (strcasestr(b6, "11a/b/g/n")) {
				site->wireless_mode = WIRELESS_11ABGN_MIXED;
			} else if (strcasestr(b6, "11a/b/g")) {
				site->wireless_mode = WIRELESS_LEGACY_11ABG_MIXED;
			} else if (strcasestr(b6, "11b/g/n")) {
				site->wireless_mode = WIRELESS_11BGN_MIXED;
			} else if (strcasestr(b6, "11a/g/n")) {
				site->wireless_mode = WIRELESS_11AGN_MIXED;
			} else if (strcasestr(b6, "11b/g")) {
				site->wireless_mode = WIRELESS_LEGACY_11BG_MIXED;
			} else if (strcasestr(b6, "11g/n")) {
				site->wireless_mode = WIRELESS_11GN_MIXED;
			} else if (strcasestr(b6, "11a/n")) {
				site->wireless_mode = WIRELESS_11AN_MIXED;
			} else if (strcasestr(b6, "11a")) {
				site->wireless_mode = WIRELESS_LEGACY_11A_ONLY;
			} else if (strcasestr(b6, "11b")) {
				site->wireless_mode = WIRELESS_LEGACY_11B_ONLY;
			} else if (strcasestr(b6, "11g")) {
				site->wireless_mode = WIRELESS_LEGACY_11G_ONLY;
			} else if (strcasestr(b6, "11n")) {
				site->wireless_mode = WIRELESS_11N_ONLY;
			}
		}

		if (!strcmp(b7, "In")){
			site->wireless_type = WIRELESS_INFRASTRUCTURE;	
		}else
			site->wireless_type = WIRELESS_ADHOC;

		if (!strcmp(b8, "YES")){
			site->support_wps = 1;
		}else
			site->support_wps = 0;

		if (count == 0) {
			retval = site;
			retval->next = NULL;
		} else {
			site->next = retval;
			retval = site;
		}

		p = strtok(NULL, (char *)delim); //next line
		count++;
	}

	*cnt = count;
	printf("success to get the scan %d results\n", count);
	free(data);
	close(skfd);
	return retval;
}

void wireless_driver_ralink_get_wpa_linkstate(int *state)
{
	int status;
	securesoho_get_wireless_card();

	wireless_driver_ralink_getlinkstatus(&status);
	*state = status;

	return;
}

void wireless_driver_ralink_linkup(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	wlan->wlan_linkstatus = 1;
	wlan->wlan_linkup_time = time(NULL);
	printf("(%s:%d) Link up\n", __FUNCTION__, __LINE__);
	/* get the favorite profile */
}

void wireless_driver_ralink_linkdown(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	wlan->wlan_linkstatus = 0;
	printf("(%s:%d) Link down\n", __FUNCTION__, __LINE__);
}



