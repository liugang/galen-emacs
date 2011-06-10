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
#include <linux/if.h>
#include <linux/wireless.h>
#include <assert.h>
#include "securesoho.h"
#include "securesoho_nic_helper.h"
#include "securesoho_wireless.h"
#include "util_wireless.h"
#include "driver_wext.h"
#include "driver_ralink.h"

#define SSID_FIELD_LEN	32

enum {    
    SHOW_CONN_STATUS = 4,
    SHOW_DRVIER_VERION = 5,
};

static void wireless_driver_rt2870_insert(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	do_cmd("/sbin/insmod", "/lib/modules/rt2870sta.ko", NULL);
}

static void wireless_driver_rt2870_down(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	do_cmd("/sbin/ifconfig", wlan->wlan_name, "down", NULL);
}

static void wireless_driver_rt2870_remove(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	do_cmd("/sbin/ifconfig", wlan->wlan_name, "down", NULL);
	do_cmd("/sbin/rmmod", "rt2870sta", NULL);
	close(wlan->wlan_skfd);
}

static void wireless_driver_rt2870_init(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	wlan->wlan_linkstatus = 0;
	if ((wlan->wlan_skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("%s: create socket fail\n", __FUNCTION__);
		return;
	}
}

static int wireless_driver_rt2870_getlinkstatus(int *status)
{
	char buffer[IW_PRIV_SIZE_MASK] = {0};
	struct iwreq wrq;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	sprintf(wrq.ifr_name, wlan->wlan_name);
	wrq.u.data.pointer = (caddr_t) buffer;
	wrq.u.data.flags   = SHOW_CONN_STATUS;
//	printf("\033[1;42m debug at %s,%s,%d wlan->wlan_skfd:%d\033[0m\n",__FILE__,__FUNCTION__,__LINE__, wlan->wlan_skfd);
	if (ioctl(wlan->wlan_skfd, RTPRIV_IOCTL_SHOW, &wrq) < 0) {
	//	printf("wireless_driver_rt2870_getlinkstatus: wlan_name=[%s]\n", wlan->wlan_name);
		*status = 2;  //network interface is down
		perror("wireless_driver_rt2870_getlinkstatus");
		return -1;
	}
	if(!buffer) 
		return 0;
	if (strncmp(buffer, "Connected", strlen("Connected")) == 0){
		*status = 1;
	} else {
		*status = 0;
	}
	return 0;
}

static int wireless_driver_rt2870_get_signal_strength(int *signal)
{
	return wireless_driver_wext_get_signal_strength(signal);
}

void wireless_driver_rt2870_reset(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

//	do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
	do_cmd(IFCONFIG, wlan->wlan_name, "up", NULL);
}

static int RTPRIV_SET_IOCTL(char *cmd)
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
  
static int wireless_driver_rt2870_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	int setup_timeout = timeout+time(NULL);
	int status = 0, ret = 0;
	char key[128], authmode[32], encryptype[32], ssid[128];
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	printf("(%s:%d) ", __FUNCTION__, __LINE__);
	do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
//	do_cmd(IFCONFIG, wlan->wlan_name, "up", NULL);
	if (securesoho_nic_ifc_up(wlan->wlan_name, 60) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->wlan_name);
		return -1;
	}
	memset(authmode, 0, 32);
	memset(encryptype, 0, 32);
	memset(key, 0, 128);
	memset(ssid, 0, 128);
	switch (conf->security_method) {
	case WIRELESS_ENCRYPT_NONE:
		snprintf(authmode, 32, "%s", "AuthMode=OPEN");
		snprintf(encryptype, 32, "%s", "EncrypType=NONE");
		break;
	case WIRELESS_ENCRYPT_WEP:
		snprintf(authmode, 32, "%s", "AuthMode=WEPAUTO");
		snprintf(encryptype, 32, "%s", "EncrypType=WEP");
		snprintf(key, 128, "Key1=%s", conf->wep_hexkey[0]);
		break;
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
		snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
		snprintf(encryptype, 32, "%s", "EncrypType=TKIP");
		snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
		break;
	case WIRELESS_ENCRYPT_WPAPSK_AES:
		snprintf(authmode, 32, "%s", "AuthMode=WPAPSK");
		snprintf(encryptype, 32, "%s", "EncrypType=AES");
		snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
		snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
		snprintf(encryptype, 32, "%s", "EncrypType=TKIP");
		snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
		snprintf(authmode, 32, "%s", "AuthMode=WPA2PSK");
		snprintf(encryptype, 32, "%s", "EncrypType=AES");
		snprintf(key, 128, "WPAPSK=%s", conf->wpa_psk);
		break;
	default:
		snprintf(authmode, 32, "%s", "AuthMode=OPEN");
		snprintf(encryptype, 32, "%s", "EncrypType=NONE");
		break;
	}
	snprintf(ssid, 128, "SSID=%s", conf->ssid);
#if 0
	do_cmd(IWPRIV, wlan->wlan_name, "set", "NetworkType=Infra", NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "set", authmode, NULL);
	do_cmd(IWPRIV, wlan->wlan_name, "set", encryptype, NULL);
#else
	RTPRIV_SET_IOCTL("NetworkType=Infra");
	RTPRIV_SET_IOCTL(authmode);
	RTPRIV_SET_IOCTL(encryptype);
#endif

	if (conf->security_method == WIRELESS_ENCRYPT_WEP) {
#if 0
		do_cmd(IWPRIV, wlan->wlan_name, "set", "DefaultKeyID=1", NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
#else
		RTPRIV_SET_IOCTL("DefaultKeyID=1");
		RTPRIV_SET_IOCTL(key);
#endif
	}
#if 0
	do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
#else
	RTPRIV_SET_IOCTL(ssid);
#endif

	if (conf->security_method == WIRELESS_ENCRYPT_WPAPSK_TKIP || 
		conf->security_method == WIRELESS_ENCRYPT_WPAPSK_AES ||
		conf->security_method == WIRELESS_ENCRYPT_WPA2PSK_TKIP || 
		conf->security_method == WIRELESS_ENCRYPT_WPA2PSK_AES) 
	{
#if 0
		do_cmd(IWPRIV, wlan->wlan_name, "set", key, NULL);
		do_cmd(IWPRIV, wlan->wlan_name, "set", ssid, NULL);
#else
		RTPRIV_SET_IOCTL(key);
		RTPRIV_SET_IOCTL(ssid);
#endif
	}


	do {
		sleep(1);
		if(setup_timeout >= time(NULL)){
			if ((ret = wireless_driver_rt2870_getlinkstatus((void *)&status)) < 0) {
				status = 0;
			}
			PRINTDEBUGMSG(MSG_DMACFG, "(%s:%d) status=%d\n", __FUNCTION__, __LINE__, status);
		}
	} while (ret == 0 && status == 0 && (setup_timeout >= time(NULL)));

	printf(">>>>>>>>>>>>>> %s:%d, Wireless configuration:%s \n", __FUNCTION__, __LINE__, status? "Succeed":"Fail");
	return (int)status;
}
#if 0
static int wireless_driver_rt2870_do_assocate(void)
{
	char conf_essid[33], authmode[32], encryptype[32], ssid[128], key[128];
	int encryp_type;
	char conf_key[65];
	char keyBuf[32];
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	printf("(%s:%d) ", __FUNCTION__, __LINE__);

	bzero(authmode, sizeof(authmode));
	bzero(encryptype, sizeof(encryptype));
	bzero(ssid, sizeof(ssid));
	bzero(key, sizeof(key));

#ifdef CONF_WIRELESS_PROFILE_SUPPORT
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_SSID_%d", wlan->profile_index);
	securesoho_string_get(keyBuf, conf_essid);
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_ENCRYPT_TYPE_%d", wlan->profile_index);
	encryp_type = securesoho_int_get(keyBuf);
	snprintf(keyBuf, sizeof(keyBuf), "WIRELESS_KEY_%d", wlan->profile_index);
	securesoho_string_get(keyBuf, conf_key);
#else
	securesoho_string_get("WIRELESS_SSID", conf_essid);
	encryp_type = securesoho_int_get("WIRELESS_ENCRYPT_TYPE");
	securesoho_string_get("WIRELESS_KEY", conf_key);
#endif

	do_cmd(IFCONFIG, wlan->wlan_name, "down", NULL);
	if (securesoho_nic_ifc_up(wlan->wlan_name, 60) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->wlan_name);
		return -1;
	}

	printf("(%s:%d) encryp_type=%d\n", __FUNCTION__, __LINE__, encryp_type);
	switch (encryp_type) {
	case WIRELESS_ENCRYPT_NONE:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=OPEN");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=NONE");
		break;
	case WIRELESS_ENCRYPT_WEP:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=WEPAUTO");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=WEP");
		snprintf(key, sizeof(key), "Key1=%s", conf_key);
		break;
	case WIRELESS_ENCRYPT_WPAPSK_TKIP:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=WPAPSK");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=TKIP");
		snprintf(key, sizeof(key), "WPAPSK=%s", conf_key);
		break;
	case WIRELESS_ENCRYPT_WPAPSK_AES:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=WPAPSK");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=AES");
		snprintf(key, sizeof(key), "WPAPSK=%s", conf_key);
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_TKIP:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=WPA2PSK");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=TKIP");
		snprintf(key, sizeof(key), "WPAPSK=%s", conf_key);
		break;
	case WIRELESS_ENCRYPT_WPA2PSK_AES:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=WPA2PSK");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=AES");
		snprintf(key, sizeof(key), "WPAPSK=%s", conf_key);
		break;
	default:
	printf("(%s:%d)\n", __FUNCTION__, __LINE__);
		snprintf(authmode, sizeof(authmode), "%s", "AuthMode=OPEN");
		snprintf(encryptype, sizeof(encryptype), "%s", "EncrypType=NONE");
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

	if (encryp_type == WIRELESS_ENCRYPT_WEP) {
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

	if (encryp_type == WIRELESS_ENCRYPT_WPAPSK_TKIP || 
		encryp_type == WIRELESS_ENCRYPT_WPAPSK_AES ||
		encryp_type == WIRELESS_ENCRYPT_WPA2PSK_TKIP || 
		encryp_type == WIRELESS_ENCRYPT_WPA2PSK_AES) 
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
static int wireless_driver_rt2870_scan(const char *ssid, int ssid_len)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	return -1;
	if (securesoho_nic_ifc_up(wlan->wlan_name, 0) < 0) {
		printf("%s[%d] Enable %s interface fail.\n", __FUNCTION__, __LINE__, wlan->wlan_name);
		return -1;
	}
	return wireless_driver_wext_scan(ssid, ssid_len);
}

struct SITE_OBJ *wireless_driver_rt2870_get_scan_results(int *cnt, WIRELESS_MODE mode)
{
	int i, *iCount;
	struct SITE_OBJ *site = NULL, *retval = NULL;
	struct scan_result *pScan_result;
	char *ppScan_result; 
	ppScan_result = get_gArScan_result();
	iCount = get_giCount();
	*cnt = 0;
	for (i = 0; i< *iCount; i++) {
		pScan_result = (struct scan_result *)(ppScan_result + sizeof(struct scan_result) * i);
		if (pScan_result->ssid_len == 0){
			continue;
		}
		if ((site = (struct SITE_OBJ *)malloc(sizeof(struct SITE_OBJ))) < 0) {
			printf("(%s:%d) Out of memory.\n", __FUNCTION__, __LINE__);
			return NULL;
		}
		memset(site, 0, sizeof(struct SITE_OBJ));
		*cnt = *cnt + 1;
		memcpy(site->essid, pScan_result->ssid, pScan_result->ssid_len);
		site->essid[pScan_result->ssid_len] = 0;
		memcpy(site->bssid, pScan_result->bssid, 18);
		site->signal_percentage = pScan_result->signal_percentage;
		site->crypt = pScan_result->crypt;

		if (i == 0) {
			retval = site;
			retval->next = NULL;
		} else {
			site->next = retval;
			retval = site;
		}
	}
	return retval;
}

void wireless_driver_rt2870_get_wpa_linkstate(int *state)
{
	int status;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	wireless_driver_rt2870_getlinkstatus(&status);
	*state = status;

	return;
}

void wireless_driver_rt2870_linkup(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	wlan->wlan_linkstatus = 1;
	wlan->wlan_linkup_time = time(NULL);
	printf("(%s:%d) Link up\n", __FUNCTION__, __LINE__);
	/* get the favorite profile */
}

void wireless_driver_rt2870_linkdown(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	wlan->wlan_linkstatus = 0;
	printf("(%s:%d) Link down\n", __FUNCTION__, __LINE__);
}

void wireless_driver_rt2870_scan_completed(void)
{
	struct scan_result *pScan_result;
	char *ppScan_result;
	int ret;
	int arg_count = 0, count = 0, *iCount;
	char *p, *p2, *Enc, *data;
	char *delim = (char[]){0xa, 0};
	int skfd;
 	struct wireless_driver_ops *wlan;
	wlan = securesoho_get_wireless_card();
	ppScan_result = get_gArScan_result();
	iCount = get_giCount();
	memset(ppScan_result, 0, sizeof(struct scan_result)*MAX_SCAN_RESULT);
	if ((skfd = iw_sockets_open()) < 0) {
		printf("(%s:%d) iw_sockets_open() fail.\n", __FUNCTION__, __LINE__);
		return;
	}
	ret = set_private(skfd, (char *)wlan->wlan_name, "get_site_survey", (void *)&data, arg_count, NULL);
	if (strlen(data) > IW_SCAN_MAX_DATA) {
		printf("(%s:%d) buffer is not big enough!\n", __FUNCTION__, __LINE__);
		return;
	}
	p = data + 1;
	while (*p != 10) p++;
	data = p++;
	p = strtok(data, (char *)delim);
	while (p && count < MAX_SCAN_RESULT) {
		pScan_result = (struct scan_result *)(ppScan_result + sizeof(struct scan_result) * count);
		/* Channel */
		p2 = strstr(p, " ");
		if(!p2)
			break;
		*p2 = 0;
		/* SSID */
		memset(pScan_result->ssid, 0, 32);
		memcpy(pScan_result->ssid, p + 4 , SSID_FIELD_LEN);
		p2 = pScan_result->ssid + SSID_FIELD_LEN - 1;
		while (*p2 == ' ') p2--;
		pScan_result->ssid_len = p2 - pScan_result->ssid + 1;
		/* BSSID */
		memcpy(pScan_result->bssid, p + 37, 17);
		/* Encryption */
		Enc = p + 57;
		p2 = strstr(Enc, " ");
		*p2 = 0;
		if (strstr(Enc, "NONE")) {
			pScan_result->crypt = WIRELESS_ENCRYPT_NONE;
		} else {
			if (strstr(Enc, "WEP")) {
				pScan_result->crypt = WIRELESS_ENCRYPT_WEP;
			} else if (strstr(Enc, "AES")) {
				if (strstr(Enc, "WPA2PSK")) {
					pScan_result->crypt = WIRELESS_ENCRYPT_WPA2PSK_AES;
				} else {
					pScan_result->crypt = WIRELESS_ENCRYPT_WPAPSK_AES;
				}
			} else if (strstr(Enc, "TKIP")) {
				if (strstr(Enc, "WPA2PSK")) {
					pScan_result->crypt = WIRELESS_ENCRYPT_WPA2PSK_TKIP;
				} else {
					pScan_result->crypt = WIRELESS_ENCRYPT_WPAPSK_TKIP;
				}
			}
		} 
		/* Singal */
		p2 = strstr(p + 80, " ");
		*p2 = 0;
		pScan_result->signal_dbm = 0;
		pScan_result->signal_percentage = atoi(p+ 80);
		p = strtok(NULL, (char *)delim);
		count++;
	}
	*iCount = count;
}

void wireless_driver_rt2870_set_scan_completed_cb(void (*cb)(void *user_data), void *user_data)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	printf("\033[1;33m%s[%d]\033[0m\n", __FUNCTION__, __LINE__);
	wlan->wlan_scan_completed_cb 	= (void *)cb;
	wlan->wlan_user_data 		= user_data;
}

#define WIRELESS_RT2870_ENC_SUPPORTED	WIRELESS_ENCRYPT_NONE | WIRELESS_ENCRYPT_WEP |	\
					WIRELESS_ENCRYPT_WPAPSK_TKIP | WIRELESS_ENCRYPT_WPAPSK_AES |	\
					WIRELESS_ENCRYPT_WPA2PSK_TKIP | WIRELESS_ENCRYPT_WPA2PSK_AES

struct usb_id_s rt2870_usb_id[] = {
	{0x148F,0x2770}, /* Ralink */
	{0x148F,0x2870}, /* Ralink */
	{0x07B8,0x2870}, /* AboCom */
	{0x07B8,0x2770}, /* AboCom */
	{0x0DF6,0x0039}, /* Sitecom 2770 */
	{0x0DF6,0x003F}, /* Sitecom 2770 */
	{0x083A,0x7512}, /* Arcadyan 2770 */
	{0x0789,0x0162}, /* Logitec 2870 */
	{0x0789,0x0163}, /* Logitec 2870 */
	{0x0789,0x0164}, /* Logitec 2870 */
	{0x177f,0x0302}, /* lsusb */
	{0x0B05,0x1731}, /* Asus */
	{0x0B05,0x1732}, /* Asus */
	{0x0B05,0x1742}, /* Asus */
	{0x0DF6,0x0017}, /* Sitecom */
	{0x0DF6,0x002B}, /* Sitecom */
	{0x0DF6,0x002C}, /* Sitecom */
	{0x0DF6,0x002D}, /* Sitecom */
	{0x14B2,0x3C06}, /* Conceptronic */
	{0x14B2,0x3C28}, /* Conceptronic */
	{0x2019,0xED06}, /* Planex Communications, Inc. */
	{0x07D1,0x3C09}, /* D-Link */
	{0x07D1,0x3C11}, /* D-Link */
	{0x14B2,0x3C07}, /* AL */
	{0x050D,0x8053}, /* Belkin */
	{0x14B2,0x3C23}, /* Airlink */
	{0x14B2,0x3C27}, /* Airlink */
	{0x07AA,0x002F}, /* Corega */
	{0x07AA,0x003C}, /* Corega */
	{0x07AA,0x003F}, /* Corega */
	{0x1044,0x800B}, /* Gigabyte */
	{0x15A9,0x0006}, /* Sparklan */
	{0x083A,0xB522}, /* SMC */
	{0x083A,0xA618}, /* SMC */
	{0x083A,0x8522}, /* Arcadyan */
	{0x083A,0x7522}, /* Arcadyan */
	{0x0CDE,0x0022}, /* ZCOM */
	{0x0586,0x3416}, /* Zyxel */
	{0x0CDE,0x0025}, /* Zyxel */
	{0x1740,0x9701}, /* EnGenius */
	{0x1740,0x9702}, /* EnGenius */
	{0x0471,0x200f}, /* Philips */
	{0x14B2,0x3C25}, /* Draytek */
	{0x13D3,0x3247}, /* AzureWave */
	{0x083A,0x6618}, /* Accton */
	{0x15c5,0x0008}, /* Amit */
	{0x0E66,0x0001}, /* Hawking */
	{0x0E66,0x0003}, /* Hawking */
	{0x129B,0x1828}, /* Siemens */
	{0x157E,0x300E},	/* U-Media */
	{0x050d,0x805c},	
	{0x050d,0x815c},
	{0x1482,0x3C09}, /* Abocom*/
	{0x14B2,0x3C09}, /* Alpha */
	{0x04E8,0x2018}, /* samsung */
	{0x5A57,0x0280}, /* Zinwell */
	{0x5A57,0x0282}, /* Zinwell */
	{0x7392,0x7718},
	{0x7392,0x7717},
	{0x1737,0x0070}, /* Linksys WUSB100 */
	{0x1737,0x0071}, /* Linksys WUSB600N */
	{0x0411,0x00e8}, /* Buffalo WLI-UC-G300N*/
	{0x050d,0x815c}, /* Belkin F5D8053 */
	{0x100D,0x9031}, /* Motorola 2770 */
	{0x0DB0,0x6899},
	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_rt2870_ops = {
	.wlan_vendor		= "Ralink",
	.wlan_chip_id		= "2870",
	.wlan_usb_dongle	= 1,
	.wlan_addr		= rt2870_usb_id,
	.wlan_name		= "ra0",
	.wlan_p2p_name		= "p2p0",
	.wlan_scan_delay	= 10,
	.wlan_associated	= 1,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 9,
	.wlan_rsn_ie_offset	= 10,
	.wlan_scan_event	= 0,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_rt2870_insert,
	.wlan_remove		= wireless_driver_rt2870_remove,
	.wlan_down		= wireless_driver_rt2870_down,
	.wlan_init		= wireless_driver_rt2870_init,
	.wlan_reset		= wireless_driver_rt2870_reset,
	.wlan_scan		= wireless_driver_rt2870_scan,
	.wlan_set_profile_wep	= wireless_driver_rt2870_set_profile,
	.wlan_set_profile_wpa	= wireless_driver_rt2870_set_profile,
	.wlan_get_status	= wireless_driver_rt2870_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_rt2870_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_rt2870_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_rt2870_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_rt2870_linkup,
	.wlan_linkdown		= wireless_driver_rt2870_linkdown,
	.wlan_scan_completed	= wireless_driver_rt2870_scan_completed,
	.wlan_set_scan_completed_cb = wireless_driver_rt2870_set_scan_completed_cb,
};
