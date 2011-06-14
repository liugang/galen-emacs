/* 
 * Copyright (C) 2006, Alphanetworks, inc.
 * Author: wills_yin@alphanetworks.com 
 * vim:cindent:ts=8:sw=8
 */
#ifndef __SECURESOHO_WIRELESS_H__
#define __SECURESOHO_WIRELESS_H__


#define IFCONFIG                      "/sbin/ifconfig"
#define IWCONFIG                      "/usr/bin/iwconfig"
#define IWPRIV                        "/usr/bin/iwpriv"
#define IWLIST                        "/usr/bin/iwlist"
#define WPA_SUPPLICANT                "/usr/bin/wpa_supplicant"

#define IW_MAX_PRIV_DEF	160
/* Some usefull constants */
#define KILO	1e3
#define MEGA	1e6
#define GIGA	1e9
/* For doing log10/exp10 without libm */
#define LOG10_MAGIC	1.25892541179

#define SSID_MAX_WPA_IE_LEN 	40
#define GENERIC_INFO_ELEM 	0xdd
#define RSN_INFO_ELEM 		0x30

/* ------------------------- PRIVATE INFO ------------------------- */
/*
 * The following is used with SIOCGIWPRIV. It allow a driver to define
 * the interface (name, type of data) for its private ioctl.
 * Privates ioctl are SIOCIWFIRSTPRIV -> SIOCIWLASTPRIV
 */

#define IW_PRIV_TYPE_MASK	0x7000	/* Type of arguments */
#define IW_PRIV_TYPE_NONE	0x0000
#define IW_PRIV_TYPE_BYTE	0x1000	/* Char as number */
#define IW_PRIV_TYPE_CHAR	0x2000	/* Char as character */
#define IW_PRIV_TYPE_INT	0x4000	/* 32 bits int */
#define IW_PRIV_TYPE_FLOAT	0x5000	/* struct iw_freq */
#define IW_PRIV_TYPE_ADDR	0x6000	/* struct sockaddr */

#define IW_PRIV_SIZE_FIXED	0x0800	/* Variable or fixed number of args */

#define IW_PRIV_SIZE_MASK	0x07FF	/* Max number of those args */

/**
 *    WEP validation funtion return value
 */

#define WEP_OK   0
#define WEP_INVALID_WEP		1
#define WEP_INVALID_WPAPSK	2
#define WEP_INVALID_CHARACTER	3
#define WEP_INVALID_64HEX	4
#define WEP_INVALID_128HEX	5
#define WEP_INVALID_64ASCII	6
#define WEP_INVALID_128ASCII	7

#define WIRELESS_SITESURVEY_CACHE_NUM		5

/* Wireless WEP encrypt level */
#define WIRELESS_WEP_ENCRYPT_LEVEL_DISABLE	0   
#define WIRELESS_WEP_ENCRYPT_LEVEL_64_HEX	1
#define WIRELESS_WEP_ENCRYPT_LEVEL_128_HEX	2
#define WIRELESS_WEP_ENCRYPT_LEVEL_64_ASCII	3
#define WIRELESS_WEP_ENCRYPT_LEVEL_128_ASCII	4

typedef enum{
	WIRELESS_ADHOC = 0,
	WIRELESS_INFRASTRUCTURE = 1,
	WIRELESS_MASTER = 2,
	WIRELESS_REPEATER = 3,
	WIRELESS_SECONDARY = 4,
	WIRELESS_MONITOR = 5,
	WIRELESS_AUTO = 6,
	WIRELESS_BOTH = 7,	/* It's means Ad-Hoc and Infrastrcture */
}WIRELESS_TYPE;

/*
0: legacy 11b/g mixed
1: legacy 11B only
2: legacy 11A only
3: legacy 11a/b/g mixed
4: legacy 11G only
5: 11ABGN mixed
6: 11N only
7: 11GN mixed
8: 11AN mixed
9: 11BGN mixed
10: 11AGN mixed
*/
typedef enum{
	WIRELESS_LEGACY_11BG_MIXED,
	WIRELESS_LEGACY_11B_ONLY,
	WIRELESS_LEGACY_11A_ONLY,
	WIRELESS_LEGACY_11ABG_MIXED,
	WIRELESS_LEGACY_11G_ONLY,
	WIRELESS_11ABGN_MIXED,
	WIRELESS_11N_ONLY,
	WIRELESS_11GN_MIXED,
	WIRELESS_11AN_MIXED,
	WIRELESS_11BGN_MIXED,
	WIRELESS_11AGN_MIXED,
}WIRELESS_MODE;

/*
{OPEN,SHARED,WEPAUTO,WPAPSK,WPA2PSK,WPANONE}
*/
typedef enum{
	WIRELESS_AUTH_OPEN,
	WIRELESS_AUTH_SHARED,
	WIRELESS_AUTH_WEPAUTO,
	WIRELESS_AUTH_WPAPSK,
	WIRELESS_AUTH_WPA2PSK,
	WIRELESS_AUTH_WPANONE,
}WIRELESS_AUTH_MODE;

/*
{NONE,WEP,TKIP,AES}
*/
typedef enum{
	WIRELESS_ENCRYPT_NONE,
	WIRELESS_ENCRYPT_WEP,
	WIRELESS_ENCRYPT_TKIP,
	WIRELESS_ENCRYPT_AES,
}WIRELESS_ENCRYPT_TYPE;

/*
{Manual, WPS/PBC, WPS/PIN}
*/
typedef enum{
	WIRELESS_WPS_NONE,
	WIRELESS_WPS_PIN,
	WIRELESS_WPS_PBC,
	WIRELESS_WPS_NFC,
	WIRELESS_WIFI_DIRECT_WPS_PBC,
	WIRELESS_WIFI_DIRECT_WPS_PIN,
}WIRELESS_WPS_MODE;


/*
 Key string                   {5 ascii characters or 10 hex number or
                              13 ascii characters or 26 hex numbers} 
*/
typedef enum{
	WIRELESS_KEYIDX_ONE,
	WIRELESS_KEYIDX_TWO,
	WIRELESS_KEYIDX_THREE,
	WIRELESS_KEYIDX_FOUR,
	WIRELESS_KEYIDX_MAX,
}WIRELESS_KEYIDX;

/*
 * WPS STATUS
*/
typedef enum{
	STATUS_WSC_IN_PROGRESS,
	STATUS_WSC_CONFIGURED,
	STATUS_WSC_NOTUSED,
	STATUS_WSC_PBC_TOO_MANY_AP,
	STATUS_WSC_PBC_NO_AP,
	STATUS_WSC_PROCEED,
	STATUS_WSC_EAP_FAILED,
	STATUS_WSC_UNKNOWN,
}WIRELESS_WPS_STATUS;

typedef struct {
	WIRELESS_MODE wireless_mode;
	WIRELESS_TYPE wireless_type;
	WIRELESS_AUTH_MODE auth_mode;
	WIRELESS_ENCRYPT_TYPE encrypt_type;
	WIRELESS_WPS_MODE wps_mode;
	char         ssid[33];
	char         bssid[18];
	int          channel;

	WIRELESS_KEYIDX  wep_key_index;
	int          wep_encrypt_level; /* 0: Disable, 1:64bit-Hex, 2:128bit-Hex, 3:64bit-Ascii, 4:128bit-Ascii */
	char         wep_hexkey[WIRELESS_KEYIDX_MAX][27];
	char         wep_asciikey[WIRELESS_KEYIDX_MAX][16];
	char         wpa_psk[65]; // 
	char         wps_pin[9];//4-8
} SecureSOHO_Wireless;

struct usb_id_s {
	unsigned int idVendor;
	unsigned int idProduct;
};

struct wireless_driver_ops {
	/*
	 * use flag 
	 */
	int        used;
	/**
	 * vender name
	 */
	const char *wlan_vendor;
	
	/**
	 * chip id
	 */
	const char *wlan_chip_id;
	
	/**
	 * address - address of mini pci card
	 */
//	const char *wlan_addr;
	struct usb_id_s *wlan_addr;

	/**
	 * devpath - USB device path
	 */
	char wlan_devpath[255];

	/**
	 * usb_dongle - usb interface
	 */
	int wlan_usb_dongle;

	/**
	 * interface name
	 */
	const char *wlan_name;
	
	/**
	 * interface name for p2p
	 */
	const char *wlan_p2p_name;
	/**
	 * delay time after send scan command
	 */
	const int wlan_scan_delay;
	
	/**
	 * associate value 
	 */
	const int wlan_associated;

	/**
	 * enc supported 
	 */
	int wlan_enc_supported;

	/**
	 * link up time
	 */
	int wlan_linkup_time;

	/**
	 * link status
	 */
	int wlan_linkstatus;

	/**
	 * User Data for scan completed call back function
	 */
	void *wlan_user_data;

	/**
	 * 
	 */
	int wlan_wpa_ie_offset;

	/**
	 * 
	 */
	int wlan_rsn_ie_offset;

	/**
	 * Wireless driver support scan completed event or not.
	 */
	int wlan_scan_event;
	
	/**
	 * socket for ioctl
	 */
	int wlan_skfd;
	
	/**
	 * site survey completed call back function
	 */
	void (*wlan_scan_completed_cb)(void (*cb)(void *UserData));

	/**
	 * insert - insert driver
	 */
	void (*wlan_insert)(void);

	/**
	 * remove - remove driver
	 */
	void (*wlan_remove)(void);

	/**
	 * down - ifconfig down driver
	 */
	void (*wlan_down)(void);


	/**
	 * init - initialize driver interface
	 */
	void (*wlan_init)(void);

	/**
	 * reset - down then up wireless interface
	 */
	void (*wlan_reset)(void);

	/**
	 * scan - request the driver to initiate scan
	 * @priv: private driver interface data
	 * @ssid: specific SSID to scan for (ProbeReq) or %NULL to scan for
	 *	all SSIDs (either active scan with broadcast SSID or passive
	 *	scan
	 * @ssid_len: length of the SSID
	 *
	 * Return: 0 on success, -1 on failure
	 *
	 * Once the scan results are ready, the driver should report scan
	 * results event for wpa_supplicant which will eventually request the
	 * results with wpa_driver_get_scan_results().
	 */
	int (*wlan_scan)(const char *ssid, int ssid_len);

	/**
	 * set profile for wep encryption
	 * @conf: profile of Access Point
	 * @timeout: time out value
	 *
	 * Return: 0 on success, -1 on failure
	 */
	int (*wlan_set_profile_wep)(SecureSOHO_Wireless *conf, int timeout);

	/**
	 * set profile for wpapsk encryption
	 * @conf: profile of Access Point
	 * @timeout: time out value
	 *
	 * Return: 0 on success, -1 on failure
	 */
	int (*wlan_set_profile_wpa)(SecureSOHO_Wireless *conf, int timeout);

	/**
	 * set profile for wpapsk encryption
	 * @conf: profile of Access Point
	 * @timeout: time out value
	 *
	 * Return: 0 on success, -1 on failure
	 */
	int (*wlan_get_profile)(SecureSOHO_Wireless *conf);


	/**
	 * get link status
	 * @status: store wireless status
	 *
	 * Return: 0 on success, -1 on failure
	 */
	int (*wlan_get_status)(int *ststus);
	
	/**
	 * get link signal strength
	 * @signal: link signal strength with access point
	 *
	 * Return: 0 on success, -1 on failure
	 */
	int (*wlan_get_signal_strength)(int *signal);

	/**
	 * get_scan_results - fetch the latest scan results
	 * @priv: private driver interface data
	 * @results: pointer to buffer for scan results
	 * @max_size: maximum number of entries (buffer size)
	 *
	 * Return: number of scan result entries used on success, -1 on failure
	 *
	 * If scan results include more than @max_size BSSes, @max_size will be
	 * returned and the remaining entries will not be included in the
	 * buffer.
	 */
	struct SITE_OBJ *(*wlan_get_scan_results)(int *cnt, WIRELESS_MODE mode);

	/**
	 * get_wpa_linkstate - get wpapsk link state
	 * 
	 * Return:
	 * @state: 1 on link success, 0 on failure
	 */
	void (*wlan_get_wpa_linkstate)(int *state);

	/**
	 * link up - set link status and assocated time.
	 */
	void (*wlan_linkup)(void);

	/**
	 * link down - set link status.
	 */
	void (*wlan_linkdown)(void);

	/**
	 * site survey completed
	 */
	void (*wlan_scan_completed)(void);

	/**
	 * Set site survey completed call back function
	 */
	void (*wlan_set_scan_completed_cb)(void (*cb)(void *UserData), void *UserData);

	/**
	 * Get the wps pin code
	 */
	int (*wlan_get_pincode)(char *pin, int size);

	/**
	 * Get the wps status
	 */
	int (*wlan_get_wps_status)(int *status);
};

typedef enum _site_sort_type_e{
	SITE_SORT_TYPE_SSID=0,
	SITE_SORT_TYPE_SIGNAL,
	SITE_SORT_TYPE_SPEED,
	SITE_SORT_TYPE_BAND,
	SITE_SORT_TYPE_SECURITY
}SITE_SORT_TYPE_E;

typedef enum {
	INTERSIL = 0,
	RALINK = 1,
	ATHEROS = 2,		/* add for atheros yangzheng 2006/04/30 */
} WIRELESS_CARD_TYPE;

struct SITE_OBJ {
	char essid[33]; //{0~z, 1~32 ascii characters}
	char bssid[18];
	int channel;
	int signal_dbm;
	int signal_percentage;
	int support_wps; //not zero for YES, zero for NO
	int wpa;

	WIRELESS_MODE wireless_mode;
	WIRELESS_TYPE wireless_type;
	WIRELESS_AUTH_MODE auth_mode;
	WIRELESS_ENCRYPT_TYPE encrypt_type;

	char wpa_ie[81];
	char rsn_ie[81];
	struct SITE_OBJ *next;
};

struct SWAP_OBJ {
	struct SITE_OBJ *site_head, *site1, *site2;
};

/* 
 * We always add the dummy wireless driver to the tail of the wireless driver list for safe
 * */
int wlan_driver_init(void);
struct wireless_driver_ops *wlan_driver_list_get(void);
int wlan_driver_del(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node);
/* 
 * Clear the driver list except dummy driver
 * */
int wlan_driver_clear(void);
int wlan_driver_add(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node);
int wlan_driver_match(unsigned short vid, unsigned short pid, void (*callback)(void *));
int wlan_driver_contains_of(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node);
int wlan_driver_dump(void);
struct wireless_driver_ops * wlan_driver_check(void);

extern void reset_wireless(void);
extern struct SITE_OBJ *securesoho_site_survey(int *cnt, WIRELESS_MODE mode);
extern void securesoho_free_site_obj(struct SITE_OBJ *object);

/* Check ssid. return 1 1 if is an invalidate value otherwise return 0*/
extern int invalidate_ssid(const char *ssid);
/* Check WEP key. retrun 1 if is an invalidate value otherwise return 0*/
extern int invalidate_wep_key(const char *key, int crypt_bit);
/* Detect wireless card plug */
void securesoho_detect_wireless_card(void);
struct wireless_driver_ops *securesoho_get_wireless_card(void);
int securesoho_is_wireless_card_dummy(void);
/* Get wireless signal */
int securesoho_get_wireless_signal(void);

int set_private(int skfd, char *ifname, char *cmdname, void **ret, int count, void *args[]);
struct SITE_OBJ *sort_site(struct SITE_OBJ *site, SITE_SORT_TYPE_E sort_type);
struct SITE_OBJ **get_arSiteObj(void);
int dbm2percentage(int dbm);
#endif				//__SECURESOHO_WIRELESS_H__
