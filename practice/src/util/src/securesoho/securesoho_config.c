/* Copyright (C) 2005, Alphanetworks, inc.
 * Author:  redsonic
 * $Header: /data/cvsroot/DMA/util/src/securesoho/securesoho_config.c,v 1.3.2.66 2007-10-31 08:46:08 joshua Exp $
 * vim:cindent:ts=8:sw=8
 * Wills Yin: Group all config parse/read/write functions into this file
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <string.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/socket.h>
#include <linux/if.h>

#include "securesoho.h"
#include "config_access.h"
#include "securesoho_wireless.h"
#include "wireless_profile_access.h"

//We'll check the screen resolution config file which locate in the root dir of USB key. There's a appropriate config entry in the config file when the MCX start up, the device will update the screen resolution.
#define MCX_RES_CONFIG_PATH		CONF_MNT_ROOT"/"CONF_EXTERNAL_MNT_PREFIX"/usb1"
#define MCX_RES_CONFIG_FILE		MCX_RES_CONFIG_PATH"/UIResolution.txt"

/*********************************************************** 
 * All interface to set/get related item from config files 
 ***********************************************************/
int securesoho_wan_get(SecureSOHO_WAN *wan)
{
	tuple_t	tp[8] = {
		{"GATEWAY",	wan->gateway,	TUPLE_STRING	},
		{"NETMASK1",	wan->mask,	TUPLE_STRING	},
		{"IP1",		wan->ip,	TUPLE_STRING	},
		{"WAN_TYPE",	&wan->type,	TUPLE_INT	},
		{"NAMESERVER1", wan->dns1,	TUPLE_STRING	},
		{"NAMESERVER2", wan->dns2,	TUPLE_STRING	},
		{"NAMESERVER3", wan->dns3,	TUPLE_STRING	},
		{NULL, NULL, 0 },
	};
	return securesoho_values_get( tp );
}

int securesoho_wireless_is_wifi_direct(void)
{
	SecureSOHO_Wireless w;
	memset(&w, 0, sizeof(SecureSOHO_Wireless));
	securesoho_wireless_get(&w);

	if(WIRELESS_WIFI_DIRECT_WPS_PBC == w.wps_mode || WIRELESS_WIFI_DIRECT_WPS_PIN == w.wps_mode) {
		return 1;
	} else {
		return 0;
	}
}

int securesoho_lan_get(SecureSOHO_LAN *lan)
{
	int	ret = 0;
	char    wlan_name[64]={0};
	char    wlan_p2p_name[64]={0};
	char	buf_lan_type[64];
	char	buf_set_dns_srv[64];
	char    ifname[64];
	/*for LAN general config*/
	tuple_t	tp_general[7] = {
		{"LANDNS",	lan->dns,	TUPLE_STRING  },
		{"LANDNS1",	lan->dns1,	TUPLE_STRING  },
		{"DOMAIN",	lan->domain,	TUPLE_STRING  },
		{"DMA_NAME",	lan->host,	TUPLE_STRING  },
		{"virdev_int",	ifname,         TUPLE_STRING  },
		{"SET_DNS_SRV",	buf_set_dns_srv,TUPLE_STRING  },
		{NULL, NULL, 0},
	};
	
	/*for wired config*/
	tuple_t	tp_wired[7] = {
		{"IP2",		lan->ip,	TUPLE_STRING  },
		{"NETMASK2",	lan->netmask,	TUPLE_STRING  },
		{"GATEWAY2",	lan->gateway,	TUPLE_STRING  },
		{"LAN_TYPE",	buf_lan_type,	TUPLE_STRING  },
		{"PPPOE_USERNAME", lan->pppoe_username, TUPLE_STRING },
		{"PPPOE_PASSWORD", lan->pppoe_password, TUPLE_STRING },
		{NULL, NULL, 0},
	};

	/*for wireless config*/
	tuple_t	tp_wireless[5] = {
		{"IP_WIRELESS",		lan->ip,	TUPLE_STRING  },
		{"NETMASK_WIRELESS",	lan->netmask,	TUPLE_STRING  },
		{"GATEWAY_WIRELESS",	lan->gateway,	TUPLE_STRING  },
		{"LAN_TYPE_WIRELESS",	buf_lan_type,	TUPLE_STRING  },
		{NULL, NULL, 0},
	};
	
	lan->lan_type = LAN_DHCP;
	lan->con_type = CON_WIRED;
	lan->ip[0] = lan->netmask[0] = lan->gateway[0] = lan->dns[0] = '\0';
	buf_lan_type[0] = lan->lan_ifname[0] = buf_set_dns_srv [0] = '\0';

	securesoho_string_get(WIRELESS_NIC_NAME, wlan_name);
	securesoho_string_get(WIRELESS_NIC_P2P_NAME, wlan_p2p_name);
	ret = securesoho_values_get( tp_general );
	lan->con_type = ((!strcmp(ifname, wlan_name) || !strcmp(ifname, wlan_p2p_name))&& ifname[0] != '\0') ? CON_WIRELESS : CON_WIRED;
	
	if(lan->con_type == CON_WIRELESS)
		ret = securesoho_values_get( tp_wireless );	
	else 
		ret = securesoho_values_get( tp_wired );

	if (buf_lan_type[0] == 's')
	{
		lan->lan_type = LAN_FIXEDIP;
	}
	else if (buf_lan_type[0] == 'p')
	{
		lan->lan_type = LAN_PPPOE;
	}
	else
	{
		lan->lan_type = LAN_DHCP;
	}

	switch(lan->con_type){
	case CON_WIRELESS:
		if(securesoho_wireless_is_wifi_direct()) {
			sprintf(lan->lan_ifname, "%s", wlan_p2p_name);
		} else {
			sprintf(lan->lan_ifname, "%s", wlan_name);
		}
		break;
	default:
		sprintf(lan->lan_ifname, "%s", LAN_PORT);
		break;
	}
	
	lan->set_dns_srv = (buf_set_dns_srv[0] == 'a') ? AUTO : MANUAL;
	return ret;
}

static int securesoho_lan_conf_set(SecureSOHO_LAN *lan, int restart)
{    
	int	ret;
	tuple_t	tp_general[5] = {
		{"LANDNS",	lan->dns,	TUPLE_STRING	},
		{"LANDNS1",	lan->dns1,	TUPLE_STRING	},
		{"virdev_int",	NULL,		TUPLE_STRING	},
		{"SET_DNS_SRV",	NULL,		TUPLE_STRING	},
		{NULL, NULL, 0},
	};

	tuple_t	tp_wired[7] = {
		{"IP2",		NULL,		TUPLE_STRING	},
		{"NETMASK2",	lan->netmask,	TUPLE_STRING	},
		{"GATEWAY2",	lan->gateway,	TUPLE_STRING	},
		{"LAN_TYPE",	NULL,		TUPLE_STRING	},
		{"PPPOE_USERNAME", lan->pppoe_username, TUPLE_STRING },
		{"PPPOE_PASSWORD", lan->pppoe_password, TUPLE_STRING },
		{NULL, NULL, 0},
	};

	tuple_t	tp_wireless[5] = {
		{"IP_WIRELESS",		NULL,		TUPLE_STRING	},
		{"NETMASK_WIRELESS",	lan->netmask,	TUPLE_STRING	},
		{"GATEWAY_WIRELESS",	lan->gateway,	TUPLE_STRING	},
		{"LAN_TYPE_WIRELESS",	NULL,		TUPLE_STRING	},
		{NULL, NULL, 0},
	};
	
	char wlan_name[64]={0};
	char wlan_p2p_name[64]={0};

	securesoho_string_get(WIRELESS_NIC_NAME, wlan_name);
	securesoho_string_get(WIRELESS_NIC_P2P_NAME, wlan_p2p_name);

	if(securesoho_wireless_is_wifi_direct()) {
		tp_general[2].container = (lan->con_type==CON_WIRELESS) ? wlan_p2p_name: LAN_PORT;
	} else {
		tp_general[2].container = (lan->con_type==CON_WIRELESS) ? wlan_name: LAN_PORT;
	}

	tp_general[3].container = (lan->set_dns_srv==AUTO) ? "a" : "m"; 
	ret =  securesoho_values_set( tp_general );
	
	if ( lan->con_type==CON_WIRELESS)
	{
		tp_wireless[0].container = (lan->lan_type=='d') ? "" : lan->ip;
		tp_wireless[3].container = (lan->lan_type==LAN_DHCP) ? "d" : "s";
		ret =  securesoho_values_set( tp_wireless );
	}
	else 
	{
		tp_wired[0].container = (lan->lan_type=='d') ? "" : lan->ip;
		if (lan->lan_type == LAN_FIXEDIP) 
		{
			tp_wired[3].container = "s";
		}
		else if (lan->lan_type == LAN_PPPOE) 
		{
			tp_wired[3].container = "p";
		}
		else 
		{	/* Set DHCP for others */
			tp_wired[3].container = "d";
		}
		ret =  securesoho_values_set( tp_wired );
	}
	return ret;
}

int securesoho_lan_set(SecureSOHO_LAN *lan, int restart)
{
	if (securesoho_lan_conf_set(lan, 0) == -ESEC_IO_ERROR)
		return -ESEC_IO_ERROR;
	DMA_ConfSave();
	return ESEC_NONE;
}


int securesoho_general_get(SecureSOHO_General *w)
{
	int	ret;
	char	*ptr = NULL;
	char	buf_FWUDCOND[64];
	char	buf_RUN_WIZARD[64];
	char	buf_ver[64];
	tuple_t	tp[8] = {
		{"DMA_NAME",		w->name,	TUPLE_STRING	},
		{"FWUDCOND",		buf_FWUDCOND,	TUPLE_STRING	},
		{"RUN_WIZARD",		buf_RUN_WIZARD,	TUPLE_STRING	},
		{"MEDIASERVER_VERSION", buf_ver,	TUPLE_STRING	},
		{NULL, NULL, 0},
	};

	buf_FWUDCOND[0] = buf_RUN_WIZARD[0] = buf_ver[0] = '\0';
	ret = securesoho_values_get( tp );
	w->fwupcond = buf_FWUDCOND[0] == '1' ? 'a' : 'm';
	w->run_wizard = buf_RUN_WIZARD[0];
	w->mediaserver_version = strtol( buf_ver, &ptr, 10);
	if (ptr && *ptr != '\0') 
		w->mediaserver_version = 0;
	return ret;
}

static int securesoho_general_conf_set(SecureSOHO_General *w, int restart)
{
	int	ret;
	char	buf_tmp[4];
	tuple_t	tp[8] = {
		{"DMA_NAME",	w->name,	TUPLE_STRING	},
		{"FWUDCOND",	NULL,		TUPLE_STRING	},
		{"RUN_WIZARD",	buf_tmp,	TUPLE_STRING	},
		{NULL, NULL, 0},
	};

	tp[1].container = (w->fwupcond == 'a') ? "1" : "0";
	buf_tmp[0] = w->run_wizard;
	buf_tmp[1] = '\0';
	ret = securesoho_values_set( tp );
	return ret;
}

int securesoho_general_set(SecureSOHO_General *w, int restart)
{
	if (securesoho_general_conf_set(w, 0)==-ESEC_IO_ERROR)
		return -ESEC_IO_ERROR;
	DMA_ConfSave();
	return ESEC_NONE;
}

int securesoho_wireless_get(SecureSOHO_Wireless *w)
{
	int	ret;
	tuple_t	tp[] = {
		{"WIRELESS_MODE",		&w->wireless_mode,	TUPLE_INT	},
		{"WIRELESS_TYPE",		&w->wireless_type,	TUPLE_INT	},
		{"WIRELESS_AUTH_MODE",		&w->auth_mode,		TUPLE_INT	},
		{"WIRELESS_ENCRYPT_TYPE",	&w->encrypt_type,	TUPLE_INT	},
		{"WIRELESS_SSID",		w->ssid,		TUPLE_STRING	},
		{"WIRELESS_BSSID",		w->bssid,		TUPLE_STRING	},
		{"WIRELESS_CHANNEL",		&w->channel,		TUPLE_INT	},
		{"WIRELESS_WPS_MODE",		&w->wps_mode,		TUPLE_INT	},
		{"WIRELESS_WPS_PIN",		&w->wps_pin,		TUPLE_STRING	},
		{"WIRELESS_WEP_ENCRYPT_LEVEL",	&w->wep_encrypt_level,	TUPLE_INT	},
		{"WIRELESS_WEP_KEY_INDEX",	&w->wep_key_index,	TUPLE_INT	},
		{"WIRELESS_WEP_ASCIIKEY0",	w->wep_asciikey[0],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY1",	w->wep_asciikey[1],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY2",	w->wep_asciikey[2],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY3",	w->wep_asciikey[3],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY0",	w->wep_hexkey[0],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY1",	w->wep_hexkey[1],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY2",	w->wep_hexkey[2],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY3",	w->wep_hexkey[3],	TUPLE_STRING	},
		{"WIRELESS_WPA_PSK",		w->wpa_psk,		TUPLE_STRING	},
		{NULL, NULL, 0},
	};
	if (!w){
		return -1;
	}

	memset(w, '\0', sizeof(SecureSOHO_Wireless));
	ret = securesoho_values_get(tp); 
	return ret;
}

static int securesoho_wireless_conf_set(SecureSOHO_Wireless *w, int restart)
{
	int	ret;
	tuple_t	tp[] = {
		{"WIRELESS_MODE",		&w->wireless_mode,	TUPLE_INT	},
		{"WIRELESS_TYPE",		&w->wireless_type,	TUPLE_INT	},
		{"WIRELESS_AUTH_MODE",		&w->auth_mode,		TUPLE_INT	},
		{"WIRELESS_ENCRYPT_TYPE",	&w->encrypt_type,	TUPLE_INT	},
		{"WIRELESS_SSID",		w->ssid,		TUPLE_STRING	},
		{"WIRELESS_BSSID",		w->bssid,		TUPLE_STRING	},
		{"WIRELESS_CHANNEL",		&w->channel,		TUPLE_INT	},
		{"WIRELESS_WPS_MODE",		&w->wps_mode,		TUPLE_INT	},
		{"WIRELESS_WPS_PIN",		&w->wps_pin,		TUPLE_STRING	},
		{"WIRELESS_WEP_ENCRYPT_LEVEL",	&w->wep_encrypt_level,	TUPLE_INT	},
		{"WIRELESS_WEP_KEY_INDEX",	&w->wep_key_index,	TUPLE_INT	},
		{"WIRELESS_WEP_ASCIIKEY0",	w->wep_asciikey[0],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY1",	w->wep_asciikey[1],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY2",	w->wep_asciikey[2],	TUPLE_STRING	},
		{"WIRELESS_WEP_ASCIIKEY3",	w->wep_asciikey[3],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY0",	w->wep_hexkey[0],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY1",	w->wep_hexkey[1],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY2",	w->wep_hexkey[2],	TUPLE_STRING	},
		{"WIRELESS_WEP_HEXKEY3",	w->wep_hexkey[3],	TUPLE_STRING	},
		{"WIRELESS_WPA_PSK",		w->wpa_psk,		TUPLE_STRING	},
		{NULL, NULL, 0},
	};
	ret = securesoho_values_set( tp ); 
	return ret; 
}

int securesoho_wireless_set(SecureSOHO_Wireless *w, int restart)
{

	if (securesoho_wireless_conf_set(w, 0)==-ESEC_IO_ERROR)
		return -ESEC_IO_ERROR;
	DMA_ConfSave();

	return ESEC_NONE;
}

int securesoho_slideshow_set(int slideshow)
{
	securesoho_int_set( "DMA_SLIDESHOW", slideshow );
	DMA_ConfSave();
	return ESEC_NONE;
}

int securesoho_slideshow_get(int *slideshow)
{
	if (slideshow)
		*slideshow = securesoho_int_get( "DMA_SLIDESHOW" );
	return 0;
}

/* Setup cache variable to speedup the access of minute & enable. */
int securesoho_screensaver_set(int minute,int enable)
{
	securesoho_int_set( "DMA_SCREENSAVER", minute );
	securesoho_int_set( "DMA_ENABLE_SCREENSAVER", enable );
	return ESEC_NONE;
}

void securesoho_smbshare_set(int enable_smbshare)
{
	securesoho_int_set("ENABLE_SAMBA_SHARE", enable_smbshare);
}

int securesoho_smbshare_get()
{
	return securesoho_int_get("ENABLE_SAMBA_SHARE");
}

void securesoho_download_speed_limit_set(int value)
{
	securesoho_int_set("DM_DOWNLOADLIMIT_SPEED", value);
}

int securesoho_download_speed_limit_get()
{
	return securesoho_int_get("DM_DOWNLOADLIMIT_SPEED");
}

void securesoho_upload_speed_limit_set(int value)
{
	securesoho_int_set("DM_UPLOADLIMIT_SPEED", value);
}

int securesoho_upload_speed_limit_get()
{
	return securesoho_int_get("DM_UPLOADLIMIT_SPEED");
}

void securesoho_download_count_limit_set(int value)
{
	securesoho_int_set("DM_DOWNLOADNUMBERLIMIT", value);
}

int securesoho_download_count_limit_get()
{
	return securesoho_int_get("DM_DOWNLOADNUMBERLIMIT");
}

void securesoho_last_channel_set(int number)
{
	securesoho_int_set("CM_LAST_CHANNEL", number);
}

int securesoho_last_channel_get()
{
	return securesoho_int_get("CM_LAST_CHANNEL");
}

int securesoho_screensaver_get(int *minute,int *enable)
{
	if (minute)
		*minute = securesoho_int_get( "DMA_SCREENSAVER" );
	if (enable)
		*enable = securesoho_int_get( "DMA_ENABLE_SCREENSAVER" );
	return 0;
}

int securesoho_lif_type_set(CONNECTION type)
{
	char wlan_name[64]={0};
	char wlan_p2p_name[64]={0};

	securesoho_string_get(WIRELESS_NIC_NAME, wlan_name);
	securesoho_string_get(WIRELESS_NIC_P2P_NAME, wlan_p2p_name);

	switch(type){
	case CON_WIRELESS:
		if(securesoho_wireless_is_wifi_direct()) {
			securesoho_string_set("virdev_int", wlan_p2p_name);
		} else {
			securesoho_string_set("virdev_int", wlan_name);
		}
		break;
	case CON_WIRED:
		securesoho_string_set("virdev_int", LAN_PORT);		
		break;
	default:
		securesoho_string_set("virdev_int", "");		
		break;
	}
	return 0;
}



int securesoho_lif_type_get(CONNECTION *type)
{
	char lif[32]={0};
	char wlan_name[64]={0};
	char wlan_p2p_name[64]={0};

	securesoho_string_get(WIRELESS_NIC_NAME, wlan_name);
	securesoho_string_get(WIRELESS_NIC_P2P_NAME, wlan_p2p_name);
	securesoho_string_get( "virdev_int", lif );

	if(!strcmp(lif, wlan_name) && lif[0] != '\0'){
		*type = CON_WIRELESS;
	}else if(!strcmp(lif, wlan_p2p_name) && lif[0] != '\0'){
		*type = CON_WIRELESS;
	}else if(!strcmp(lif, LAN_PORT) && lif[0] != '\0'){
		*type = CON_WIRED;
	}else{
		*type = CON_UNKNOWN;
	} 
	return 0;
}
#if 0
int securesoho_lif_set(char *lif)
{
	securesoho_string_set( "virdev_int", lif);
	return 0;
}
#endif

int securesoho_lif_get(char *lif)
{
	CONNECTION type;
	char wlan_name[64]={0};
	char wlan_p2p_name[64]={0};
	
	securesoho_string_get(WIRELESS_NIC_NAME, wlan_name);
	securesoho_string_get(WIRELESS_NIC_P2P_NAME, wlan_p2p_name);
	securesoho_lif_type_get(&type);

	switch(type){
	case CON_WIRELESS:
		if(securesoho_wireless_is_wifi_direct()) {
			sprintf(lif, "%s", wlan_p2p_name);
		} else {
			sprintf(lif, "%s", wlan_name);
		}
		break;
	case CON_WIRED:
		sprintf(lif, "%s", LAN_PORT);
		break;
	case CON_UNKNOWN:
		sprintf(lif, "");
		break;
	default:
		sprintf(lif, "");
		break;
	}
	return 0;
}

extern int securesoho_excp_name_set(char *excp_name)
{
	securesoho_string_set("EXCP_NAME", excp_name);
	DMA_ConfSave();
	return ESEC_NONE;
}

extern int securesoho_excp_name_get(char *excp_name)
{
	securesoho_string_get("EXCP_NAME", excp_name);
	return 0;
}

int securesoho_devicename_set(char *devicename)
{
	securesoho_string_set( "DMA_NAME", devicename );
	DMA_ConfSave();
	return ESEC_NONE;
}

int securesoho_devicename_get(char *devicename)
{
	securesoho_string_get( "DMA_NAME", devicename );
	return 0;
}

void securesoho_locale_set(const char * localename)
{
	securesoho_string_set( "LOCALE", localename ); 
	DMA_ConfSave();
}

void securesoho_locale_get(char *localename)
{
	localename[0] = '\0';
	securesoho_string_get( "LOCALE", localename );
	return ;
}

void securesoho_city_set(const char* cityname)
{
	securesoho_string_set("CITY", cityname); 
	DMA_ConfSave();
}

void securesoho_city_get(char* cityname)
{
	cityname[0] = '\0';
	securesoho_string_get("CITY", cityname);
	return ;
}

void securesoho_netbiosname_prefix_get(char *netbiosname_prefix)
{
	netbiosname_prefix[0] = '\0';
	securesoho_string_get( "NETBIOSNAME_PREFIX", netbiosname_prefix);
}

void securesoho_default_encoding_type_get(char *encoding_type)
{
	encoding_type[0] = '\0';
	securesoho_string_get( "DEFAULT_ENCODING_TYPE", encoding_type );
}

void securesoho_text_encoding_type_set(const char * encoding_type)
{
	securesoho_string_set( "TEXT_ENCODING_TYPE", encoding_type ); 
	DMA_ConfSave();
}

void securesoho_text_encoding_type_get(char *encoding_type)
{
	encoding_type[0] = '\0';
	securesoho_string_get( "TEXT_ENCODING_TYPE", encoding_type );
}

void securesoho_portal_locale_set(const char * localename)
{
	securesoho_string_set( "PORTAL_LOCALE", localename ); 
	DMA_ConfSave();
}

void securesoho_portal_locale_get(char *localename)
{
	localename[0] = '\0';
	securesoho_string_get( "PORTAL_LOCALE", localename );
	return ;
}

void securesoho_model_set(const char *modelname)
{
	securesoho_string_set( "MODEL_NAME", modelname ); 
}

void securesoho_model_get(char *modelname)
{
	securesoho_string_get( "MODEL_NAME", modelname );
}

int securesoho_remotetype_get(int* remotetype)
{
	*remotetype = securesoho_int_get( "DMA_RC_COMPATIBLE_CHANNEL" ); 
	return 0;
}

void securesoho_remotetype_set(int remotetype)
{
	securesoho_int_set( "DMA_RC_COMPATIBLE_CHANNEL", remotetype );
	DMA_ConfSave();
}

int securesoho_media_server_name_get(char *ServerName)
{
	securesoho_string_get( "DMA_MEDIA_SERVER_NAME", ServerName );
	return 0;
}

int securesoho_media_server_get(char *ServerHandle)
{
	securesoho_string_get( "DMA_MEDIA_SERVER", ServerHandle );
	return 0;
}
int securesoho_media_server_set(const char*ServerHandle, const char *ServerName)
{
	char	buf_server_handle[1024]={0};
	char	buf_server_name[1024]={0};

	securesoho_string_get( "DMA_MEDIA_SERVER", buf_server_handle );
	securesoho_string_get( "DMA_MEDIA_SERVER_NAME", buf_server_name );
	if (ServerHandle && strcmp( buf_server_handle, ServerHandle )){
		securesoho_string_set( "DMA_MEDIA_SERVER", ServerHandle );
	}
	if(ServerName && strcmp(buf_server_name, ServerName)){
		securesoho_string_set( "DMA_MEDIA_SERVER_NAME", ServerName );
	}
	return 0;
}

int securesoho_default_samba_name_get(char *ServerName)
{
	securesoho_string_get( "DMA_SAMBA_SERVER_NAME", ServerName );
	return 0;
}

int securesoho_default_samba_get(char *ServerHandle)
{
	securesoho_string_get( "DMA_SAMBA_SERVER", ServerHandle );
	return 0;
}
int securesoho_default_samba_set(const char*ServerHandle, const char *ServerName)
{
	char	buf_server_handle[1024]={0};
	char	buf_server_name[1024]={0};

	securesoho_string_get( "DMA_SAMBA_SERVER", buf_server_handle );
	securesoho_string_get( "DMA_SAMBA_SERVER_NAME", buf_server_name );
	if (ServerHandle && strcmp( buf_server_handle, ServerHandle )){
		securesoho_string_set( "DMA_SAMBA_SERVER", ServerHandle );
	}
	if(ServerName && strcmp(buf_server_name, ServerName)){
		securesoho_string_set( "DMA_SAMBA_SERVER_NAME", ServerName );
	}
	return 0;
}

int securesoho_filename_set(int filename)
{
	securesoho_int_set( "FILENAME", filename ); 
	return ESEC_NONE;
}

int securesoho_filename_get(int *filename)
{
	*filename = securesoho_int_get( "FILENAME" );
	return 0;
}

int securesoho_firmware_checker_status_get(int *status)
{
	*status = securesoho_int_get( "FWUDCOND" );
	return 0; 
}

int securesoho_firmware_checker_status_set(int status)
{
	securesoho_int_set( "FWUDCOND", status );
	return 0;
}

void securesoho_get_model_magic_number(char *magic_number)
{
	MODEL_NUMBER mn;
	device_region_data data;
	const char *magic="";
	
	securesoho_get_device_region_data(&data);

	mn = data.region;
	switch(mn){
	case MN_EU:
		magic = "eu";
		break;
	case MN_WW:
		magic = "ww";
		break;
	case MN_WWN:
		magic = "wwn";
		break;
	case MN_WWP:
		magic = "wwp";
		break;
	case MN_CA:
		magic = "ca";
		break;
	case MN_TW:
		magic = "tw";
		break;
	case MN_AUS:
		magic = "au";
		break;
	case MN_USA:
	default:
		magic = "";
		break;
	}
	sprintf(magic_number, "%s", magic);
	return;
}

static void _static_get_fw_version_file(char *FWUDVERINFOFILE)
{
	char model_magic_number[32];

	securesoho_get_model_magic_number(model_magic_number);
	if(FWUDVERINFOFILE){
		sprintf(FWUDVERINFOFILE, "%s%s-VerInfo.txt", CONF_PRODUCT, model_magic_number);	
	}
	fprintf(stdout, "F:%s:%d, F/W Version file:%s\n", __FUNCTION__, __LINE__, FWUDVERINFOFILE);
	return;
}

int securesoho_firmware_upgrade_info_get (char *FWUDVERINFOFILE, 
					  char *FWUDSERVER) 
{

	securesoho_string_get( "FWUDSERVER", FWUDSERVER );
	_static_get_fw_version_file( FWUDVERINFOFILE );
	return ESEC_NONE;
}

/* 
 * 'securesoho_get_curversion' - Get the current firmware version 
 * output:
 *    pVer  -- char array for store version
 * return:
 *    0 -- OK
 *    1 -- FAIL
 */
int securesoho_get_curversion(char *pVer)
{
	FILE *fp;
	char oldver[64];
	char magic_model_number[32];

	fp = fopen("/conf_src/version", "r");
	if (fp == NULL)
		return 1;
	fscanf(fp, "%s", oldver);

	securesoho_get_model_magic_number(magic_model_number);
	sprintf(pVer, "%s%s", oldver, magic_model_number);
	fclose(fp);
	return 0;
}

/* 
 * 'securesoho_get_version_stage' - Get the current firmware version stage
 * output:
 *    pVer  -- char array for store version stage
 * return:
 *    0 -- OK
 *    1 -- FAIL
 */
int securesoho_get_version_stage(char *stage, int len)
{
	FILE *fp = NULL;
	char cStage[64];
	int ret;

	if((NULL == stage) || (0 == len))
			return 1;

	fp = fopen("/conf_src/version_stage", "r");
	if (fp == NULL)
		return 1;

	ret = fscanf(fp, "%s", cStage);
	if((-1 == ret ) || !strlen(cStage)) {
		fclose(fp);
		return 1;
	}

	snprintf(stage, len, "%s", cStage);
	fclose(fp);
	return 0;
}
/*                                                                                                                                                           
 * 'securesoho_get_curbuilddate' - Get the current firmware build date 
 * output:
 *    pVer  -- char array for store version
 * return:
 *    0 -- OK
 *    1 -- FAIL
 */
int securesoho_get_curbuilddate(char *pBuild)
{
        FILE *fp;
        char oldbuild[64];

        fp = fopen("/conf_src/date", "r");
        if (fp == NULL)
                return 1;
        fscanf(fp, "%s", oldbuild);

        strcpy(pBuild, oldbuild);
        fclose(fp);
        return 0;
}


/*
 * 'Config_VolumeSet' - Set the Volume into the DMA config file
 *  input:
 *	 iVolume -- Volume want to set
 *  return:
 *	 -ESEC_IO_ERROR -- IO error occured
 *	 ESEC_NONE	-- OK
 */
int config_set_volume(int iVolume)
{
	securesoho_int_set( "DMA_VOLUME", iVolume );
	return ESEC_NONE;
}

/*
 * 'Config_VolumeGet' - Get the Volume from the DMA config file
 *  output:
 *	 piVolume -- pointer to the getted volume
 *  return:
 *	 ESEC_VALUE_NOT_FOUND -- could not found item in config file
 *	 ESEC_NONE	      -- OK
 */
int config_get_volume(int *piVolume)
{
	*piVolume = securesoho_int_get( "DMA_VOLUME" );
	return 0;
}

int config_get_mute(int *pMute)
{
	*pMute = securesoho_int_get( "DMA_MUTE" );
	return 0;
}

int config_set_mute(int iMute)
{
	securesoho_int_set( "DMA_MUTE", iMute);
	return ESEC_NONE;
}

int restore_config()
{
	securesoho_replace_config( "/tmp/config.bak" );
	return ESEC_NONE;
}

int backup_config()
{
	int	lockfd = securesoho_config_lock();
	safe_cp( "/conf/config", "/tmp/config.bak" );
	securesoho_config_unlock( lockfd );
	return ESEC_NONE;
}

int securesoho_set_colorspace(int colorspace)
{
	securesoho_int_set( "COLORSPACE", colorspace );
	return ESEC_NONE;
}

int securesoho_get_colorspace(int *colorspace)
{
	*colorspace = securesoho_int_get("COLORSPACE");
	return ESEC_NONE;
}

/* 05-01-12 odie added */
/* 0:480i 1:480p 2:720p 3:1080i 4:auto*/
int securesoho_tvoutres_get(int *piTvOutResolution)
{
	tv_av_setting_t tv_setting;
	if (piTvOutResolution){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_VIDEO_OUT_RES;
		securesoho_get_tv_setting(&tv_setting);
		*piTvOutResolution = tv_setting.video_out_res;
	}	
	
	return 0;
}

void securesoho_tvoutres_set(int iTvOutResolution)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_VIDEO_OUT_RES;
	tv_setting.video_out_res = iTvOutResolution;
	securesoho_save_tv_setting(&tv_setting);
}


/*0:COMPOSITE 1: COMPONENT 2:HDMI 3:RGB_SCART_INTERLACE*/
int securesoho_tvoutmode_get(int* component_or_composite)
{
	tv_av_setting_t tv_setting;
	if (component_or_composite){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_VIDEO_OUT;
		securesoho_get_tv_setting(&tv_setting);
		*component_or_composite = tv_setting.video_out;
	}	
	return 0;
}

//Provide a way to update the screen resolution by USB
//The format of the resolution file is <width>x<height>
//
//The following is an example of the contents of the file for running at 720p:1280x720
static int securesoho_update_resolution(int *width, int *height)
{
	FILE *fp;
	struct stat buf;
	char *data;

	fprintf(stderr, "%s:%d update resolution by USB\n", __FUNCTION__, __LINE__);
	if (!width || !height){
		fprintf(stderr, "%s:%d Wrong width or height pointer\n", __FUNCTION__, __LINE__);
		return -1;
	}

	if ( stat(MCX_RES_CONFIG_FILE, &buf) ) {
		fprintf(stderr, "%s:%d the resolution config file:%s is not exist\n", __FUNCTION__, __LINE__, MCX_RES_CONFIG_FILE);
		return -1;
	}

	if ( (data = malloc(buf.st_size + 2)) == NULL ){
		fprintf(stderr, "%s:%d Failed to malloc %lld bytes, out of memory\n", __FUNCTION__, __LINE__, buf.st_size + 2);
		return -1;
	}

	if ( (fp = fopen(MCX_RES_CONFIG_FILE, "r")) != NULL )
	{
		char *res_start, *res_end, *end_ptr;
		long w, h; 
		fread(data, 1, buf.st_size, fp);
		data[buf.st_size] = '\0';                  /* termination required by flex */
		data[buf.st_size+1] = '\0';
		if ( (res_start = strstr(data, "<width>")) &&
				(res_end = strstr(data, "<height>"))){
			res_start += strlen("<width>");
			*res_end = '\0';
			//Now the string res_start will be 1280x720 format.
			w = strtol(res_start, &end_ptr, 10);
			if (end_ptr == res_start || (*end_ptr != 'x' && *end_ptr != 'X')) {
				fprintf(stderr, "%s:%d(%s:%s) No resolution were found, please check the file format\n", __FUNCTION__, __LINE__, res_start, end_ptr);
				free(data);
				return -1;
			}
			res_start = end_ptr + 1; // skip 'x' or 'X'
			h = strtol(res_start, &end_ptr, 10);
			if (end_ptr == res_start){
				fprintf(stderr, "%s:%d No resolution were found, please check the file format\n", __FUNCTION__, __LINE__);
				free(data);
				return -1;
			}
			*width = w;
			*height = h;
		}
		fclose(fp);
	}

	free(data);
	return 0;
}

int securesoho_get_osd_page_info(int *width, int *height)
{
	int tvoutres = 0;
	int tv_scan = 0;
	int tvout = 0;
	int width1,height1;

	securesoho_tvoutres_get(&tvoutres);
	securesoho_tvoutmode_get(&tvout);
	securesoho_tv_scan_get(&tv_scan);

	if (tvoutres >= VRM_720P50) {
		if (tv_scan == TS_16_9_SD){
			width1   = HDMI_HD_SCREEN_WIDHT_16x9;
			height1  = HDMI_HD_SCREEN_HEIGHT_16x9 ;
		} else {
			width1   = HDMI_HD_SCREEN_WIDTH_4x3;
			height1  = HDMI_HD_SCREEN_HEIGHT_4x3;
		}
	} else {
		if (tv_scan == TS_16_9_SD){
			width1  = SCREEN_WIDHT_SD_16x9;
			height1 = SCREEN_HEIGHT_SD_16x9;
		} else {
			width1  = SCREEN_WIDHT_SD_4x3;
			height1 = SCREEN_HEIGHT_SD_4x3;
		}
	}

	if (width) *width = width1;
	if (height) *height = height1;

	/* update the resolution by USB for mcx extender */
	if (securesoho_update_resolution(&width1, &height1) == 0) {
		*width = width1;
		*height = height1;
	}	

	return tvout;
}

void securesoho_tvoutmode_set(int hdtv)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_VIDEO_OUT;
	tv_setting.video_out = hdtv;
	securesoho_save_tv_setting(&tv_setting);
	
}	

/* get the audo output interface mode from config file */
int securesoho_audiochannel_get(int* _5_1_or_2_channel)
{
	tv_av_setting_t tv_setting;
	if (_5_1_or_2_channel){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_AUDIO_OUT;
		securesoho_get_tv_setting(&tv_setting);
		*_5_1_or_2_channel = tv_setting.audio_out;
	}
	return 0;
}

/* set the audio output interface mode to config file */
void securesoho_audiochannel_set(int _5_1_or_2_channel)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_AUDIO_OUT;

	tv_setting.audio_out = _5_1_or_2_channel;
	securesoho_save_tv_setting(&tv_setting);
}
#ifdef CONF_AUDIO_DTSHD_OUTPUT
/* get the audo dtshd output interface mode from config file */
int securesoho_audio_dtshd_get(int* _dtshd_out)
{
	tv_av_setting_t tv_setting;
	if (_dtshd_out){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_AUDIO_DTSHD_OUT;
		securesoho_get_tv_setting(&tv_setting);
		*_dtshd_out = tv_setting.audio_dtshd_out;
	}
	return 0;
}

/* set the audio dtshd output interface mode to config file */
void securesoho_audio_dtshd_set(int _dtshd_out)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_AUDIO_DTSHD_OUT;

	tv_setting.audio_dtshd_out = _dtshd_out;
	securesoho_save_tv_setting(&tv_setting);
}
#endif

/* get the tv output trick mode from config file */
void securesoho_tvout_trick_get(int* tvtrickmode)
{
	tv_av_setting_t tv_setting;
	if (tvtrickmode){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_TRICK_MODE;
		securesoho_get_tv_setting(&tv_setting);
		*tvtrickmode = tv_setting.trick_mode;
	}
	return ;
}

/* set the tv output trick mode to config file */
void securesoho_tvout_trick_set(int trickmode)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_TRICK_MODE;
	tv_setting.trick_mode = trickmode;
	securesoho_save_tv_setting(&tv_setting);
}



int securesoho_tv_scan_get(int *tv_scan)
{
	tv_av_setting_t tv_setting;
	if (tv_scan){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_TV_SCAN;
		securesoho_get_tv_setting(&tv_setting);
		*tv_scan = tv_setting.tv_scan;
	}
	return 0;	
}

/* get the TV mode */
int securesoho_ntsc_pal_get(int* ntsc_pal)
{
	tv_av_setting_t tv_setting;
	if (ntsc_pal){
		memset(&tv_setting,0,sizeof(tv_av_setting_t));
		tv_setting.set_mask = MSK_NTSC_PAL;
		securesoho_get_tv_setting(&tv_setting);
		*ntsc_pal = tv_setting.ntsc_pal;
	}
	return 0;
}

int securesoho_ntsc_pal_set(int ntsc_pal)
{
	tv_av_setting_t tv_setting;
	memset(&tv_setting,0,sizeof(tv_av_setting_t));
	tv_setting.set_mask = MSK_NTSC_PAL;
	tv_setting.ntsc_pal = ntsc_pal;
	securesoho_save_tv_setting(&tv_setting);
	return 0;
}

#ifdef CONF_CAS
int securesoho_content_aggregation_get(int* scan)
{
	*scan = securesoho_int_get( "DMA_CONTENT_AGGREGATION" ); 
	return 0;
}

int securesoho_content_aggregation_set(int scan)
{
	scan = securesoho_int_set( "DMA_CONTENT_AGGREGATION" , scan); 
	return 0;
}
#endif

#ifdef CONF_LCB_REMUX
int securesoho_video_gapless_playback_get(int* mode)
{
	*mode = securesoho_int_get("DMA_VIDEO_GAPLESS_PLAYBACK" ); 
	return 0;
}

int securesoho_video_gapless_playback_set(int mode)
{
	mode = securesoho_int_set("DMA_VIDEO_GAPLESS_PLAYBACK" , mode);
	return 0;
}
#endif

int securesoho_subtitle_font_size_get(int* font_size)
{
	if (font_size)
		*font_size = securesoho_int_get("SUBTITLE_FONT_SIZE"); 
	return 0;
}

int securesoho_subtitle_font_size_set(int font_size)
{
	font_size = securesoho_int_set("SUBTITLE_FONT_SIZE", font_size); 
	return 0;
}

int securesoho_smart_preview_get(int* smt_preview)
{
	if (smt_preview)
		*smt_preview = securesoho_int_get("SMART_PREVIEW"); 
	return 0;
}

int securesoho_smart_preview_set(int smt_preview)
{
	smt_preview = securesoho_int_set("SMART_PREVIEW", smt_preview); 
	return 0;
}

int securesoho_fast_channel_create_wizard_get(int* fccw)
{
	if (fccw)
		*fccw = securesoho_int_get("FAST_CHANNEL_CREATE_WIZARD"); 
	return 0;
}

int securesoho_fast_channel_create_wizard_set(int fccw)
{
	fccw = securesoho_int_set("FAST_CHANNEL_CREATE_WIZARD", fccw); 
	return 0;
}

int securesoho_browsemode_get_by_type(int* browsemode,int type)
{
#ifdef CONF_BROWSE_MODE_BY_MEDIA_TYPE
	switch(type){
	case 1:
		*browsemode = securesoho_int_get( "DMA_BROWSEMODE_MUSIC" ); 
		break;
	case 2:
		*browsemode = securesoho_int_get( "DMA_BROWSEMODE_VIDEO" ); 
		break;
	case 3:
		*browsemode = securesoho_int_get( "DMA_BROWSEMODE_PHOTO" ); 
		break;
	default:
		*browsemode = securesoho_int_get( "DMA_BROWSEMODE" ); 
		break;
	}
#else
	*browsemode = securesoho_int_get( "DMA_BROWSEMODE" ); 
#endif
	return 0;
}

int securesoho_browsemode_get(int* browsemode)
{
	*browsemode = securesoho_int_get( "DMA_BROWSEMODE" ); 
	return 0;
}

int securesoho_photo_slideshow_effect_get(char* slideshow_effect)
{
	securesoho_string_get( "PHOTO_FADE_INOUT", slideshow_effect);
	return 0;
}

void securesoho_photo_slideshow_effect_set(char* slideshow_effect)
{
	securesoho_string_set( "PHOTO_FADE_INOUT", slideshow_effect);
}

int securesoho_dma_first_boot_get(int* is_boot_first)
{
	if (is_boot_first){
		*is_boot_first = securesoho_int_get("DMA_FIRST_BOOT");
		return 0;
	}
	return -1;
}

int securesoho_dma_first_boot_set(int is_boot_first)
{
	securesoho_int_set("DMA_FIRST_BOOT", is_boot_first);
	return 0;
}

#ifdef CONF_MCX
int securesoho_mcx_cert_partition_get(char *cert, int len)
{
	if(!cert) {
		fprintf(stderr, "%s:%d. input is NULL.\n", __FUNCTION__, __LINE__);
		return 0;
	}
	snprintf(cert, len, "%s", CONF_CERT_MTD_PARTITION);
	return 1;
}

int securesoho_mcx_setting_partition_get(char *path, int len)
{
	if(!path) {
		fprintf(stderr, "%s:%d. input is NULL.\n", __FUNCTION__, __LINE__);
		return 0;
	}
	snprintf(path, len, "%s", CONF_CONFIG_MTD_PARTITION);
	return 1;
}

int securesoho_boot_first_get(int* is_boot_first)
{
	if (is_boot_first){
		*is_boot_first = securesoho_int_get("MCX_BOOT_FIRST");
		return 0;
	}
	return -1;
}

int securesoho_boot_fist_set(int is_boot_first)
{
	securesoho_int_set("MCX_BOOT_FIRST", is_boot_first);
	return 0;
}

int securesoho_mcx_auto_startup_get(char* autostartup)
{
	securesoho_string_get( "MCX_AUTO_STARTUP", autostartup );
	return 0;
}

void securesoho_mcx_auto_startup_set(char* autostartup)
{
	securesoho_string_set( "MCX_AUTO_STARTUP", autostartup );
}

int securesoho_mcx_green_btn_get(char* enable)
{
	securesoho_string_get( "MCX_GREEN_BTN", enable );
	return 0;
}
void securesoho_mcx_green_btn_set(char* enable)
{
	if (enable == NULL){
                fprintf(stderr, "%s:%d input parameter is NULL!\n", __FUNCTION__, __LINE__);
		return;
	}

	securesoho_string_set( "MCX_GREEN_BTN", enable );
	return ;
}

int securesoho_is_green_button_triggered(void){
	int fd;
	char cmdline[256]; //max length of kernel cmdline
	int ret;

	fd = open("/proc/cmdline", O_RDWR);
	if(fd < 0){
		fprintf(stderr, "%s:%d Failed to open proc cmdline file\n", __FUNCTION__, __LINE__);
		return 0;
	}		
	ret = read(fd, cmdline, sizeof(cmdline));
	close(fd);
	if (ret < 0){
		fprintf(stderr, "Read cmdline info, set no default\n");
		return 0;
	}
	cmdline[ret] = '\0';
	fprintf(stdout, "The cmdline is %s\n", cmdline);
	if (strstr(cmdline, "gbtn=1") != NULL)
		return 1;
	return 0;
}
#endif

int securesoho_remote_controller_get(char *rtype)
{
	securesoho_string_get( "DMA_REMOTE_TYPE", rtype );
	return 0;
}

#ifdef __EM86XX__
//Kenn Slagter 2005-01-06
//I want to be able to change the remote control version if I want to.
void securesoho_remote_controller_set(char *rtype)
{
	securesoho_string_set( "DMA_REMOTE_TYPE", rtype );
	DMA_ConfSave();
}

#endif

int securesoho_tvoutmode_get_inner(unsigned char* component_or_composite)
{
	char    buf[MAX_LINE_LENGTH]={0};
	if (component_or_composite){
		securesoho_string_get(CONFIG_VIDEO_OUT, buf );
		if (!strcmp( buf, "COMPOSITE")) {
			*component_or_composite = VM_COMPOSITE;
		}else if(!strcmp( buf, "COMPONENT")) {
			*component_or_composite = VM_COMPONENT;
		}else if(!strcmp( buf, "HDMI")) {
			*component_or_composite = VM_HDMI;
		}else if(!strcmp( buf, "RGB")){
			*component_or_composite = VM_RGB;
		}else if(!strcmp( buf, "RGB_SCART_INTERLACE")) {
			*component_or_composite = VM_RGB_SCART_INTERLACE;
		}else if(!strcmp( buf, "RGB_SCART_PROGRESSIVE")){
			*component_or_composite = VM_RGB_SCART_PROGRESSIVE;
		}else if(!strcmp( buf, "S_VIDEO_AV") || !strcmp(buf, "SVIDEO_SCART")){
			*component_or_composite = VM_S_VIDEO_AV;
		}else
			*component_or_composite = VM_COMPOSITE;
	}
	return 0;

}


int securesoho_tvoutmode_set_inner(unsigned char  component_or_composite)
{
	switch(component_or_composite){
		case VM_COMPOSITE:
			securesoho_string_set(CONFIG_VIDEO_OUT,"COMPOSITE");
			break;
		case VM_COMPONENT:
			securesoho_string_set(CONFIG_VIDEO_OUT,"COMPONENT");
			break;
		case VM_HDMI:
			securesoho_string_set(CONFIG_VIDEO_OUT,"HDMI");
			break;
		case VM_RGB:
			securesoho_string_set(CONFIG_VIDEO_OUT,"RGB");
			break;
		case VM_RGB_SCART_INTERLACE:
			securesoho_string_set(CONFIG_VIDEO_OUT,"RGB_SCART_INTERLACE");
			break;
		case VM_RGB_SCART_PROGRESSIVE:
			securesoho_string_set(CONFIG_VIDEO_OUT,"RGB_SCART_PROGRESSIVE");
			break;
		case VM_S_VIDEO_AV:
			securesoho_string_set(CONFIG_VIDEO_OUT,"S_VIDEO_AV");
			break;
		default :
			securesoho_string_set(CONFIG_VIDEO_OUT,"COMPOSITE");
			break;
	}
	return 0;
}


int securesoho_tvoutres_get_inner(unsigned char *iTvOutResolution)
{
	char    buf[MAX_LINE_LENGTH]={0};
	if (iTvOutResolution){
		securesoho_string_get( CONFIG_VIDEO_OUT_RES, buf );
		if(!strcmp( buf, "480p60")){
			*iTvOutResolution = VRM_480P60;
		} else if (!strcmp( buf, "576p50")){
			*iTvOutResolution = VRM_576P50;
		} else if(!strcmp( buf, "720p50")){
			*iTvOutResolution = VRM_720P50;
		} else if(!strcmp( buf, "720p60")){
			*iTvOutResolution = VRM_720P60;
		} else if(!strcmp( buf, "1080i50")){
			*iTvOutResolution = VRM_1080I50;
		} else if(!strcmp( buf, "1080i60")){
			*iTvOutResolution = VRM_1080I60;
		} else if(!strcmp( buf, "1080p50")){
			*iTvOutResolution = VRM_1080P50;
		} else if(!strcmp( buf, "1080p60")){
			*iTvOutResolution = VRM_1080P60;
		} else if(!strcmp( buf, "1080p24")){
			*iTvOutResolution = VRM_1080P24;
		} else if(!strcmp( buf, "auto")){
			*iTvOutResolution = VRM_AUTO;
		} else{ 
			*iTvOutResolution = VRM_AUTO;
		}	
	}
	return 0;
}


void securesoho_tvoutres_set_inner(unsigned char iTvOutResolution)
{
	switch(iTvOutResolution){
	case VRM_480P60:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"480p60");
		break;
	case VRM_576P50:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"576p50");
		break;
	case VRM_720P50:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"720p50");
		break;
	case VRM_720P60:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"720p60");
		break;
	case VRM_1080I50:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"1080i50");
		break;
	case VRM_1080I60:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"1080i60");
		break;
	case VRM_1080P50:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"1080p50");
		break;
	case VRM_1080P60:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"1080p60");
		break;
	case VRM_1080P24:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"1080p24");
		break;

	case VRM_AUTO:
		securesoho_string_set(CONFIG_VIDEO_OUT_RES,"auto");
		break;
	default:
		break;
	}
	return ;
}


unsigned char securesoho_ntsc_pal_get_inner(unsigned char * ntsc_pal)
{
	char	buf[MAX_LINE_LENGTH]={0};

	securesoho_string_get( CONFIG_NTSC_OR_PAL, buf );
	if (!strcmp( buf, "NTSC")) {
		*ntsc_pal = NPM_NTSC;
	} else {
		*ntsc_pal = NPM_PAL;
	}
	return 0;
}

void securesoho_ntsc_pal_set_inner(unsigned char ntsc_pal)
{
	if (ntsc_pal==NPM_NTSC)
		securesoho_string_set( CONFIG_NTSC_OR_PAL, "NTSC" );
	else
		securesoho_string_set( CONFIG_NTSC_OR_PAL, "PAL" );
}



unsigned char securesoho_get_tv_setting_inner(tv_av_setting_t *tv_setting)
{
	if (tv_setting==NULL)
		return 1;
	if (tv_setting->set_mask & MSK_NTSC_PAL){
		securesoho_ntsc_pal_get_inner(&(tv_setting->ntsc_pal));
	}	

	if (tv_setting->set_mask & MSK_TV_SCAN)
		tv_setting->tv_scan = securesoho_int_get(CONFIG_TV_SCAN);

	if (tv_setting->set_mask & MSK_VIDEO_OUT)
		securesoho_tvoutmode_get_inner(&(tv_setting->video_out));
	
	if (tv_setting->set_mask & MSK_VIDEO_OUT_RES){
		securesoho_tvoutres_get_inner(&(tv_setting->video_out_res));
	}
	
	if (tv_setting->set_mask & MSK_AUDIO_OUT)
		tv_setting->audio_out = securesoho_int_get(CONFIG_AUDIO_OUT);
#ifdef CONF_AUDIO_DTSHD_OUTPUT
	if (tv_setting->set_mask & MSK_AUDIO_DTSHD_OUT) {
		tv_setting->audio_dtshd_out = securesoho_int_get(CONFIG_AUDIO_DTSHD_OUT);
	}
#endif

	if (tv_setting->set_mask & MSK_VIDEO_PIP)
		tv_setting->video_pip = securesoho_int_get(CONFIG_VIDEO_PIP);
	if (tv_setting->set_mask & MSK_SCAN_MODE)
		tv_setting->scan_mode =securesoho_int_get(CONFIG_SCAN_MODE);
	if (tv_setting->set_mask & MSK_TRICK_MODE)
		tv_setting->trick_mode = securesoho_int_get(CONFIG_TV_TRICK);

	return 0;
	
}




void securesoho_get_tv_setting(tv_av_setting_t *tv_mode)
{
	if(tv_mode){
		if (tv_mode->set_mask==0x00){//to get all			
			tv_mode->set_mask = 0xff;
			securesoho_get_tv_setting_inner(tv_mode);
		}else
			securesoho_get_tv_setting_inner(tv_mode);
	}

}

void securesoho_save_tv_setting(tv_av_setting_t *tv_mode)
{
	if (tv_mode == NULL)
		return ;
	if(tv_mode->set_mask&MSK_NTSC_PAL){
		securesoho_ntsc_pal_set_inner(tv_mode->ntsc_pal);
	}

	 if(tv_mode->set_mask&MSK_TV_SCAN){
		 securesoho_int_set(CONFIG_TV_SCAN,tv_mode->tv_scan);		 
	 }

	 if(tv_mode->set_mask&MSK_VIDEO_OUT){
		 securesoho_tvoutmode_set_inner(tv_mode->video_out);
	 }

	 if(tv_mode->set_mask&MSK_VIDEO_OUT_RES){
		 securesoho_tvoutres_set_inner(tv_mode->video_out_res);
	 }

	 if(tv_mode->set_mask&MSK_AUDIO_OUT){
		 securesoho_int_set(CONFIG_AUDIO_OUT,tv_mode->audio_out);
	 }

#ifdef CONF_AUDIO_DTSHD_OUTPUT
	 if(tv_mode->set_mask&MSK_AUDIO_DTSHD_OUT){
		 securesoho_int_set(CONFIG_AUDIO_DTSHD_OUT,tv_mode->audio_dtshd_out);
	 }
#endif
	 if(tv_mode->set_mask&MSK_VIDEO_PIP){
		 securesoho_int_set(CONFIG_VIDEO_PIP,tv_mode->video_pip);
	 }
	 if(tv_mode->set_mask&MSK_SCAN_MODE){
		 securesoho_int_set(CONFIG_SCAN_MODE,tv_mode->scan_mode);
	 }

	 if(tv_mode->set_mask&MSK_TRICK_MODE){
		 securesoho_int_set(CONFIG_TV_TRICK,tv_mode->trick_mode);
	 }
	 return ;
}
void securesoho_browsemode_set_by_type(int browsemode,int type)
{
#ifdef CONF_BROWSE_MODE_BY_MEDIA_TYPE
	switch(type){
	case 1:
		securesoho_int_set( "DMA_BROWSEMODE_MUSIC", browsemode );
		break;
	case 2:
		securesoho_int_set( "DMA_BROWSEMODE_VIDEO", browsemode );
		break;
	case 3:
		securesoho_int_set( "DMA_BROWSEMODE_PHOTO", browsemode );
		break;
	default:
		securesoho_int_set( "DMA_BROWSEMODE", browsemode );
		break;
	}
#else
	securesoho_int_set( "DMA_BROWSEMODE", browsemode );
#endif
	DMA_ConfSave();
}

void securesoho_browsemode_set(int browsemode)
{
	securesoho_int_set( "DMA_BROWSEMODE", browsemode );
	DMA_ConfSave();
}

int securesoho_preset_url_get(int preset_key,char **url)
{
	char	buf_preset_key[MAX_LINE_LENGTH];
	char	buf_url[MAX_LINE_LENGTH]={0};

	sprintf(buf_preset_key, "PRESET_URL_%d", preset_key );
	securesoho_string_get( buf_preset_key, buf_url );
	*url = strdup( buf_url );
	return 0;
}

int securesoho_preset_url_set(int preset_key,const char *url)
{
	char	buf_preset_key[1024];

	sprintf(buf_preset_key, "PRESET_URL_%d", preset_key );
	securesoho_string_set( buf_preset_key, url );
	return ESEC_NONE;
}

int securesoho_preset_map_get(int preset_key, int *index)
{
	char	buf_preset[1024]={0};
	char	tmp_buf[1024]={0};

	sprintf(buf_preset, "PRESET_MAP_%d", preset_key);
	tmp_buf[0] = '\0';
	securesoho_string_get( buf_preset, tmp_buf );
	if (tmp_buf[0] == '\0')
		*index = -1;
	else
		*index = atoi(tmp_buf);
	return 0;
}

int securesoho_preset_map_set(int preset_key, int index)
{
	char	buf_preset[1024];
	int	idx;

	sprintf(buf_preset, "PRESET_MAP_%d", preset_key);
	idx = securesoho_int_get( buf_preset );
	if (idx != index) 
		securesoho_int_set( buf_preset, index );
	return ESEC_NONE;
}

void securesoho_mass_product_test_procedure_get(int *value)
{
	*value = securesoho_int_get( "MASS_PRODUCT" );
}

int securesoho_model_name_get(char *model_name)
{
	securesoho_string_get( "CONF_DMA_MODEL_NAME", model_name );
	return 0;
}

int securesoho_scart_get(int *scart)
{
#define IRDASCARTGET 0x3c06
	int fd;
	fd = open("/dev/irda", O_RDONLY);

	if (ioctl(fd, IRDASCARTGET, scart) < 0) {
		fprintf(stderr,"Can not tell if it support cart.");
		*scart=0;
	}
	close(fd);	
	return 0;   
}

#  define IR_IOC_MAGIC    'I'
#  define USB_LED_ON      _IO(IR_IOC_MAGIC, 7)
#  define USB_LED_OFF     _IO(IR_IOC_MAGIC, 8)
#  define WCN_SUCCESS_LED _IO(IR_IOC_MAGIC, 9)
#  define WCN_FAIL_LED    _IO(IR_IOC_MAGIC, 10)
/*
 * old setting
 #  define WCN_SUCCESS_LED 0x3c0b
 #  define WCN_FAIL_LED    0x3c0a
*/
/* ok: 0: wcn failed
 *     1: wcn successful
 *     3: usb inserted
 *     4: usb removed
 */
void securesoho_wcn_status_led(int ok)
{
	int fd, request= 0, arg;
	fd = open("/dev/irda", O_RDWR);
	if (fd < 0)
		return;
	if (ok==0)
		request = WCN_FAIL_LED;
	else if (ok==1)
		request = WCN_SUCCESS_LED;
	else if (ok==3)
		request = USB_LED_ON;
	else if (ok==4)
		request = USB_LED_OFF;
	ioctl(fd, request, &arg);
	close(fd);
}

#define REAL_VIDEO_OUT_TYPE		"/tmp/real_video_out_type"
#define REAL_VIDEO_OUT_TYPE_LOCK	"/tmp/real_video_out_type_lock"
static int access_real_tvouttype_lock(void)
{
	int     lockfd = -1;

	lockfd = open(REAL_VIDEO_OUT_TYPE_LOCK, O_CREAT | O_RDWR, 0644);
	if (lockfd >= 0 && flock( lockfd, LOCK_EX ) < 0) {
		printf("\033[1;42mflock REAL_VIDEO_OUT_TYPE_LOCK failed..\033[0m\n");
		close(lockfd);
		lockfd = -1;
	}
	return lockfd;
}

static void access_real_tvouttype_unlock(int fd)
{
	if (fd >= 0) {
		flock(fd, LOCK_UN );
		close(fd);
	}
}

int securesoho_real_tvouttype_get(int *video_out, int *video_out_res) {
	int ret=0;
	int lockfd = -1;
	FILE *fp = NULL;

	lockfd = access_real_tvouttype_lock();

	fp = fopen(REAL_VIDEO_OUT_TYPE, "r");
	if (!fp){
		ret = -1;
		goto out;
	}
	fscanf(fp, "video_out=%d,video_out_res=%d", video_out, video_out_res);	
	fclose(fp);
out:
	access_real_tvouttype_unlock(lockfd);
	return ret;
}

int securesoho_real_tvouttype_set(int video_out, int video_out_res) {
	int ret=0;
	int lockfd = -1;
	FILE *fp = NULL;

	lockfd = access_real_tvouttype_lock();

	fp = fopen(REAL_VIDEO_OUT_TYPE, "w");
	if (!fp){
		ret = -1;
		goto out;
	}
	fprintf(fp, "video_out=%d,video_out_res=%d", video_out, video_out_res);
	fclose(fp);
out:
	access_real_tvouttype_unlock(lockfd);
	return ret;
}

int securesoho_ip_type_set(int type)
{
	securesoho_int_set( "DMA_IP_TYPE", type );
	DMA_ConfSave();
	return ESEC_NONE;
}
int securesoho_ip_type_get(int *ptype)
{	
	if (ptype)
		*ptype = securesoho_int_get( "DMA_IP_TYPE" );
	return 0;
}
