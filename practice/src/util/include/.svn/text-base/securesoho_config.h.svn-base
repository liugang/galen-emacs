/* 
 * Copyright (C) 2006, Alphanetworks, inc.
 * Author: wills_yin@alphanetworks.com 
 * vim:cindent:ts=8:sw=8
 */
#ifndef __SECURESOHO_CONFIG_H__
#define __SECURESOHO_CONFIG_H__

#include "securesoho_wireless.h"

#define WIRELESS_NIC_NAME    "wlan_name"

#define WIRELESS_NIC_P2P_NAME    "wlan_p2p_name"
/*
 * The maximum and minimum value of the slideshow and screen saver period
 */
#define MAX_SCREEN_SAVER_PERIOD 60
#define MIN_SCREEN_SAVER_PERIOD 1
#define MAX_SLIDE_SHOW_PERIOD 30
#define MIN_SLIDE_SHOW_PERIOD 1

typedef enum{
	BROWSE_LIST = 0,
	BROWSE_THUMB,
	BROWSE_ALBUM_BOOK,
	BROWSE_COVER_FLOW,
	BROWSE_COVER_EXTENDED,
	BROWSE_LIST_EXTENDED,
	BROWSE_ITEM_MAX
}BROWSEMODE;

typedef enum {
	CON_WIRED = 0,
	CON_WIRELESS = 1,
	CON_UNKNOWN = 2

}CONNECTION;

typedef enum {
	WIRED = 0,
	WIRELESS = 1,
	UNCONNECTED = 2

}CONNECTED_TYPE;

typedef enum{
	AUTO 	= 0,
	MANUAL	= 1
}SET_DNS_SRV;

#ifdef CONF_AUDIO_DTSHD_OUTPUT
typedef enum{
	DTSHD_7_1 = 0,
    DTSHD_5_1 = 1
}SET_DTSHD_DOWNMIX;
#endif

typedef enum{
	LAN_FIXEDIP = 0,
	LAN_DHCP = 1,
	LAN_PPPOE = 2
}LANTYPE;

typedef enum{
	TYPE_UNKNOWN = 0,
	TYPE_AUTO_IP = 1,
	TYPE_DHCP = 2,
	TYPE_STATIC = 3,
	TYPE_PPPOE =4
}IP_TYPE;

typedef struct {
        char name[64];
        char fwupcond;
	char run_wizard;
        long mediaserver_version;
} SecureSOHO_General;

typedef struct {
        char ip[16];
        char netmask[16];
	char gateway[16];
	char dns[16];
	char dns1[16];
	char domain[64];
	char host[65];
	char lan_ifname[10]; //Steven Kuo: Wireless interface name of Marvell chip is wdev0sta0
	char pppoe_username[64]; /* PPPoE username/password provided by ISP */
	char pppoe_password[64];
	LANTYPE lan_type; /* Adds a type 'p' for PPPoE */
	CONNECTION con_type;
	SET_DNS_SRV set_dns_srv;
} SecureSOHO_LAN;

typedef struct{
	char xmpp_username[128];
	char xmpp_password[128];
	char xmpp_jid[128];
	char xmpp_server[128];
}SecureSOHO_XMPP;

/**
 * The object that represent the WAN seeting. It is the partial list of 
 * all SecureSOHO variables. The definition of each fields is quite 
 * self-describe. The type if WAN connection type,
 *  0: Disable.
 *  1: Static
 *  2: Dynamic
 *  3-4 : Obsolete
 *  5: PPPOE
 *  6: PPTP
 *  7: Wireless client mode
 */
typedef struct {
        int type;
	char ip[16];
	char mask[16];
	char gateway[16];
	char dns1[128];
	char dns2[128];
	char dns3[128];
} SecureSOHO_WAN;

/*
 * General settings 
 */
extern int securesoho_general_set(SecureSOHO_General *w, int restart);
extern int securesoho_general_get(SecureSOHO_General *w);

/*
 * Subtitle font size 
 */
extern int securesoho_subtitle_font_size_get(int* font_size);
extern int securesoho_subtitle_font_size_set(int font_size);

/*
 * Smart preview 
 */
extern int securesoho_smart_preview_get(int* smt_preview);
extern int securesoho_smart_preview_set(int smt_preview);

/*
 * Fast channel create wizard
 */
extern int securesoho_fast_channel_create_wizard_get(int* fccw);
extern int securesoho_fast_channel_create_wizard_set(int fccw);

/*
 *  Update the LAN setup and restart the system to use the new value if the 
 * <restart> is 1.
 */
extern int securesoho_lan_set(SecureSOHO_LAN *lan, int restart);
extern int securesoho_lan_get(SecureSOHO_LAN *lan);

/*
 *  Update the wireless setting and restart the system to use the new value if
 *  the 'restart' is 1.
 */
extern int securesoho_wireless_set(SecureSOHO_Wireless *wireless, int restart);
extern int securesoho_wireless_get(SecureSOHO_Wireless *wireless);

extern int securesoho_wireless_is_wifi_direct(void);

/* 
 * Preset module
 */
extern int securesoho_preset_map_get(int preset_key, int *index);
extern int securesoho_preset_map_set(int preset_key, int index);
extern int securesoho_preset_url_set(int preset_key,const char *url);
extern int securesoho_preset_url_get(int preset_key,char **url);

/* 
 * Firmware update module
 */
extern int securesoho_get_curversion(char *pVer);
extern int securesoho_get_curbuilddate(char *pBuild);
extern int securesoho_get_version_stage(char *stage, int len);
extern int securesoho_firmware_upgrade_info_get (char *FWUDVERINFOFILE, char *FWUDSERVER);
extern int securesoho_firmware_checker_status_get(int *status);
extern int securesoho_firmware_checker_status_set(int status);

/*
 *  Slide show interval
 */
extern int securesoho_slideshow_set(int slideshow);
extern int securesoho_slideshow_get(int *slideshow);

extern int securesoho_excp_name_set(char *excp_name);
extern int securesoho_excp_name_get(char *excp_name);
/*
   switch of samba share
*/
extern void securesoho_smbshare_set(int enable_smbshare);
extern int securesoho_smbshare_get();

/*
   Donwload/upload  speed limit and download count limit
*/
extern void securesoho_download_speed_limit_set(int value);
extern int securesoho_download_speed_limit_get();
extern void securesoho_upload_speed_limit_set(int value);
extern int securesoho_upload_speed_limit_get();
extern void securesoho_download_count_limit_set(int value);
extern int securesoho_download_count_limit_get();

extern void securesoho_last_channel_set(int number);
extern int securesoho_last_channel_get();

/*
 * Idle time of screensaver 
 */
extern int securesoho_screensaver_set(int minute,int enable);
extern int securesoho_screensaver_get(int *minute,int *enable);

/*
 * Current lan interface type being used
 */
extern int securesoho_lif_get(char *lif);
extern int securesoho_lif_type_get(CONNECTION *type);
extern int securesoho_lif_type_set(CONNECTION type);

/*
 *  Media Server
 */
extern int securesoho_media_server_set(const char *ServerHandle, const char*ServerName);
extern int securesoho_media_server_name_get(char *ServerName);
extern int securesoho_media_server_get(char *ServerHandle);

/*
 *  Samba Server
 */
int securesoho_default_samba_get(char *ServerHandle);
int securesoho_default_samba_name_get(char *ServerName);
int securesoho_default_samba_set(const char*ServerHandle, const char *ServerName);

/*
 * Filename ???
 */
extern int securesoho_filename_set(int filename);
extern int securesoho_filename_get(int *filename);

/*
 * Local for multilan
 */
extern void securesoho_locale_set(const char * localename);
extern void securesoho_locale_get(char * localename);

/*
 * City for weather
 */
extern void securesoho_city_set(const char* cityname);
extern void securesoho_city_get(char* cityname);

/**
 * get the prefix string of the netbios name.
 * netbios name looks like <NETBIOS NAME PREFIX>-<%02x%02x%02x>
 */
extern void securesoho_netbiosname_prefix_get(char *netbiosname_prefix);

/*
 * To get or set additional encoding type
 */
extern void securesoho_default_encoding_type_get(char *encoding_type);
extern void securesoho_text_encoding_type_set(const char * encoding_type);
extern void securesoho_text_encoding_type_get(char *encoding_type);

/*
 * Portal Local for multilan
 */
extern void securesoho_portal_locale_set(const char * localename);
extern void securesoho_portal_locale_get(char * localename);

/*
 * Model Name Get/Set
 */
extern void securesoho_model_set(const char *modelname);
extern void securesoho_model_get(char *modelname);

void securesoho_get_model_magic_number(char *magic_number);

/*
 *Remote type
 */
extern int securesoho_remotetype_get(int* remotetype);
extern void securesoho_remotetype_set(int remotetype);

/*
 * DMA Device name
 */
extern int securesoho_devicename_set(char *devicename);
extern int securesoho_devicename_get(char *devicename);

/*
 * Voulume
 */
extern int config_set_volume(int iVolumn);
extern int config_get_volume(int *piVolumn);
extern int config_get_mute(int *pMute);
extern int config_set_mute(int iMute);
/*
 * ip_type
 */
extern int securesoho_ip_type_set(int type);
extern int securesoho_ip_type_get(int *ptype);
/* 

 * The audo output interface mode 
 */
extern int securesoho_audiochannel_get(int* _5_1_or_2_channel);
extern void securesoho_audiochannel_set(int _5_1_or_2_channel);
extern int securesoho_tv_scan_get(int *tv_scan);
/* 
 * Photo show mode: slideshow/list 
 */

extern void securesoho_browsemode_set_by_type(int browsemode,int type);
extern int securesoho_browsemode_get_by_type(int* browsemode,int type);

extern void securesoho_browsemode_set(int browsemode);
extern int securesoho_browsemode_get(int* browsemode);

extern void securesoho_mass_product_test_procedure_get(int *mass_product_test_procedure);

/*
 * The type of remote controller
 */
extern int securesoho_remote_controller_get(char *rtype);
extern void securesoho_remote_controller_set(char *rtype);

/*
 * Scart
 */
extern	int securesoho_scart_set(int mode);
extern  int securesoho_model_name_get(char *model_name);

/*
 * Content aggregation
 */
#ifdef CONF_CAS
extern int securesoho_content_aggregation_get(int *scan);
extern int securesoho_content_aggregation_set(int scan);
#endif

/*
 * video gapless playback
 */
#ifdef CONF_LCB_REMUX
extern int securesoho_video_gapless_playback_get(int *mode);
extern int securesoho_video_gapless_playabck_set(int mode);
#endif


extern int securesoho_tvoutmode_get(int *piTvOutMode);
extern void securesoho_tvoutmode_set(int iTvOutMode);
extern int securesoho_tvoutres_get(int *piTvOutResolution);
extern void securesoho_tvoutres_set(int iTvOutResolution);
extern int securesoho_ntsc_pal_set(int ntsc_pal);

/*
 * slideshow effect for picture player
 */
#define CONFIG_SLIDESHOW_EFFECT_RANDOM  "0"
#define CONFIG_SLIDESHOW_EFFECT_FADE_IN "1"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_IN "2"
#define CONFIG_SLIDESHOW_EFFECT_OFF     "3"
#define CONFIG_SLIDESHOW_EFFECT_ZOOM	 "6"
#define CONFIG_SLIDESHOW_EFFECT_PUSH "7"
#define CONFIG_SLIDESHOW_EFFECT_REPLACE "8"
#define CONFIG_SLIDESHOW_EFFECT_CHECKER "9"
#define CONFIG_SLIDESHOW_EFFECT_CROSS_FADE "10"
#define CONFIG_SLIDESHOW_EFFECT_STRIP_LEFT_DOWN "12"
#define CONFIG_SLIDESHOW_EFFECT_SNAKE "13"
#define CONFIG_SLIDESHOW_EFFECT_DISSOLVE "14"
#define CONFIG_SLIDESHOW_EFFECT_WATER_FALL "15"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_LEFT "16"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_RIGHT "17"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_TOP "18"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_BOTTOM "19"
#define CONFIG_SLIDESHOW_EFFECT_WIPE_ALL "20"
#define CONFIG_SLIDESHOW_EFFECT_REPLACE_LEFT "21"
#define CONFIG_SLIDESHOW_EFFECT_REPLACE_RIGHT "22"
#define CONFIG_SLIDESHOW_EFFECT_REPLACE_TOP "23"
#define CONFIG_SLIDESHOW_EFFECT_REPLACE_BOTTOM "24"

extern int securesoho_photo_slideshow_effect_get(char *slideshow_effect);
extern void securesoho_photo_slideshow_effect_set(char *slideshow_effect);
extern int securesoho_dma_first_boot_get(int* is_boot_first);
extern int securesoho_dma_first_boot_set(int is_boot_first);

/*
 * mcx auto startup 
 */
#ifdef CONF_MCX
extern int securesoho_boot_first_get(int* is_boot_first);
extern int securesoho_boot_fist_set(int is_boot_first);
extern int securesoho_mcx_cert_partition_get(char *cert, int len);
extern int securesoho_mcx_setting_partition_get(char *path, int len);
extern int securesoho_mcx_auto_startup_get(char* autostartup);
extern void securesoho_mcx_auto_startup_set(char* autostartup);
extern int securesoho_mcx_green_btn_get(char* enable);
extern void securesoho_mcx_green_btn_set(char* enable);
extern int securesoho_is_green_button_triggered(void);
#endif
/* 
 *         tv mode atribute setting
 *
 */
#define CONFIG_DMA_ENABLE_PAL   "CONFIG_DMA_ENABLE_PAL"
#define CONFIG_AUDIO_OUT        "AUDIO_OUT"
#define CONFIG_VIDEO_OUT        "VIDEO_OUT"
#define CONFIG_VIDEO_OUT_RES    "VIDEO_OUT_RESOLUTION"
#define CONFIG_VIDEO_PIP        "VIDEO_PIP"
#define CONFIG_TV_SCAN          "TV_SCAN"
#define CONFIG_SCAN_MODE        "SCAN_MODE"
#define CONFIG_TV_TRICK         "CONFIG_TV_TRICK"
#define CONFIG_NTSC_OR_PAL	    "NTSC_OR_PAL"
#define CONFIG_LIGHTNESS    "LIGHTNESS"
#define CONFIG_CONTRAST	    "CONTRAST"
#define CONFIG_1080P_DISPLAY_MODE	    "1080P_DISPLAY_MODE"

#define CONFIG_SAMBAR_AUTO_LOGIN	    "SAMBA_AUTO_LOGIN"
#define CONFIG_SAMBAR_AUTO_CLEAR_LOGIN_INFO	    "SAMBA_AUTO_CLEAR_LOGIN_INFO"
#define CONFIG_POWER_MODE	    "POWER_MODE"
#define CONFIG_DIVX_VOD	"DIVX_VOD"

#define CONFIG_NETWORK_SERVICE_AGREE_NOTICE	    "NETWORK_SERVICE_AGREE_NOTICE"

#define CONFIG_SCREENSAVER_TYPE	"SCREENSAVER_TYPE"


#ifdef CONF_AUDIO_DTSHD_OUTPUT
#define CONFIG_AUDIO_DTSHD_OUT        "AUDIO_DTSHD_OUT"
#endif

/*
 * HDMI HD screen resolution 
 */ 
#define HDMI_HD_SCREEN_WIDHT_16x9  1024 
#define HDMI_HD_SCREEN_HEIGHT_16x9 576
#define HDMI_HD_SCREEN_WIDTH_4x3   768
#define HDMI_HD_SCREEN_HEIGHT_4x3  576 

#define SCREEN_WIDHT_SD_16x9	   853
#define SCREEN_HEIGHT_SD_16x9      480
#define SCREEN_WIDHT_SD_4x3        640 
#define SCREEN_HEIGHT_SD_4x3       480

#define  MSK_NTSC_PAL           0x00000001
#define  MSK_TV_SCAN            0x00000002
#define  MSK_VIDEO_OUT          0x00000004
#define  MSK_VIDEO_OUT_RES      0x00000008
#define  MSK_AUDIO_OUT          0x00000010
#define  MSK_VIDEO_PIP          0x00000020
#define  MSK_SCAN_MODE          0x00000040
#define  MSK_TRICK_MODE         0x00000080

#ifdef CONF_AUDIO_DTSHD_OUTPUT
#define  MSK_AUDIO_DTSHD_OUT    0x00000100
#endif

typedef enum{
	VM_COMPOSITE = 0,
	VM_COMPONENT,
	VM_HDMI,
	VM_RGB,
	VM_RGB_SCART_INTERLACE,
	VM_RGB_SCART_PROGRESSIVE,
	VM_S_VIDEO_AV
}VIDEO_MODE;

typedef enum{
	VRM_480I60 = 0,
	VRM_480P60,
	VRM_576I50,
	VRM_576P50,
	VRM_720P50,
	VRM_720P60,
	VRM_1080I50,
	VRM_1080I60,
	VRM_AUTO,
	VRM_1080P50,
	VRM_1080P60,
	VRM_1080P24
}VIDEO_RES_MODE;

typedef enum{
	TS_4_3_LB=0,
	TS_4_3_PS,
	TS_16_9_SD=32
}TV_SCAN_MODE;

typedef enum{
	NPM_PAL=0,
	NPM_NTSC
}NTSC_PAL_MODE;

typedef enum{
	AOM_ANALOG=0,
	AOM_SPDIF,
	AOM_HDMI
}AUDIO_OUT_MODE;

typedef enum{
	VTM_FF_REV=0,
	VTM_JUMP
}VIDEO_TRICK_MODE;

typedef enum{
	VPM_NO=0,
	VPM_YES
}VIDEO_PIP_MODE;


typedef enum{
	SM_PROGRESSIVE=0,
	SM_INTERLACE
}SCAN_MODE;

typedef enum{
	CS_AUTO = 0,
	CS_RGB,
	CS_YCC_601,
	CS_YCC_709,
	CS_YCC_601f,
	CS_YCC_709f,
	CS_RGBl,
	CS_xvYCC601,
	CS_xvYCC709,
	CS_xvYCC601f,
	CS_xvYCC709f,
}COLOR_SPACE_MODE;

typedef struct  _tv_av_setting_t_
{
	unsigned int set_mask;
	unsigned char ntsc_pal;
	unsigned char tv_scan;
	unsigned char  video_out;
	unsigned char video_out_res;
	unsigned char audio_out;
	unsigned char video_pip;
	unsigned char scan_mode;
	unsigned char trick_mode;
#ifdef CONF_AUDIO_DTSHD_OUTPUT
	unsigned char audio_dtshd_out;
	unsigned char reserved[3];
#else
	unsigned char reserved[4];
#endif
}tv_av_setting_t;

void securesoho_get_tv_setting(tv_av_setting_t *tv_mode);
void securesoho_save_tv_setting(tv_av_setting_t *tv_mode);
unsigned char securesohu_init_tv_setting(void);
void securesoho_tvout_trick_get(int* tvtrickmode);
void securesoho_tvout_trick_set(int trickmode);
int securesoho_real_tvouttype_get(int *video_out, int *video_out_res);
int securesoho_real_tvouttype_set(int video_out, int video_out_res);
int securesoho_get_osd_page_info(int *width, int *height);

int securesoho_get_colorspace(int *colorspace);
int securesoho_set_colorspace(int colorspace);
#ifdef CONF_AUDIO_DTSHD_OUTPUT
int securesoho_audio_dtshd_get(int* _dtshd_out);
void securesoho_audio_dtshd_set(int _dtshd_out);
#endif
/*
 * get the TV mode from IrDA device
 */
int securesoho_ntsc_pal_get(int* ntsc_pal);

int backup_config(void);
int restore_config(void);

/*
 * This API is used to do factory default
 *   1) replace the config with factory default ('/conf_src/config_factory_default'->'/conf/config')
 *   2) remove wireless profile settings ('/conf/wireless_profile.xml')
 */
int securesoho_factory_default(void);

void securesoho_wcn_status_led(int ok);
int securesoho_save_cert_dir(char *i_dir, char *o_filename);
#endif //__SECURESOHO_CONFIG_H__
