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
#include "securesoho_wireless.h"
#include "util_wireless.h"
#include "driver_wext.h"
#include "driver_ralink.h"

static void wireless_driver_rt3572_insert(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	do_cmd("/sbin/insmod", "/lib/modules/rt3572sta.ko", NULL);
	printf("\033[1;32m%s:%d, wlan->wlan_name=%s\033[0m\n", __FUNCTION__, __LINE__, wlan->wlan_name);
}

static void wireless_driver_rt3572_remove(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	do_cmd("/sbin/ifconfig", wlan->wlan_name, "down", NULL);
	do_cmd("/sbin/rmmod", "rt3572sta", NULL);
	close(wlan->wlan_skfd);
}

#define WIRELESS_RT3572_ENC_SUPPORTED	WIRELESS_ENCRYPT_NONE | WIRELESS_ENCRYPT_WEP |	\
					WIRELESS_ENCRYPT_TKIP | WIRELESS_ENCRYPT_AES 

struct usb_id_s rt3572_usb_id[] = {
	/*
	 * RT35xx. 
	 * Got from 2009_0819_RT3572_Linux_STA_V2.2.0.0
	 * osd/linux/usb_main_dev.c
	 */
	{0x148F,0x3572}, /* Ralink 3572 */
	{0x1740,0x9801}, /* EnGenius 3572 */
	{0x0DF6,0x0041}, /* Sitecom 3572 */
	{0x0DF6,0x0042},
	{0x04BB,0x0944}, /* I-O DATA 3572 */
	{0x1690,0x0740}, /* 3572 */
	{0x1690,0x0744}, /* 3572 */
	{0x5A57,0x0284}, /* Zinwell 3572 */
	{0x167B,0x4001}, /* 3572 */

	/*
	 * RT3572.  Purchased from the market. 
	 */
	{0x1737,0x0079}, /* Linsys WUSB600N ver.2 */

	/*
	 * RT2870. 
	 * Got from 2009_0819_RT3572_Linux_STA_V2.2.0.0
	 * osd/linux/usb_main_dev.c
	 */
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

	/*
	 * RT2870. Purchased from the market.
	 */
	{0x0411,0x0148}, /* Buffalo WLI-UC-G300HP */
	{0x157e,0x3013}, /* TRENDnet TEW-645UB H/W:V1.0R */
	{0x050D,0x825A}, /* Belkin F5D8055 v1 */
	{0x07d1,0x3c09}, /* D-Link DWA-140 H/W Ver.: B1 */
	{0x1737,0x0071}, /* Linksys WUSB600N */

	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_rt3572_ops = {
	.wlan_vendor		= "Ralink",
	.wlan_chip_id		= "3572",
	.wlan_usb_dongle	= 1,
	.wlan_addr		= rt3572_usb_id,
	.wlan_devpath		= "",
	.wlan_name		= "ra0",
	.wlan_p2p_name		= "p2p0",
	.wlan_scan_delay	= 10,
	.wlan_associated	= 1,
	.wlan_enc_supported	= WIRELESS_RT3572_ENC_SUPPORTED,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 9,
	.wlan_rsn_ie_offset	= 10,
	.wlan_scan_event	= 0,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_rt3572_insert,
	.wlan_remove		= wireless_driver_rt3572_remove,
	.wlan_down		= wireless_driver_ralink_down,
	.wlan_init		= wireless_driver_ralink_init,
	.wlan_reset		= wireless_driver_ralink_reset,
	.wlan_scan		= wireless_driver_ralink_scan,
	.wlan_set_profile_wep	= wireless_driver_ralink_set_profile,
	.wlan_set_profile_wpa	= wireless_driver_ralink_set_profile,
	.wlan_get_profile	= wireless_driver_ralink_get_profile,
	.wlan_get_status	= wireless_driver_ralink_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_ralink_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_ralink_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_ralink_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_ralink_linkup,
	.wlan_linkdown		= wireless_driver_ralink_linkdown,
	.wlan_get_pincode       = wireless_driver_ralink_getpincode,
	.wlan_get_wps_status    = wireless_driver_ralink_getwpsstatus,
};
