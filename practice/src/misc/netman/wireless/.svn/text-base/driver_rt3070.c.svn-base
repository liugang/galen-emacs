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

static void wireless_driver_rt3070_insert(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	do_cmd("/sbin/insmod", "/lib/modules/rtutil3070sta.ko", NULL);
	do_cmd("/sbin/insmod", "/lib/modules/rt3070sta.ko", NULL);
	do_cmd("/sbin/insmod", "/lib/modules/rtnet3070sta.ko", NULL);
	printf("\033[1;32m%s:%d, wlan->wlan_name=%s\033[0m\n", __FUNCTION__, __LINE__, wlan->wlan_name);
}

static void wireless_driver_rt3070_remove(void)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	do_cmd("/sbin/ifconfig", wlan->wlan_name, "down", NULL);
	do_cmd("/sbin/rmmod", "rtnet3070sta", NULL);
	do_cmd("/sbin/rmmod", "rt3070sta", NULL);
	do_cmd("/sbin/rmmod", "rtutil3070sta", NULL);
	close(wlan->wlan_skfd);
}

#define WIRELESS_RT3070_ENC_SUPPORTED	WIRELESS_ENCRYPT_NONE | WIRELESS_ENCRYPT_WEP |	\
					WIRELESS_ENCRYPT_TKIP | WIRELESS_ENCRYPT_AES 

struct usb_id_s rt3070_usb_id[] = {
	/*
	 * The following got form :2009_1229_RT3070_LinuxSTA_V2.3.0.0
	 * NETIF/common/rtusb_dev_id.c
	 */
	{0x148F,0x3070}, /* Ralink 3070 */
	{0x148F,0x3071}, /* Ralink 3071 */
	{0x148F,0x3072}, /* Ralink 3072 */
	{0x0DB0,0x3820}, /* Ralink 3070 */
	{0x0DB0,0x871C}, /* Ralink 3070 */
	{0x0DB0,0x822C}, /* Ralink 3070 */
	{0x0DB0,0x871B}, /* Ralink 3070 */
	{0x0DB0,0x822B}, /* Ralink 3070 */
	{0x0DF6,0x003E}, /* Sitecom 3070 */
	{0x0DF6,0x0042}, /* Sitecom 3072 */
	{0x0DF6,0x0048}, /* Sitecom 3070 */
	{0x0DF6,0x0047}, /* Sitecom 3071 */
	{0x14B2,0x3C12}, /* AL 3070 */
	{0x18C5,0x0012}, /* Corega 3070 */
	{0x083A,0x7511}, /* Arcadyan 3070 */
	{0x083A,0xA701}, /* SMC 3070 */
	{0x083A,0xA702}, /* SMC 3072 */
	{0x1740,0x9703}, /* EnGenius 3070 */
	{0x1740,0x9705}, /* EnGenius 3071 */
	{0x1740,0x9706}, /* EnGenius 3072 */
	{0x1740,0x9707}, /* EnGenius 3070 */
	{0x1740,0x9708}, /* EnGenius 3071 */
	{0x1740,0x9709}, /* EnGenius 3072 */
	{0x13D3,0x3273}, /* AzureWave 3070*/
	{0x13D3,0x3305}, /* AzureWave 3070*/
	{0x1044,0x800D}, /* Gigabyte GN-WB32L 3070 */
	{0x2019,0xAB25}, /* Planex Communications, Inc. RT3070 */
	{0x07B8,0x3070}, /* AboCom 3070 */
	{0x07B8,0x3071}, /* AboCom 3071 */
	{0x07B8,0x3072}, /* Abocom 3072 */
	{0x7392,0x7711}, /* Edimax 3070 */
	{0x1A32,0x0304}, /* Quanta 3070 */
	{0x1EDA,0x2310}, /* AirTies 3070 */
	{0x07D1,0x3C0A}, /* D-Link 3072*/
	{0x07d1,0x3C0D}, /* D-Link DWA-125 H/W Ver.: A1 (1T1R) */
	{0x07D1,0x3C0E}, /* D-Link 3070 */
	{0x07D1,0x3C0F}, /* D-Link 3070 */
	{0x07d1,0x3C16}, /* D-Link DWA-125 H/W Ver.: A2	2010.Mar.16 */
	{0x1D4D,0x000C}, /* Pegatron Corporation 3070 */
	{0x1D4D,0x000E}, /* Pegatron Corporation 3070 */
	{0x5A57,0x5257}, /* Zinwell 3070 */
	{0x5A57,0x0283}, /* Zinwell 3072 */
	{0x04BB,0x0945}, /* I-O DATA 3072 */
	{0x04BB,0x0947}, /* I-O DATA 3070 */
	{0x04BB,0x0948}, /* I-O DATA 3072 */
	{0x203D,0x1480}, /* Encore 3070 */
	{0x20B8,0x8888}, /* PARA INDUSTRIAL 3070 */
	{0x0B05,0x1784}, /* Asus 3072 */
	{0x203D,0x14A9}, /* Encore 3070*/
	{0x0DB0,0x899A}, /* MSI 3070*/
	{0x0DB0,0x3870}, /* MSI 3070*/
	{0x0DB0,0x870A}, /* MSI 3070*/
	{0x0DB0,0x6899}, /* MSI 3070 */
	{0x0DB0,0x3822}, /* MSI 3070 */
	{0x0DB0,0x3871}, /* MSI 3070 */
	{0x0DB0,0x871A}, /* MSI 3070 */
	{0x0DB0,0x822A}, /* MSI 3070 */
	{0x0DB0,0x3821}, /* Ralink 3070 */
	{0x0DB0,0x821A}, /* Ralink 3070 */
	{0x5A57,0x0282}, /* zintech 3072 */	
	{0x083A,0xA703}, /* IO-MAGIC */
	{0x13D3,0x3307}, /* Azurewave */
	{0x13D3,0x3321}, /* Azurewave */
	{0x07FA,0x7712}, /* Edimax */
	{0x0789,0x0166}, /* Edimax */

	/*
	 * Got vID/pID from products purchased from the market, 
	 * which have used the RT3070 chipset.
	 */
	{0x2019,0xed14}, /* Planex GW-USMicroN	2009.Nov.02 */
	{0x050D,0x825B}, /* Belkin F5D8055 v2 */
	{0x050D,0x935A}, /* Belkin F6D4050 v1 */
	{0x050D,0x935B}, /* Belkin F6D4050 v2 */ 
	{0x0411,0x0158}, /* Buffalo WLI-UC-GNHP	2009.Nov.02 */
	{0x0411,0x015d}, /* Buffalo WLI-UC-GN */
	{0x0411,0x016F}, /* Buffalo G300N ver. 1.01 2010.May.06 */
	{0x0586,0x341a}, /* Zyxel NWD-270N 2009.Nov.02 */
	{0x0e66,0x0013}, /* Hawking HWUN3 2010.Feb.25 */
	{0xFFFF,0xFFFF}/* Terminating entry */
};

struct wireless_driver_ops wireless_driver_rt3070_ops = {
	.wlan_vendor		= "Ralink",
	.wlan_chip_id		= "3070",
	.wlan_usb_dongle	= 1,
	.wlan_addr		= rt3070_usb_id,
	.wlan_devpath		= "",
	.wlan_name		= "ra0",
	.wlan_p2p_name		= "p2p0",
	.wlan_scan_delay	= 10,
	.wlan_associated	= 1,
	.wlan_enc_supported	= WIRELESS_RT3070_ENC_SUPPORTED,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 9,
	.wlan_rsn_ie_offset	= 10,
	.wlan_scan_event	= 0,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_rt3070_insert,
	.wlan_remove		= wireless_driver_rt3070_remove,
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
