#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "util_wireless.h"

const WIRELESS_MODE iw_mode[] = { WIRELESS_AUTO, WIRELESS_ADHOC, WIRELESS_INFRASTRUCTURE,
	WIRELESS_MASTER, WIRELESS_REPEATER, WIRELESS_SECONDARY, WIRELESS_MONITOR};

static struct scan_result gArScan_result[MAX_SCAN_RESULT];
static unsigned char gArWlan_buffer[IW_SCAN_MAX_DATA];
static int giCount;

int *get_giCount()
{
	return &giCount;
}

unsigned char *get_gArWlan_buffer()
{
	return (unsigned char *)gArWlan_buffer;
}

char *get_gArScan_result()
{
	return (char *)&gArScan_result;
}

static void wireless_driver_wext_insert()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}
static void wireless_driver_wext_down(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_wext_remove(void)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

static void wireless_driver_wext_init()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

int wireless_driver_wext_wpa(int mode, char *key)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

static int wireless_driver_wext_getlinkstatus(int *status)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

static int wireless_driver_wext_set_profile(SecureSOHO_Wireless *conf, int timeout)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);

	return -1;
}

struct SITE_OBJ *wireless_driver_wext_get_scan_results(int *cnt, WIRELESS_MODE mode)
{
	int i;
	struct SITE_OBJ *site = NULL, *retval = NULL;

	*cnt = 0;
	for (i = 0; i< giCount; i++) {
		if (*gArScan_result[i].ssid == 0){
			continue;
		}
		if (mode != gArScan_result[i].mode && mode != WIRELESS_BOTH){
			continue;
		}
		if ((site = (struct SITE_OBJ *)malloc(sizeof(struct SITE_OBJ))) < 0) {
			printf("(%s:%d) Out of memory.\n", __FUNCTION__, __LINE__);
			return NULL;
		}
		memset(site, 0, sizeof(struct SITE_OBJ));
		*cnt = *cnt + 1;
		memcpy(site->essid, &gArScan_result[i].ssid, gArScan_result[i].ssid_len);
		site->essid[gArScan_result[i].ssid_len] = 0;
		memcpy(site->wpa_ie, &gArScan_result[i].wpa_ie, gArScan_result[i].wpa_ie_len);
		site->wpa_ie[gArScan_result[i].wpa_ie_len] = 0;
		if (gArScan_result[i].wpa_ie_len > 0) {
			parse_wpa_ie((unsigned char*)site->wpa_ie, &site->wpa);
		}
		memcpy(site->rsn_ie, &gArScan_result[i].rsn_ie, gArScan_result[i].rsn_ie_len);
		site->rsn_ie[gArScan_result[i].rsn_ie_len] = 0;
		if (gArScan_result[i].rsn_ie_len > 0) {
			parse_rsn_ie((unsigned char*)site->rsn_ie, &site->wpa);
		}
		site->encrypt_type = gArScan_result[i].crypt;
//		site->channel = &result[i]->freq;
		site->wireless_type =  gArScan_result[i].mode;
		site->signal_dbm = gArScan_result[i].signal_dbm;
		site->signal_percentage = gArScan_result[i].signal_percentage;
		site->wireless_mode = gArScan_result[i].wireless_type;

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

/*------------------------------------------------------------------*/
/*
 * Read /proc/net/wireless to get the latest statistics
 */
#define PROC_NET_WIRELESS "/proc/net/wireless"
int iw_get_proc_stats(const char * ifname, struct iw_statistics *stats)
{
	FILE *	f = fopen(PROC_NET_WIRELESS, "r");
	char	buf[256];
	char *	bp;
	int	t;

	if(f==NULL)
		return -1;
	/* Loop on all devices */
	while(fgets(buf,255,f))
	{
		bp=buf;
		while(*bp&&isspace(*bp))
			bp++;
		/* Is it the good device ? */
		if(strncmp(bp,ifname,strlen(ifname))==0 && bp[strlen(ifname)]==':')
		{
			/* Skip ethX: */
			bp=strchr(bp,':');
			bp++;
			/* -- status -- */
			bp = strtok(bp, " ");
			sscanf(bp, "%X", &t);
			stats->status = (unsigned short) t;
			/* -- link quality -- */
			bp = strtok(NULL, " ");
			if(strchr(bp,'.') != NULL)
				stats->qual.updated |= 1;
			sscanf(bp, "%d", &t);
			stats->qual.qual = (unsigned char) t;
			/* -- signal level -- */
			bp = strtok(NULL, " ");
			if(strchr(bp,'.') != NULL)
				stats->qual.updated |= 2;
			sscanf(bp, "%d", &t);
			stats->qual.level = (unsigned char) t;
			/* -- noise level -- */
			bp = strtok(NULL, " ");
			if(strchr(bp,'.') != NULL)
				stats->qual.updated += 4;
			sscanf(bp, "%d", &t);
			stats->qual.noise = (unsigned char) t;
			/* -- discarded packets -- */
			bp = strtok(NULL, " ");
			sscanf(bp, "%d", &stats->discard.nwid);
			bp = strtok(NULL, " ");
			sscanf(bp, "%d", &stats->discard.code);
			bp = strtok(NULL, " ");
			sscanf(bp, "%d", &stats->discard.misc);
			fclose(f);
			/* No conversion needed */
			return 0;
		}
	}
	fclose(f);
	return -1;
}

int wireless_driver_wext_get_signal_strength(int *signal)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	struct iw_statistics stats;

	if (NULL == wlan) return -1;
#if 0
	int skfd;
	struct iwreq wrq;
	if (( skfd = socket( AF_INET, SOCK_DGRAM, 0 ) ) < 0 ) {
		perror("wireless_driver_wext_get_signal_strength() socket:");
		return -1;
	}
	strncpy(wrq.ifr_name, wlan->wlan_name, sizeof(wrq.ifr_name)-1);
	wrq.u.data.pointer = (caddr_t) &stats;
	wrq.u.data.length = sizeof(struct iw_statistics);
	wrq.u.data.flags = 1;
	if(ioctl(skfd, SIOCGIWSTATS, &wrq) < 0) {
		perror("wireless_driver_wext_get_signal_strength() ioctl:");
		close(skfd);
		return -1;
	}
	close(skfd);
#else
	iw_get_proc_stats(wlan->wlan_name, &stats);
#endif
	*signal = dbm2percentage(stats.qual.level - 256);
	return 0;
}

void wireless_driver_wext_reset()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_wext_get_wpa_linkstate(int *state)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_wext_linkup()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_wext_linkdown()
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_wext_set_scan_completed_cb(void (*cb)(void *user_data), void *user_data)
{
	printf("(%s:%d) ENTER\n", __FUNCTION__, __LINE__);
}

void wireless_driver_wext_scan_completed()
{
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();
	struct iwreq iwr;
	int skfd=-1;
	int ap_num = 0;
	unsigned char *res_buf;
	struct iw_event iwe_buf, *iwe = &iwe_buf;
	char *pos, *end, *custom;
	size_t len, clen, res_buf_len;

	if ((skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("%s: create socket fail\n", __FUNCTION__);
		return;
	}
	
	res_buf_len = IW_SCAN_MAX_DATA;
	memset(gArWlan_buffer, 0, IW_SCAN_MAX_DATA);
	while (1) {
		res_buf = (unsigned char *)&gArWlan_buffer;
		memset(&iwr, 0, sizeof(iwr));
		strncpy(iwr.ifr_name, wlan->wlan_name, IFNAMSIZ);
		iwr.u.data.pointer = res_buf;
		iwr.u.data.length = res_buf_len;

		if (ioctl(skfd, SIOCGIWSCAN, &iwr) == 0)
			break;

		perror("ioctl[SIOCGIWSCAN]");
		goto error;
	}

	len = iwr.u.data.length;

	pos = (char *) res_buf;
	end = (char *) res_buf + len;

	memset(gArScan_result, 0, sizeof(struct scan_result)*MAX_SCAN_RESULT);
	while (pos + IW_EV_LCP_LEN <= end) {
		int ssid_len;
		int div = 1000000, i=0;

		/* Event data may be unaligned, so make a local, aligned copy
		 * before processing. */
		memcpy(&iwe_buf, pos, sizeof(struct iw_event));
		if (iwe->len <= IW_EV_LCP_LEN)
			break;
		switch (iwe->cmd) {
		case SIOCGIWAP:
			ap_num++;
//			memcpy(new->bssid, iwe->u.ap_addr.sa_data, ETH_ALEN);
			break;
		case SIOCGIWMODE:
			gArScan_result[ap_num-1].mode = iw_mode[iwe->u.mode];
			break;
		case SIOCGIWESSID:
			ssid_len = iwe->u.essid.length;
			custom = pos + IW_EV_POINT_LEN;
			if (custom + ssid_len > end)
				break;
			if (iwe->u.essid.flags &&
			    ssid_len > 0 &&
			    ssid_len <= IW_ESSID_MAX_SIZE) {
				if (ap_num <= MAX_SCAN_RESULT) {
					memcpy(&gArScan_result[ap_num-1].ssid, custom, ssid_len);
					gArScan_result[ap_num-1].ssid_len = ssid_len;
				}
			}
			if (ssid_len == 0)
				gArScan_result[ap_num-1].ssid_len = ssid_len;
			break;
		case SIOCGIWFREQ:
			if (iwe->u.freq.e > 6) {
				printf("(%s:%d)Invalid freq.\n", __FUNCTION__, __LINE__);
#if 0
				printf( "Invalid freq "
					"in scan results (BSSID="
					MACSTR ": m=%d e=%d\n",
					MAC2STR(results[ap_num].bssid),
					iwe->u.freq.m, iwe->u.freq.e);
#endif
				break;
			}
			for (i = 0; i < iwe->u.freq.e; i++)
				div /= 10;
			if (ap_num <= MAX_SCAN_RESULT)
				gArScan_result[ap_num-1].freq = iwe->u.freq.m / div;
			break;
		case IWEVQUAL:
			if (ap_num <= MAX_SCAN_RESULT) {
				gArScan_result[ap_num-1].signal_dbm = iwe->u.qual.level - 256;
				gArScan_result[ap_num-1].signal_percentage = dbm2percentage(gArScan_result[ap_num-1].signal_dbm);
			}
			break;
		case SIOCGIWENCODE:
			if (ap_num <= MAX_SCAN_RESULT)
				gArScan_result[ap_num-1].crypt = (iwe->u.data.flags & IW_ENCODE_DISABLED)?0:1;
			break;
		case SIOCGIWRATE:
			break;
#if 0
		case IWEVGENIE:
			gpos = genie = pos + IW_EV_POINT_LEN;
			gend = genie + iwe->u.data.length;
			if (gend > end) {
				printf("IWEVGENIE overflow");
				break;
			}
			while (gpos + 1 < gend &&
			       gpos + 2 + (unsigned char) gpos[1] <= gend) {
				unsigned char ie = gpos[0], ielen = gpos[1] + 2;
				if (ielen > SSID_MAX_WPA_IE_LEN) {
					gpos += ielen;
					continue;
				}
				switch (ie) {
				case GENERIC_INFO_ELEM:
					if (ielen < 2 + 4 ||
					    memcmp(&gpos[2],
						   "\x00\x50\xf2\x01", 4) != 0)
						break;
					memcpy(new->wpa_ie, gpos, ielen);
					new->wpa_ie[ie_len] = 0;
					break;
				case RSN_INFO_ELEM:
					memcpy(new->rsn_ie, gpos, ielen);
					new->rsn_ie[ie_len] = 0;
					break;
				}
				gpos += ielen;
			}
			break;
#endif
		case IWEVCUSTOM:
			custom = pos + IW_EV_POINT_LEN;
			clen = iwe->u.data.length;
			if (custom + clen > end)
				break;
			if (clen > 7 && (strncmp(custom, "wpa_ie", 6) == 0 || strncmp(custom, "WPA_IE", 6) == 0)) {
				char *spos;
				int bytes;

				spos = custom + wlan->wlan_wpa_ie_offset;
				bytes = custom + clen - spos;
				if (bytes & 1)
					break;
				bytes /= 2;
				if (bytes > SSID_MAX_WPA_IE_LEN) {
					printf("Too long WPA IE "
						   "(%d)", bytes);
					break;
				}
				if (ap_num <= MAX_SCAN_RESULT) {
					memcpy(&gArScan_result[ap_num-1].wpa_ie, spos, bytes * 2);
					gArScan_result[ap_num-1].wpa_ie_len = bytes * 2;
				}
			} else if (clen > 7 && (strncmp(custom, "rsn_ie", 6) == 0 || strncmp(custom, "WPA2_IE", 7) == 0)) {
				char *spos;
				int bytes;

				spos = custom + wlan->wlan_rsn_ie_offset;
				bytes = custom + clen - spos;
				if (bytes & 1)
					break;
				bytes /= 2;
				if (bytes > SSID_MAX_WPA_IE_LEN) {
					printf("Too long RSN IE "
						   "(%d)", bytes);
					break;
				}
				if (ap_num <= MAX_SCAN_RESULT) {
					memcpy(&gArScan_result[ap_num-1].rsn_ie, spos, bytes * 2);
					gArScan_result[ap_num-1].rsn_ie_len = bytes * 2;
				}
			} else if (clen > 7 && (strncmp(custom, "Mode", 4) == 0)) {
				/* Marvell driver: wireless mode */
				char *spos;
				int bytes;
				char buf[16];

				spos = custom + 7;
				bytes = custom + clen - spos;
				memset(buf, 0, 16);
				memcpy(buf, spos, bytes);
				if (ap_num <= MAX_SCAN_RESULT) {
					if (strcasestr(buf, "11a/b/g/n")) {
						gArScan_result[i].wireless_type = WIRELESS_11ABGN_MIXED;
					} else if (strcasestr(buf, "11a/b/g")) {
						gArScan_result[i].wireless_type = WIRELESS_LEGACY_11ABG_MIXED;
					} else if (strcasestr(buf, "11b/g/n")) {
						gArScan_result[i].wireless_type = WIRELESS_11BGN_MIXED;
					} else if (strcasestr(buf, "11a/g/n")) {
						gArScan_result[i].wireless_type = WIRELESS_11AGN_MIXED;
					} else if (strcasestr(buf, "11b/g")) {
						gArScan_result[i].wireless_type = WIRELESS_LEGACY_11BG_MIXED;
					} else if (strcasestr(buf, "11g/n")) {
						gArScan_result[i].wireless_type = WIRELESS_11GN_MIXED;
					} else if (strcasestr(buf, "11a/n")) {
						gArScan_result[i].wireless_type = WIRELESS_11AN_MIXED;
					} else if (strcasestr(buf, "11a")) {
						gArScan_result[i].wireless_type = WIRELESS_LEGACY_11A_ONLY;
					} else if (strcasestr(buf, "11b")) {
						gArScan_result[i].wireless_type = WIRELESS_LEGACY_11B_ONLY;
					} else if (strcasestr(buf, "11g")) {
						gArScan_result[i].wireless_type = WIRELESS_LEGACY_11G_ONLY;
					} else if (strcasestr(buf, "11n")) {
						gArScan_result[i].wireless_type = WIRELESS_11N_ONLY;
					}
				}
			}
			break;
		}

		pos += iwe->len;
	}
	giCount = ap_num;
error:
	if(skfd > 0) close(skfd);
}

int wireless_driver_wext_set_ssid(const char *ssid, int ssid_len)
{
	int 				skfd = 0;
	int 				ret = 0;
	char 				buf[33];
	struct iwreq 			iwr;
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	if (ssid_len > 32)
		return -1;

	if ((skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("%s: create socket fail\n", __FUNCTION__);
		return -1;
	}
	
	memset(&iwr, 0, sizeof(iwr));
	strncpy(iwr.ifr_name, wlan->wlan_name, IFNAMSIZ);
	/* flags: 1 = ESSID is active, 0 = not (promiscuous) */
	iwr.u.essid.flags = (ssid_len != 0);
	memset(buf, 0, sizeof(buf));
	memcpy(buf, ssid, ssid_len);
	printf("(%s:%d) ssid = %s, ssid_len = %d\n", __FUNCTION__, __LINE__, buf, ssid_len);
	iwr.u.essid.pointer = (caddr_t) buf;
	/* For historic reasons, set SSID length to include one extra
	 * character, C string nul termination, even though SSID is really an
	 * octet string that should not be presented as a C string. Some Linux
	 * drivers decrement the length by one and can thus end up missing the
	 * last octet of the SSID if the length is not incremented here. */
	iwr.u.essid.length = ssid_len ? ssid_len + 1 : 0;

	if (ioctl(skfd, SIOCSIWESSID, &iwr) < 0) {
		perror("ioctl[SIOCSIWESSID]");
		ret = -1;
	}

	close(skfd);

	return ret;
}

int wireless_driver_wext_scan(const char *ssid, int ssid_len)
{
	int 				skfd = 0;
	struct iwreq			wrq;
	struct wireless_driver_ops 	*wlan = securesoho_get_wireless_card();

	if (ssid && ssid_len > 0) {
		if (wireless_driver_wext_set_ssid(ssid, ssid_len) < 0) {
			printf("%s: set ssid fail\n", __FUNCTION__);
			return -1;
		}
	}
	if ((skfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		printf("%s: create socket fail\n", __FUNCTION__);
		return -1;
	}
	
	memcpy(wrq.ifr_name, wlan->wlan_name, IFNAMSIZ);
	
	wrq.u.data.pointer = NULL;
	wrq.u.data.flags = 0;
	wrq.u.data.length = 0;

	if (ioctl(skfd, SIOCSIWSCAN, &wrq) < 0) {
		perror("SIOCSIWSCAN");
		close(skfd);
		return -1;
	}

	close(skfd);
	
	return 0;
}

struct wireless_driver_ops wireless_driver_wext_ops = {
	.wlan_vendor		= "Wireless Extent",
	.wlan_chip_id		= "",
	.wlan_usb_dongle	= 0,
	.wlan_addr		= NULL,
	.wlan_name		= "",
	.wlan_p2p_name		= "",
	.wlan_scan_delay	= 1,
	.wlan_associated	= 1,
	.wlan_linkup_time	= 0,
	.wlan_linkstatus	= 0,
	.wlan_user_data		= NULL,
	.wlan_wpa_ie_offset	= 0,
	.wlan_rsn_ie_offset	= 0,
	.wlan_scan_event	= 1,
	.wlan_skfd		= -1,
	.wlan_scan_completed_cb = NULL,
	.wlan_insert		= wireless_driver_wext_insert,
	.wlan_remove		= wireless_driver_wext_remove,
	.wlan_down		= wireless_driver_wext_down,
	.wlan_init		= wireless_driver_wext_init,
	.wlan_reset		= wireless_driver_wext_reset,
	.wlan_scan		= wireless_driver_wext_scan,
	.wlan_set_profile_wep	= wireless_driver_wext_set_profile,
	.wlan_set_profile_wpa	= wireless_driver_wext_set_profile,
	.wlan_get_status	= wireless_driver_wext_getlinkstatus,
	.wlan_get_signal_strength = wireless_driver_wext_get_signal_strength,
	.wlan_get_scan_results	= wireless_driver_wext_get_scan_results,
	.wlan_get_wpa_linkstate = wireless_driver_wext_get_wpa_linkstate,
	.wlan_linkup		= wireless_driver_wext_linkup,
	.wlan_linkdown		= wireless_driver_wext_linkdown,
	.wlan_scan_completed	= wireless_driver_wext_scan_completed,
	.wlan_set_scan_completed_cb = wireless_driver_wext_set_scan_completed_cb,
};

