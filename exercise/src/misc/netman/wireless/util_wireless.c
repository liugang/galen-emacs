#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/select.h>
#include <net/if_arp.h>
#include <asm/types.h>
#include <sys/socket.h>
#include <linux/if_ether.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include "securesoho.h"
#include "util_wireless.h"

void rtnl_close(struct rtnl_handle *rth)
{
	close(rth->fd);
}

int rtnl_open(struct rtnl_handle *rth, unsigned subscriptions)
{
	int addr_len;

	memset(rth, 0, sizeof(rth));

	rth->fd = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
	if (rth->fd < 0) {
		perror("Cannot open netlink socket");
		return -1;
	}

	memset(&rth->local, 0, sizeof(rth->local));
	rth->local.nl_family = AF_NETLINK;
	rth->local.nl_groups = subscriptions;

	if (bind(rth->fd, (struct sockaddr*)&rth->local, sizeof(rth->local)) < 0) {
		perror("Cannot bind netlink socket");
		return -1;
	}
	addr_len = sizeof(rth->local);
	if (getsockname(rth->fd, (struct sockaddr*)&rth->local, (socklen_t *)&addr_len) < 0) {
		perror("Cannot getsockname");
		return -1;
	}
	if (addr_len != sizeof(rth->local)) {
		printf("Wrong address length %d\n", addr_len);
		return -1;
	}
	if (rth->local.nl_family != AF_NETLINK) {
		printf("Wrong address family %d\n", rth->local.nl_family);
		return -1;
	}
	rth->seq = time(NULL);
	return 0;
}

static void wevent_wireless(char *data, int len)
{
	struct iw_event iwe_buf, *iwe = &iwe_buf;
	char *pos, *end;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();

	pos = data;
	end = data + len;

	while (pos + IW_EV_LCP_LEN <= end) {
		/* Event data may be unaligned, so make a local, aligned copy
		 * before processing. */
		memcpy(&iwe_buf, pos, sizeof(struct iw_event));
		if (iwe->len <= IW_EV_LCP_LEN) {
			return;
		}
		switch (iwe->cmd) {
		case SIOCGIWAP:
			if (memcmp(iwe->u.ap_addr.sa_data, "\x00\x00\x00\x00\x00\x00", ETH_ALEN) == 0
			    ||
			    memcmp(iwe->u.ap_addr.sa_data, "\x44\x44\x44\x44\x44\x44", ETH_ALEN) == 0) {
				// Link down
				wlan->wlan_linkdown();
			} else {
				// Link up
				wlan->wlan_linkup();
			}
			break;
		case IWEVREGISTERED:
			wlan->wlan_linkup();
			break;
		case IWEVEXPIRED:
			wlan->wlan_linkdown();
			break;
		case SIOCGIWSCAN:
			wlan->wlan_scan_completed();
			break;
		default:
			break;
		}

		pos += iwe->len;
	}
}

void handle_netlink_events(struct rtnl_handle *rth)
{
	char buf[8192];
	int left;
	struct sockaddr_nl from;
	socklen_t fromlen;
	struct nlmsghdr *h;
	struct ifinfomsg *ifi;
	int attrlen, nlmsg_len, rta_len;
	struct rtattr * attr;

	fromlen = sizeof(from);
	left = recvfrom(rth->fd, buf, sizeof(buf), MSG_DONTWAIT,
			(struct sockaddr *) &from, &fromlen);
	if (left < 0) {
		if (errno != EINTR && errno != EAGAIN)
			perror("recvfrom(netlink)");
		return;
	}

	h = (struct nlmsghdr *) buf;
	while (left >= sizeof(*h)) {
		int len, plen;

		len = h->nlmsg_len;
		plen = len - sizeof(*h);
		if (len > left || plen < 0) {
			printf("Malformed netlink message: len=%d left=%d plen=%d\n", len, left, plen);
			break;
		}

		switch (h->nlmsg_type) {
		case RTM_NEWLINK:
			if (plen < sizeof(*ifi))
				return;

			ifi = NLMSG_DATA(h);

			/* TODO: use ifi->ifi_index to recognize the interface (?) */

			nlmsg_len = NLMSG_ALIGN(sizeof(struct ifinfomsg));

			attrlen = h->nlmsg_len - nlmsg_len;
			if (attrlen < 0)
				return;

			attr = (struct rtattr *) (((char *) ifi) + nlmsg_len);

			rta_len = RTA_ALIGN(sizeof(struct rtattr));
			while (RTA_OK(attr, attrlen)) {
				if (attr->rta_type == IFLA_WIRELESS) {
					wevent_wireless(((char *) attr) + rta_len, attr->rta_len - rta_len);
				} else if (attr->rta_type == IFLA_IFNAME) {
				}
				attr = RTA_NEXT(attr, attrlen);
			}
			break;
		}

		len = NLMSG_ALIGN(len);
		left -= len;
		h = (struct nlmsghdr *) ((char *) h + plen);
	}

	if (left > 0) {
		printf("%d extra bytes in the end of netlink message\n", left);
	}
}

int ctoi(char value)
{
	if (value > '9' && value > 'F')
		return value - 87;
	else if (value > '9')
		return value - 55;
	else
		return value - '0';
}

/*
 * key_type:
 *	Multicasts key = 0
 *	Unicasts key = 1
 */
int parse_wpa_oui(unsigned char *oui, int key_type, unsigned int *encrypt, char *ret)
{
#if 0
	if (*oui == 0x0 && *(oui+1) == 0x50 && *(oui+2) == 0xf2 && *(oui+3) == 0x1)
	{
		strcpy(ret, "WEP-40");
		return 0;
	}
	if (*oui == 0x0 && *(oui+1) == 0x50 && *(oui+2) == 0xf2 && *(oui+3) == 0x2)
	{
		switch (key_type)
		{
			case WIRELESS_ENCRYPT_MCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA_PSK_MCAST_TKIP;
				break;
			case WIRELESS_ENCRYPT_UCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA_PSK_UCAST_TKIP;
				break;
		}
		strcpy(ret, "TKIP");
		return 0;
	}
	if (*oui == 0x0 && *(oui+1) == 0x50 && *(oui+2) == 0xf2 && *(oui+3) == 0x4)
	{
		switch (key_type)
		{
			case WIRELESS_ENCRYPT_MCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA_PSK_MCAST_CCMP;
				break;
			case WIRELESS_ENCRYPT_UCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA_PSK_UCAST_CCMP;
				break;
		}
		strcpy(ret, "CCMP");
		return 0;
	}
	if (*oui == 0x0 && *(oui+1) == 0x50 && *(oui+2) == 0xf2 && *(oui+3) == 0x5)
	{
		strcpy(ret, "WEP-104");
		return 0;
	}
#endif
	strcpy(ret, "UnKnown");
	return -1;
}

void parse_wpa_ie(unsigned char *ie, int *ret)
{
#if 0
	int cnt, i, len;
	char ie_data[128], buf[16];
	char *data;
	unsigned int encrypt = 0;

	len = (*(ie+2) - '0') * 16 + ( *(ie+3) - '0' ) + 2;
	for (i = 0; i < len * 2; i = i + 2) {
		ie_data[i/2] = ctoi(*(ie + i)) * 16;
		ie_data[i/2] += ctoi(*(ie + i + 1));
	}
	data = ie_data;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    WPA Information\n");
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Element ID:\t\t\t%d\n", *data);
	data += 1;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Length:\t\t\t\t%d\n", *data);
	data += 1;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	OUI:\t\t\t\t0x%02X-0x%02X-0x%02X-0x%02X\n", *data, *(data+1), *(data+2), *(data+3));
	data += 4;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Version:\t\t\t%d\n", *data);
	data += 2;
	parse_wpa_oui((unsigned char *)data, WIRELESS_ENCRYPT_MCAST_KEY, &encrypt, buf);
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Multicast cipher OUI:\t\t0x%02X-0x%02X-0x%02X-%02X %s\n", *data, *(data+1), *(data+2), *(data+3), buf);
	data += 4;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Number of Unicasts:\t\t%d\n", *data);
	cnt = *data;
	data += 2;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Unicasts Cipher List\t\t\n");
	for (i = 0; i < cnt; i++) {
		parse_wpa_oui((unsigned char *)data, WIRELESS_ENCRYPT_UCAST_KEY, &encrypt, buf);
		PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    		Unicasts cipher OUI:\t0x%02X-0x%02X-0x%02X-%02X %s\n", *data, *(data+1), *(data+2), *(data+3), buf);
		data += 4;
	}
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Number of Auths:\t\t%d\n", *data);
	cnt = *data;
	data += 2;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	AuthKey Mngmnt Suite List\n");
	for (i = 0; i < cnt; i++) {
		PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    		Auth OUI:\t\t0x%02X-0x%02X-0x%02X-%02X\n", *data, *(data+1), *(data+2), *(data+3));
		data += 4;
	}
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "\n");
	*ret |= encrypt | WIRELESS_ENCRYPT_WPA_PSK;
#endif
}

/*
 * key_type:
 *	Multicasts key = 0
 *	Unicasts key = 1
 */
void parse_rsn_oui(unsigned char *oui, int key_type, unsigned int *encrypt, char *ret)
{
#if 0
	if (*oui == 0x0 && *(oui+1) == 0x0f && *(oui+2) == 0xac && *(oui+3) == 0x1)
	{
		strcpy(ret, "WEP-40");
		return;
	}
	if (*oui == 0x0 && *(oui+1) == 0x0f && *(oui+2) == 0xac && *(oui+3) == 0x2)
	{
		switch (key_type)
		{
			case WIRELESS_ENCRYPT_MCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA2_PSK_MCAST_TKIP;
				break;
			case WIRELESS_ENCRYPT_UCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA2_PSK_UCAST_TKIP;
				break;
		}
		strcpy(ret, "TKIP");
		return;
	}
	if (*oui == 0x0 && *(oui+1) == 0x0f && *(oui+2) == 0xac && *(oui+3) == 0x4)
	{
		switch (key_type)
		{
			case WIRELESS_ENCRYPT_MCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA2_PSK_MCAST_CCMP;
				break;
			case WIRELESS_ENCRYPT_UCAST_KEY:
				*encrypt |= WIRELESS_ENCRYPT_WPA2_PSK_UCAST_CCMP;
				break;
		}
		strcpy(ret, "CCMP");
		return;
	}
	if (*oui == 0x0 && *(oui+1) == 0x0f && *(oui+2) == 0xac && *(oui+3) == 0x5)
	{
		strcpy(ret, "WEP-104");
		return;
	}
#endif
	strcpy(ret, "UnKnown");
	return;
}

void parse_rsn_ie(unsigned char *ie, int *ret)
{
#if 0
	int cnt, i, len;
	char ie_data[128], buf[16];
	char *data;
	unsigned int encrypt = 0;

	len = (*(ie+2) - '0') * 16 + ( *(ie+3) - '0' ) + 2;
	for (i = 0; i < len * 2; i = i + 2) {
		ie_data[i/2] = ctoi(*(ie + i)) * 16;
		ie_data[i/2] += ctoi(*(ie + i + 1));
	}
	data = ie_data;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    RSN Information\n");
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Element ID:\t\t\t%d\n", *data);
	data += 1;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Length:\t\t\t\t%d\n", *data);
	data += 1;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Version:\t\t\t%d\n", *data);
	data += 2;
	parse_rsn_oui((unsigned char *)data, WIRELESS_ENCRYPT_MCAST_KEY, &encrypt, buf);
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Group Key Cipher Suite:\t\t0x%02X-0x%02X-0x%02X-%02X %s\n", *data, *(data+1), *(data+2), *(data+3), buf);
	data += 4;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Parewise Key Count:\t\t%d\n", *data);
	cnt = *data;
	data += 2;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	Parewise Key Cipher List\n");
	for (i = 0; i < cnt; i++) {
		parse_rsn_oui((unsigned char *)data, WIRELESS_ENCRYPT_UCAST_KEY, &encrypt, buf);
		PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "				Parewise Key cipher:\t0x%02X-0x%02X-0x%02X-%02X %s\n", *data, *(data+1), *(data+2), *(data+3), buf);
		data += 4;
	}
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	AuthKey Mngmnt Count:\t\t%d\n", *data);
	cnt = *data;
	data += 2;
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	AuthKey Mngmnt Suite List\n");
	for (i = 0; i < cnt; i++) {
		PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "				AuthKey Mngmnt:\t\t0x%02X-0x%02X-0x%02X-%02X\n", *data, *(data+1), *(data+2), *(data+3));
		data += 4;
	}
	PRINTDEBUGMSG(MSG_SEURESOHO_WIRELESS, "                    	RSN Capabilities: 		0x%02X%02X\n\n", *data, *(data+1));
	*ret |= encrypt | WIRELESS_ENCRYPT_WPA2_PSK;
#endif
}

void set_crypt_type(int *crypt, int wpa)
{
#if 0
	if ((wpa == WIRELESS_ENCRYPT_NONE) && *crypt == 1) {
		*crypt = WIRELESS_ENCRYPT_WEP;
	} else if ((wpa & WIRELESS_ENCRYPT_WPA2_PSK) &&
			((wpa & WIRELESS_ENCRYPT_WPA2_PSK_MCAST_CCMP) ||
			(wpa & WIRELESS_ENCRYPT_WPA2_PSK_MCAST_TKIP)) &&
			(wpa & WIRELESS_ENCRYPT_WPA2_PSK_UCAST_CCMP)) {
		*crypt = WIRELESS_ENCRYPT_WPA2PSK_AES;
	} else if ((wpa & WIRELESS_ENCRYPT_WPA2_PSK) &&
			(wpa & WIRELESS_ENCRYPT_WPA2_PSK_MCAST_TKIP) &&
			(wpa & WIRELESS_ENCRYPT_WPA2_PSK_UCAST_TKIP)) {
		*crypt = WIRELESS_ENCRYPT_WPA2PSK_TKIP;
	} else if ((wpa & WIRELESS_ENCRYPT_WPA_PSK) &&
			((wpa & WIRELESS_ENCRYPT_WPA_PSK_MCAST_CCMP) ||
			(wpa & WIRELESS_ENCRYPT_WPA_PSK_MCAST_TKIP)) &&
			(wpa & WIRELESS_ENCRYPT_WPA_PSK_UCAST_CCMP)) {
		*crypt = WIRELESS_ENCRYPT_WPAPSK_AES;
	} else if ((wpa & WIRELESS_ENCRYPT_WPA_PSK) &&
			(wpa & WIRELESS_ENCRYPT_WPA_PSK_MCAST_TKIP) &&
			(wpa & WIRELESS_ENCRYPT_WPA_PSK_UCAST_TKIP)) {
		*crypt = WIRELESS_ENCRYPT_WPAPSK_TKIP;
	} else {
		*crypt = WIRELESS_ENCRYPT_NONE;
	}
#endif
}



