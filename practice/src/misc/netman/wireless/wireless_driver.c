/* 
 * Copyright (C) 2005 - 2010, ALPHA Networks, inc.
 * Author:  redsonic
 * vim:cindent:ts=8:sw=8
 * Wills Yin: Group all wireless related functions into this file.
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <asm/types.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include <arpa/inet.h>
#include <errno.h>
#include "securesoho.h"
#include "securesoho_wireless.h"
#include "config_access.h"
#include "bloboid.h"

#ifndef	__TARGET_H__
#error	"NO Target.h"
#endif

#define MYLOG_CATEGORY_NAME "net_daemon"
#include "mylog.h"

static struct SITE_OBJ *arSiteObj[WIRELESS_SITESURVEY_CACHE_NUM];

extern struct wireless_driver_ops wireless_driver_marvell_ops;
extern struct wireless_driver_ops wireless_driver_atheros_ops;
#ifdef CONF_WIRELESS_RT3070
extern struct wireless_driver_ops wireless_driver_rt3070_ops;
#endif
#ifdef CONF_WIRELESS_RT3572
extern struct wireless_driver_ops wireless_driver_rt3572_ops;
#endif
#ifdef CONF_WIRELESS_RT5370
extern struct wireless_driver_ops wireless_driver_rt5370_ops;
#endif
extern struct wireless_driver_ops wireless_driver_dummy_ops;

struct wireless_driver_ops *securesoho_wireless_drivers[] =
{
#ifdef CONF_WIRELESS_MARVELL
	&wireless_driver_marvell_ops,
#endif
#ifdef CONF_WIRELESS_AR5413
	&wireless_driver_atheros_ops,
#endif
#ifdef CONF_WIRELESS_RT3070
	&wireless_driver_rt3070_ops,
#endif
#ifdef CONF_WIRELESS_RT3572
	&wireless_driver_rt3572_ops,
#endif
#ifdef CONF_WIRELESS_RT5370
	&wireless_driver_rt5370_ops,
#endif
	&wireless_driver_dummy_ops,

	NULL
};

int dbm2percentage(int dbm)
{
	if (dbm >= -45)
		return 100;
	else if (dbm <= -95)
		return 0;
	else
		return (95 - (-1 * dbm))*2;
}

struct SITE_OBJ **get_arSiteObj(void)
{
	return arSiteObj;
}

static int usbid_cmp(struct usb_id_s *usb_id, int cnt, struct usb_id_s *usb_id2, int cnt2)
{
	struct usb_id_s *tmp_id, *tmp_id2;
	int i, j;

	for(i = 0; i < cnt; i++) {
		tmp_id = &usb_id[i];
		for (j = 0; j < cnt2; j++) {
			tmp_id2 = &usb_id2[j];
			if (tmp_id->idVendor == tmp_id2->idVendor && tmp_id->idProduct == tmp_id2->idProduct) {
				mylog_trace("\033[1;32m%s:%d, FOUND: vid: 0x%04x, pid: 0x%04x\033[0m\n", __FUNCTION__, __LINE__, tmp_id->idVendor, tmp_id->idProduct);
				return 1;
			}
		}
	}

	return 0;
}

static int cnt_usbid(struct usb_id_s *usb_id)
{
	int i = 0;
	struct usb_id_s *tmp_id;

	tmp_id = &usb_id[0];
	while (!(tmp_id->idVendor == 0xFFFF && tmp_id->idProduct == 0xFFFF)) {
		i++;
		tmp_id = &usb_id[i];
	}

	return i;
}


struct SITE_OBJ *securesoho_site_survey(int *cnt, WIRELESS_MODE mode)
{
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	if (!wlan)
		return NULL;

	wlan->wlan_reset();//make sure the wireless interface on
	if (wlan->wlan_get_scan_results);	
		return wlan->wlan_get_scan_results(cnt, mode);
	return NULL;
}

void securesoho_free_site_obj(struct SITE_OBJ *site)
{
	struct SITE_OBJ *p;
	
	while (site != NULL){
		p = site;
		site = site->next;
		free(p);
	}
}

int invalidate_ssid(const char *ssid)
{
	int i, len;

	if (ssid == NULL)  return 1;
	len = strlen(ssid);

	if (1 > len || len > 32)  return 1;
	
	for (i = 0; i < len; ++i)
		if (!isalnum(ssid[i]))
			return 1;
	return 0;
}

int invalidate_wep_key(const char *key, int crypt_bit)
{
	int len, i;
	char c;

	len = strlen(key);

	if (crypt_bit == WIRELESS_ENCRYPT_NONE)
		return WEP_OK;

	if (crypt_bit == WIRELESS_ENCRYPT_WEP){
		if (key == NULL || (len != 5 && len != 10 &&  len != 13 && len != 26)) {
			printf("%s:%d key = %s, length = %d\n", __FUNCTION__, __LINE__, key, len);
			return WEP_INVALID_WEP;
		}
			
		if(len == 10 || len == 26){
			for (i = 0; i < len; i++){
				c = *(key + i);
				if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
					printf("%s:%d key = %s\n", __FUNCTION__, __LINE__, key);
					return WEP_INVALID_WEP;
				}
			}
		}
	}
	if (crypt_bit == WIRELESS_ENCRYPT_TKIP || crypt_bit == WIRELESS_ENCRYPT_AES) {
		if ( key == NULL || (strlen(key) > 64) || (strlen(key) < 8)){
			return WEP_INVALID_WPAPSK;
		}
		if (len == 64 ){
			for (i = 0; i < strlen(key); i++){
				c = *(key + i);
				if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))
					return WEP_INVALID_WPAPSK;
			}
		}
	}
	return WEP_OK;
}

struct wireless_driver_ops *create_wlan_drv(struct wireless_driver_ops *wlan_drv)
{
	struct wireless_driver_ops *wlan;

	if ((wlan = (struct wireless_driver_ops *)malloc(sizeof(struct wireless_driver_ops))) == NULL) {
		perror("Allocate memory for struct wireless_driver_ops!!");
		return NULL;
	}
	memcpy(wlan, wlan_drv, sizeof(struct wireless_driver_ops));

	return wlan;
}

struct wireless_driver_ops *wlan_driver_list_get(void)
{
	return NULL;
}

/* 
 * The dummy wireless driver is always marked as used
 */
int wlan_driver_init(void)
{
	int i;
	struct wireless_driver_ops *p = NULL;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->wlan_vendor && strcmp(p->wlan_vendor, "Dummy")) 
			p->used = 0;
		else
			p->used = 1;
	}
	return 0;
}


int wlan_driver_del(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node)
{
	return 0;
}


/* 
 * Clear the driver list except dummy driver
 */
int wlan_driver_clear(void)
{
	wlan_driver_init();
	wlan_driver_dump();
	return 0;
}

int wlan_driver_contains_of(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node)
{
	int i;
	struct wireless_driver_ops *p = NULL;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->wlan_chip_id && strcmp(p->wlan_chip_id, node->wlan_chip_id) == 0) {
			if (p->used) {
				mylog_info("The driver has already in the driver list");
				return 1;
			}
			break;
		}
	}

	return 0;
}

int wlan_driver_add(struct wireless_driver_ops *drv_list, struct wireless_driver_ops *node)
{
	int i;
	struct wireless_driver_ops *p = NULL;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->wlan_chip_id && strcmp(p->wlan_chip_id, node->wlan_chip_id) == 0) {
			if (p->used)
				mylog_info("The driver has already in the driver list");
			else
				p->used = 1;
			break;
		}
	}

	wlan_driver_dump();

	return 0;
}

/* 
 * Return :
 * 0 - found the matched driver
 * -1 - not found the matched driver
 * -2 - out of memory
 * */
int wlan_driver_match(unsigned short vid, unsigned short pid, void (*callback)(void *))
{
	struct wireless_driver_ops *p = NULL;
	struct usb_id_s usb_id;
	int i = 0, cnt = 0;
	int ret = -1;

	usb_id.idVendor = vid;
	usb_id.idProduct = pid;

	mylog_trace("The match patten is 0x%04x:0x%04x", vid, pid);
	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		cnt = cnt_usbid(p->wlan_addr);
		if (usbid_cmp(&usb_id, 1, p->wlan_addr, cnt)) {
			mylog_trace("%s Found a matched wireless driver", __func__);
			if (callback)
				callback((void *)p);
			ret = 0;
		}
	}
	mylog_trace("%s done", __func__);

	if ((p = securesoho_get_wireless_card()) && p->wlan_name)
		securesoho_string_set(WIRELESS_NIC_NAME, p->wlan_name);
	if ((p = securesoho_get_wireless_card()) && p->wlan_p2p_name) {
		securesoho_string_set(WIRELESS_NIC_P2P_NAME, p->wlan_p2p_name);
	}
	return ret;
}

/* 
 * check the driver list except dummy driver
 */
struct wireless_driver_ops * wlan_driver_check(void)
{
	int i;
	struct wireless_driver_ops *p;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->used && strcmp(p->wlan_vendor, "Dummy") != 0) 
			return p;
	}

	return NULL;
}


int wlan_driver_dump(void)
{
	int i;
	struct wireless_driver_ops *p;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->used)
			mylog_debug("imVendor:[%s], ID:[%s], Addr:[%p], ifname:[%s], p2pname:[%s]", 
				p->wlan_vendor, 
				p->wlan_chip_id, 
				p->wlan_addr, 
				p->wlan_name,
				p->wlan_p2p_name);
	}

	return 0;
}
/* 
 * Return:
 *   0 - found the pci card
 *  -1 - not fond the pci card
 *  -2 - failed to open file
 * */

int wireless_card_detect_pci(void)
{
	return 0;
}

struct wireless_driver_ops *securesoho_get_wireless_card(void)
{
	int i;
	struct wireless_driver_ops * p;

	for (i = 0; securesoho_wireless_drivers[i]; i++) {
		p = securesoho_wireless_drivers[i];
		if (p->used)
			return p;
	}

	return NULL;
}

int securesoho_get_wireless_signal(void)
{
	int signal=-1;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	if (wlan && wlan->wlan_get_signal_strength){
		wlan->wlan_get_signal_strength(&signal);
	}
	return signal;
}

int securesoho_is_wireless_available(void)
{
	return securesoho_is_wireless_card_dummy()?0:1;
}

int securesoho_is_wireless_card_dummy(void)
{
	struct wireless_driver_ops *ops = NULL;
	ops = securesoho_get_wireless_card();
	printf("\033[1;42m debug at %s,%s,%d ops->wlan_vendor:%s\033[0m\n",__FILE__,__FUNCTION__,__LINE__, ops->wlan_vendor);
	if (ops && strcmp(ops->wlan_vendor, "Dummy") == 0){
		printf("The wireless card is invalid.\n");	
		return 1;
	}
	else 
		return 0;
}

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

#define ETHER_ADDR_LEN		6	/* length of an Ethernet address */
/*
 * Structure of a 48-bit Ethernet address.
 */
struct  ether_addr {
	u_char octet[ETHER_ADDR_LEN];
} __packed;

/*------------------------------------------------------------------*/
/*
 * Display an Ethernet address in readable format.
 */
void
iw_ether_ntop(const struct ether_addr* eth, char* buf)
{
  sprintf(buf, "%02X:%02X:%02X:%02X:%02X:%02X",
	  eth->octet[0], eth->octet[1],
	  eth->octet[2], eth->octet[3],
	  eth->octet[4], eth->octet[5]);
}

/*------------------------------------------------------------------*/
/* Backwards compatability
 * Actually, those form are much easier to use when dealing with
 * struct sockaddr... */
static inline char*
iw_pr_ether(char* bufp, const unsigned char* addr)
{
	iw_ether_ntop((const struct ether_addr *) addr, bufp);
	return bufp;
}

/************************* MISC SUBROUTINES **************************/

/* Size (in bytes) of various events */
static const int priv_type_size[] = {
	0,				/* IW_PRIV_TYPE_NONE */
	1,				/* IW_PRIV_TYPE_BYTE */
	1,				/* IW_PRIV_TYPE_CHAR */
	0,				/* Not defined */
	sizeof(__u32),			/* IW_PRIV_TYPE_INT */
	sizeof(struct iw_freq),		/* IW_PRIV_TYPE_FLOAT */
	sizeof(struct sockaddr),	/* IW_PRIV_TYPE_ADDR */
	0,				/* Not defined */
};

/*------------------------------------------------------------------*/
/*
 * Max size in bytes of an private argument.
 */
int
iw_get_priv_size(int	args)
{
	int	num = args & IW_PRIV_SIZE_MASK;
	int	type = (args & IW_PRIV_TYPE_MASK) >> 12;

	return(num * priv_type_size[type]);
}

/*------------------------------------------------------------------*/
/*
 * Convert our internal representation of frequencies to a floating point.
 */
double
iw_freq2float(struct iw_freq *	in)
{
	/* Version without libm : slower */
	int		i;
	double	res = (double) in->m;

	for(i = 0; i < in->e; i++)
		res *= 10;

	return(res);
}

/*------------------------------------------------------------------*/
/*
 * Get information about what private ioctls are supported by the driver
 */
int
iw_get_priv_info(int skfd, char *ifname, struct iw_priv_args * priv, int maxpriv)
{
	struct iwreq wrq;

	/* Ask the driver */
	wrq.u.data.pointer = (caddr_t) priv;
	wrq.u.data.length = maxpriv;
	wrq.u.data.flags = 0;
	strncpy(wrq.ifr_name, ifname, IFNAMSIZ);
	/* Do the request */
	if (ioctl(skfd, SIOCGIWPRIV, &wrq) < 0)
		return (-1);

	/* Return the number of ioctls */
	return (wrq.u.data.length);
}

/*------------------------------------------------------------------*/
/*
 * Execute a private command on the interface
 */
int set_private(int skfd,	/* Socket */
	      char *ifname,
	      char *cmdname,
	      void **ret,
	      int count,
	      void *args[])
{				/* Dev name */
	struct iwreq	wrq;
	unsigned char	buffer[4096];	/* Only that big in v25 and later */
	struct	iw_priv_args priv[IW_MAX_PRIV_DEF];
	int	priv_num;		/* Max of private ioctl */
	int	subcmd = 0;	/* sub-ioctl index */
	int	offset = 0;	/* Space for sub-ioctl index */
	int	k, i = 0;
	unsigned char	*data = NULL;

	/* Read the private ioctls */
	priv_num = iw_get_priv_info(skfd, ifname, priv, IW_MAX_PRIV_DEF);

	/* Is there any ? */
	if (priv_num <= 0) {
		/* Could skip this message ? */
		fprintf(stderr, "%-8.8s  no private ioctls.\n\n", ifname);
		return (-1);
	}

	/* Search the correct ioctl */
	k = -1;
	while((++k < priv_num) && strcmp(priv[k].name, cmdname));

	/* If not found... */
	if(k == priv_num)
	{
		printf("Invalid command : %s\n", cmdname);
		return(-1);
	}
	  
	/* Watch out for sub-ioctls ! */
	if(priv[k].cmd < SIOCDEVPRIVATE) {
		int	j = -1;

		/* Find the matching *real* ioctl */
		while((++j < priv_num) && ((priv[j].name[0] != '\0') ||
				(priv[j].set_args != priv[k].set_args) ||
				(priv[j].get_args != priv[k].get_args)));

		/* If not found... */
		if(j == priv_num) {
			fprintf(stderr, "Invalid private ioctl definition for : %s\n",
				cmdname);
			return(-1);
		}

		/* Save sub-ioctl number */
		subcmd = priv[k].cmd;
		/* Reserve one int (simplify alignement issues) */
		offset = sizeof(__u32);
		/* Use real ioctl definition from now on */
		k = j;
	}
	
	/* If we have to set some data */
	if((priv[k].set_args & IW_PRIV_TYPE_MASK) &&
			(priv[k].set_args & IW_PRIV_SIZE_MASK)){
		switch(priv[k].set_args & IW_PRIV_TYPE_MASK){
#if 0
		case IW_PRIV_TYPE_BYTE:
			/* Number of args to fetch */
			wrq.u.data.length = 1;
			if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
				wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

			/* Fetch args */
			for(; i < wrq.u.data.length; i++) {
				sscanf(args[i], "%i", &temp);
				buffer[i] = (char) temp;
			}
			break;
#endif

		case IW_PRIV_TYPE_INT:
			/* Number of args to fetch */
			wrq.u.data.length = 1;
			if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
				wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

			/* Fetch args */
			for(; i < wrq.u.data.length; i++) {
				//printf("(%s:%d) args[%d]=%d, wrq.u.data.length=%d\n", __FUNCTION__, __LINE__, i, (int)args[i], wrq.u.data.length);
				((__s32 *) buffer)[i] = (__s32) args[i];
			}
			break;

		case IW_PRIV_TYPE_CHAR:
			if(i < count){
				/* Size of the string to fetch */
				wrq.u.data.length = strlen(args[i]) + 1;
				if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
					wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

				/* Fetch string */
				//printf("(%s:%d) args[%d]=%s, wrq.u.data.length=%d\n", __FUNCTION__, __LINE__, i, (char *)args[i], wrq.u.data.length);
				memcpy(buffer, args[i], wrq.u.data.length);
				buffer[sizeof(buffer) - 1] = '\0';
				//printf("(%s:%d) buffer=%s\n", __FUNCTION__, __LINE__, buffer);
				i++;
			} else {
				wrq.u.data.length = 1;
				buffer[0] = '\0';
			}
			break;

#if 0
		case IW_PRIV_TYPE_FLOAT:
			/* Number of args to fetch */
			wrq.u.data.length = 1;
			if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
				wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

			/* Fetch args */
			for(; i < wrq.u.data.length; i++) {
				double		freq;

				if(sscanf(args[i], "%lg", &(freq)) != 1){
					printf("Invalid float [%s]...\n", args[i]);
					return(-1);
				}    
				if(index(args[i], 'G')) freq *= GIGA;
				if(index(args[i], 'M')) freq *= MEGA;
				if(index(args[i], 'k')) freq *= KILO;
				sscanf(args[i], "%i", &temp);
				iw_float2freq(freq, ((struct iw_freq *) buffer) + i);
			}
			break;
#endif

#if 0
		case IW_PRIV_TYPE_ADDR:
			/* Number of args to fetch */
			wrq.u.data.length = 1;
			if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
				wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

			/* Fetch args */
			for(; i < wrq.u.data.length; i++) {
				if(iw_in_addr(skfd, ifname, args[i],
						((struct sockaddr *) buffer) + i) < 0){
					printf("Invalid address [%s]...\n", args[i]);
					return(-1);
				}
			}
			break;
#endif

		default:
			printf("%s:%d ERROR()\n", __FUNCTION__, __LINE__);
			fprintf(stderr, "Not yet implemented...\n");
			return(-1);
		}
	  
		if((priv[k].set_args & IW_PRIV_SIZE_FIXED) &&
				(wrq.u.data.length != (priv[k].set_args & IW_PRIV_SIZE_MASK))){
			printf("The command %s need exactly %d argument...\n",
					cmdname, priv[k].set_args & IW_PRIV_SIZE_MASK);
			return(-1);
		}
	}	/* if args to set */
	else
	{
		wrq.u.data.length = 0L;
	}

	strncpy(wrq.ifr_name, ifname, IFNAMSIZ);

	/* Those two tests are important. They define how the driver
	 * will have to handle the data */
	if((priv[k].set_args & IW_PRIV_SIZE_FIXED) &&
			((iw_get_priv_size(priv[k].set_args) + offset) <= IFNAMSIZ)){
		/* First case : all SET args fit within wrq */
		if(offset)
			wrq.u.mode = subcmd;
		memcpy(wrq.u.name + offset, buffer, IFNAMSIZ - offset);
	} else {
		if((priv[k].set_args == 0) &&
				(priv[k].get_args & IW_PRIV_SIZE_FIXED) &&
				(iw_get_priv_size(priv[k].get_args) <= IFNAMSIZ)){
			/* Second case : no SET args, GET args fit within wrq */
			if(offset)
				wrq.u.mode = subcmd;
		} else {
			/* Thirst case : args won't fit in wrq, or variable number of args */
			wrq.u.data.pointer = (caddr_t) buffer;
			wrq.u.data.flags = subcmd;
		}
	}

	/* Perform the private ioctl */
	if(ioctl(skfd, priv[k].cmd, &wrq) < 0) {
		fprintf(stderr, "Interface doesn't accept private ioctl...\n");
		fprintf(stderr, "%s (%X): %s\n", cmdname, priv[k].cmd, strerror(errno));
		return(-1);
	}

	/* If we have to get some data */
	if((priv[k].get_args & IW_PRIV_TYPE_MASK) &&
			(priv[k].get_args & IW_PRIV_SIZE_MASK)) {
		int	j;
		int	n = 0;		/* number of args */

		printf("%-8.8s  %s:\n", ifname, cmdname);

		/* Check where is the returned data */
		if((priv[k].get_args & IW_PRIV_SIZE_FIXED) &&
				(iw_get_priv_size(priv[k].get_args) <= IFNAMSIZ)) {
			memcpy(buffer, wrq.u.name, IFNAMSIZ);
			n = priv[k].get_args & IW_PRIV_SIZE_MASK;
		} else {
			n = wrq.u.data.length;
		}

		switch(priv[k].get_args & IW_PRIV_TYPE_MASK) {
#if 0
		case IW_PRIV_TYPE_BYTE:
			printf("(%s:%d)\n", __FUNCTION__, __LINE__);
			/* Display args */
			for(j = 0; j < n; j++){
				printf("%d  ", buffer[j]);
			}
			printf("\n");
	  		break;
#endif

		case IW_PRIV_TYPE_INT:
			/* Display args */
			for(j = 0; j < n; j++){
				*ret = (void *)((__s32 *) buffer)[j];
			}
			break;

		case IW_PRIV_TYPE_CHAR:
			/* Display args */
			buffer[wrq.u.data.length - 1] = '\0';
			data = malloc(wrq.u.data.length);
			strncpy((char *)data, (char *)buffer, wrq.u.data.length);
			data[wrq.u.data.length - 1] = '\0';
			*ret = data;
			
			break;

#if 0
		case IW_PRIV_TYPE_FLOAT:
		{
			double		freq;
			printf("(%s:%d)\n", __FUNCTION__, __LINE__);
			/* Display args */
			for(j = 0; j < n; j++) {
				freq = iw_freq2float(((struct iw_freq *) buffer) + j);
				if(freq >= GIGA){
					printf("%gG  ", freq / GIGA);
				} else if(freq >= MEGA) {
					printf("%gM  ", freq / MEGA);
				} else {
					printf("%gk  ", freq / KILO);
				}
			}
			printf("\n");
		}
		break;

		case IW_PRIV_TYPE_ADDR:
		{
			char		scratch[160];
			struct sockaddr *	hwa;
			printf("(%s:%d)\n", __FUNCTION__, __LINE__);
			/* Display args */
			for(j = 0; j < n; j++) {
				hwa = ((struct sockaddr *) buffer) + j;
				if(j) {
					printf("           %.*s", 
					(int) strlen(cmdname), "                ");
					printf("%s\n", iw_pr_ether(scratch, hwa->sa_data));
				}
			}
		}
		break;
#endif

		default:
			fprintf(stderr, "Not yet implemented...\n");
			return(-1);
		}
	}	/* if args to set */

	return 0;
}

/*
 * Open a socket.
 * Depending on the protocol present, open the right socket. The socket
 * will allow us to talk to the driver.
 */
int
iw_sockets_open(void)
{
  static const int families[] = {
    AF_INET, AF_IPX, AF_AX25, AF_APPLETALK
  };
  unsigned int	i;
  int		sock;

  /*
   * Now pick any (exisiting) useful socket family for generic queries
   * Note : don't open all the socket, only returns when one matches,
   * all protocols might not be valid.
   * Workaround by Jim Kaba <jkaba@sarnoff.com>
   * Note : in 99% of the case, we will just open the inet_sock.
   * The remaining 1% case are not fully correct...
   */

  /* Try all families we support */
  for(i = 0; i < sizeof(families)/sizeof(int); ++i)
    {
      /* Try to open the socket, if success returns it */
      sock = socket(families[i], SOCK_DGRAM, 0);
      if(sock >= 0)
	return sock;
  }

  return -1;
}

