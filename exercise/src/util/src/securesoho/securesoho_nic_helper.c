/* Copyright (C) 2005, Alphanetworks, inc.
 * Author:	redsonic
 * $Header: /data/cvsroot/DMA/util/src/securesoho/securesoho_nic_helper.c,v 1.1.2.13 2007-07-06 06:19:47 ken Exp $
 * vim:cindent:ts=8:sw=8
 * Wills Yin: Group all functions related to network into this file.
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
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <asm/types.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include <errno.h>
#include <time.h>
#include <semaphore.h>
#include <linux/sockios.h>

#include "securesoho.h"

int securesoho_nic_get_ifc_hwaddr(const char* ifname,int namelen,char* hwaddr)
{
	struct ifreq ifr;
	int s;
	int ok = 0;

	s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s==-1) {
		return -1;
	}

	strcpy(ifr.ifr_name, ifname);
	if (ioctl(s, SIOCGIFHWADDR, &ifr) == 0)
		ok = 1;
	else
		printf("ioctl failed in function:%s\n",__FUNCTION__);
	close(s);
	if (ok)
		memcpy(hwaddr,ifr.ifr_hwaddr.sa_data, 6);
	else
		return -1;
	return 0;
}

int securesoho_nic_wait_ifc_up(const char *devname, int timeout)
{
	int up = 0, LocalSock;
	struct ifreq ifReq;

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		printf("FILE:%s,LINE:%d.Can't do that\r\n",__FILE__,__LINE__);
		return 0;
	}

	do {
		strcpy (ifReq.ifr_name, devname);

		if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
			printf("Can't get flags\r\n");
			close(LocalSock);
			exit(1);
		}

		if ((ifReq.ifr_flags & IFF_UP)){
			up = 1;
			break;
		}			

		if (!timeout)  
			break;
		--timeout;
		sleep(1);
	} while (timeout);

	close(LocalSock);
	return up;
}

int securesoho_nic_wait_ifc_ready(const char *devname, int timeout)
{
	char szBuffer[16*sizeof(struct ifreq)];
	struct ifconf ifConf;
	struct ifreq ifReq;
	int nResult;
	int LocalSock;
	struct sockaddr_in LocalAddr;
	int i;

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		printf("FILE:%s,LINE:%d.Can't do that\r\n",__FILE__,__LINE__);
		return 0;
	}
	ifConf.ifc_len = sizeof szBuffer;
	ifConf.ifc_ifcu.ifcu_buf = (caddr_t)szBuffer;
	nResult = ioctl(LocalSock, SIOCGIFCONF, &ifConf);
	if (nResult < 0) {
		printf("FILE:%s,LINE:%d.ioctl error\r\n",__FILE__,__LINE__);
		exit(1);
	}
	for (;;) {
		for (i = 0;(i < ifConf.ifc_len);) {
			struct ifreq *pifReq = (struct ifreq *)((caddr_t)ifConf.ifc_req + i);
			i += sizeof *pifReq;
			strcpy (ifReq.ifr_name, pifReq -> ifr_name);
			if (strcmp (devname, pifReq -> ifr_name)==0) {
				if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0) {
					printf("FILE:%s,LINE:%d.Can't get flags\r\n",__FILE__,__LINE__);
					exit(1);
				}
				if (!(ifReq.ifr_flags & IFF_UP)) {
					break;
				}
				if (pifReq -> ifr_addr.sa_family == AF_INET) {
					memcpy (&LocalAddr, &pifReq -> ifr_addr, sizeof pifReq -> ifr_addr);
					if (LocalAddr.sin_addr.s_addr != 0) {
						close(LocalSock);
						return 1;
					}
				}
			}
		}
		if (!timeout)  break;
		--timeout;
		sleep(1);
	}
	close(LocalSock);
	return 0;
}

int securesoho_nic_wait_host_ready(int timeout)
{
	char dev[64]={0};

	securesoho_lif_get(dev);
	if(dev[0] == '\0')
		return 0;
	return securesoho_nic_wait_ifc_ready(dev, timeout);
}

typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;
/* This should work for both 32 and 64 bit userland. */
struct ethtool_cmd {
	u32	cmd;
	u32	supported;	/* Features this interface supports */
	u32	advertising;	/* Features this interface advertises */
	u16	speed;		/* The forced speed, 10Mb, 100Mb, gigabit */
	u8	duplex;		/* Duplex, half or full */
	u8	port;		/* Which connector port */
	u8	phy_address;
	u8	transceiver;	/* Which tranceiver to use */
	u8	autoneg;	/* Enable or disable autonegotiation */
	u32	maxtxpkt;	/* Tx pkts before generating tx int */
	u32	maxrxpkt;	/* Rx pkts before generating rx int */
	u32	reserved[4];
};

/* for passing single values */
struct ethtool_value {
	u32	cmd;
	u32	data;
};
/* This structure is used in all SIOCxMIIxxx ioctl calls */
struct mii_ioctl_data {
	u16		phy_id;
	u16		reg_num;
	u16		val_in;
	u16		val_out;
};

#define ETHTOOL_GLINK		0x0000000a /* Get link status */
#define MII_BMCR		0x00	   /* Basic mode control register */
#define MII_BMSR		0x01	   /* Basic mode status register  */
#define MII_PHYSID1		0x02	   /* PHYS ID 1			  */
#define MII_PHYSID2		0x03	   /* PHYS ID 2			  */
#define MII_ADVERTISE		0x04	   /* Advertisement control reg	  */
#define MII_LPA			0x05	   /* Link partner ability reg	  */

#define BMCR_ANENABLE  0x1000
#define BMCR_SPEED100  0x2000

#define MII_BMCR_AN_ENA		   0x1000
#define MII_BMCR_100MBIT	   0x2000
#define MII_BMSR_LSTATUS	   0x0004     /* Link status		     */
#define MII_BMSR_AN_COMPLETE	   0x0020
#define MII_AN_100BASET4	   0x0200
#define MII_AN_100BASETX_FD	   0x0100
#define MII_AN_100BASETX_HD	   0x0080
#define MII_AN_10BASET_FD	   0x0040
#define MII_AN_10BASET_HD	   0x0020

/* Link partner ability register. */
#define LPA_SLCT		0x001f	/* Same as advertise selector  */
#define LPA_10HALF		0x0020	/* Can do 10mbps half-duplex   */
#define LPA_10FULL		0x0040	/* Can do 10mbps full-duplex   */
#define LPA_100HALF		0x0080	/* Can do 100mbps half-duplex  */
#define LPA_100FULL		0x0100	/* Can do 100mbps full-duplex  */
#define LPA_100BASE4		0x0200	/* Can do 100mbps 4k packets   */
#define LPA_RESV		0x1c00	/* Unused...		       */
#define LPA_RFAULT		0x2000	/* Link partner faulted	       */
#define LPA_LPACK		0x4000	/* Link partner acked us       */
#define LPA_NPAGE		0x8000	/* Next page bit	       */

#define LPA_DUPLEX		(LPA_10FULL | LPA_100FULL)
#define LPA_100			(LPA_100FULL | LPA_100HALF | LPA_100BASE4)

static unsigned int mii_nway_result (unsigned int negotiated)
{
	unsigned int ret;

	if (negotiated & LPA_100FULL)
		ret = LPA_100FULL;
	else if (negotiated & LPA_100BASE4)
		ret = LPA_100BASE4;
	else if (negotiated & LPA_100HALF)
		ret = LPA_100HALF;
	else if (negotiated & LPA_10FULL)
		ret = LPA_10FULL;
	else
		ret = LPA_10HALF;

	return ret;
}

int securesoho_nic_check_ifc_link(const char* ifname,int namelen)
{ 
	int skfd;
	struct ifreq ifr;
	struct ethtool_value edata;
	struct mii_ioctl_data * mii_data;

	if (( skfd = socket( AF_INET, SOCK_DGRAM, 0 ) ) < 0 ) {
		printf("socket error\n");
		return 0;
	}

	edata.cmd = ETHTOOL_GLINK;
	//strncpy(ifr.ifr_name, ifname, sizeof(ifr.ifr_name)-1);
	sprintf(ifr.ifr_name, "%s", ifname);
	ifr.ifr_data = (char *) &edata;
	if (ioctl(skfd, SIOCETHTOOL, &ifr) >= 0) {	// success											     
		close(skfd);
		return (edata.data ? 1 : 0);
	}

	if (ioctl(skfd, SIOCGMIIPHY, &ifr) < 0) {
		printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, ifname, errno, strerror(errno));
		close(skfd);
		return 0;
	}

	mii_data = (struct mii_ioctl_data *) &(ifr.ifr_data);
	mii_data->reg_num = MII_BMSR;
	if (ioctl(skfd, SIOCGMIIREG, &ifr) < 0) {
		printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, ifname, errno, strerror(errno));
		close(skfd);
		return 0;
	}

	close(skfd);

	if ((mii_data->val_out) & MII_BMSR_LSTATUS)
		return 1;

	return 0;
}

/* return: 
 *     0 : ready
 *     1 : inteface down
 *     2 : invalid ip (0.0.0.0)
 *     3 : unknown
 */
int securesoho_nic_ifc_status(const char *devname)
{
	char szBuffer[16*sizeof(struct ifreq)];
	struct ifconf ifConf;
	struct ifreq ifReq;
	int nResult;
	int LocalSock;
	struct sockaddr_in LocalAddr;
	int i, ret=3;

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		printf("FILE:%s,LINE:%d.Can't do that\r\n",__FILE__,__LINE__);
		exit(1);
	}
	ifConf.ifc_len = sizeof szBuffer;
	ifConf.ifc_ifcu.ifcu_buf = (caddr_t)szBuffer;
	nResult = ioctl(LocalSock, SIOCGIFCONF, &ifConf);
	if (nResult < 0) {
		printf("FILE:%s,LINE:%d.ioctl error\r\n",__FILE__,__LINE__);
		exit(1);
	}
	for (i = 0;(i < ifConf.ifc_len);) {
		struct ifreq *pifReq = (struct ifreq *)((caddr_t)ifConf.ifc_req + i);
		i += sizeof *pifReq;
		strcpy (ifReq.ifr_name, pifReq -> ifr_name);
		if (strcmp (devname, pifReq -> ifr_name)==0) {
			if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0) {
				printf("FILE:%s,LINE:%d.Can't get flags\r\n",__FILE__,__LINE__);
				exit(1);
			}
			if (ifReq.ifr_flags & IFF_UP) {
				if (pifReq -> ifr_addr.sa_family == AF_INET) {
					memcpy (&LocalAddr, &pifReq -> ifr_addr, sizeof pifReq -> ifr_addr);
					if (LocalAddr.sin_addr.s_addr != 0) {
						/* valid ip */
						ret = 0;
					}else{
						/* invalid ip */
						ret = 2;
					}
				}
			}else{
				/* inteface down */
				ret = 1;
			}
			goto out;
		}
	}
out:
	close(LocalSock);
	return ret;
}

ENUM_NIC_TYPE securesoho_nic_get_type(void)
{
	ENUM_NIC_TYPE type=NIC_TYPE_UNKNOWN;
	char dev[64]={0};

	securesoho_lif_get(dev);
	
	if(!strcmp(dev, LAN_PORT))
		type = NIC_TYPE_WIRED;
	else if(!strcmp(dev, WLAN_PORT))
		type = NIC_TYPE_WIRELESS;
	return type;
		
}

#define	 NIC_RATE_100MBPS  (100*1000)
#define	 NIC_RATE_10MBPS   (10*1000)
static int _nic_wired_max_bit_rate(char *dev)
{
	int rate=0;
	int skfd=0;
	unsigned int lpa, bmcr, advert;
	struct ifreq ifr;
	struct mii_ioctl_data * mii_data;

	if(!securesoho_nic_check_ifc_link(dev, strlen(dev))){
		/* not link up */
		fprintf(stdout, "F:%s:%d, %s Net Link Down\n", __FUNCTION__, __LINE__, dev);
		goto out;
	}

	if (( skfd = socket( AF_INET, SOCK_DGRAM, 0 ) ) < 0 ) {
		printf("socket error\n");
		goto out;
	}

	sprintf(ifr.ifr_name, "%s", dev);
        if (ioctl(skfd, SIOCGMIIPHY, &ifr) < 0) {
                printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, 
		       dev, errno, strerror(errno));
                close(skfd);
                return 0;
        }

	mii_data = (struct mii_ioctl_data *) &(ifr.ifr_data);
	mii_data->reg_num = MII_BMCR;
	mii_data->val_out = 0;
	if (ioctl(skfd, SIOCGMIIREG, &ifr) < 0) {
		printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, dev, errno, strerror(errno));
		goto out;
	}
	bmcr = (unsigned int)(mii_data->val_out);

	mii_data = (struct mii_ioctl_data *) &(ifr.ifr_data);
	mii_data->val_out = 0;
	mii_data->reg_num = MII_ADVERTISE;
	if (ioctl(skfd, SIOCGMIIREG, &ifr) < 0) {
		printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, dev, errno, strerror(errno));
		goto out;
	}
	advert = (unsigned int)(mii_data->val_out);
	
	mii_data = (struct mii_ioctl_data *) &(ifr.ifr_data);
	mii_data->val_out = 0;
	mii_data->reg_num = MII_LPA;
	if (ioctl(skfd, SIOCGMIIREG, &ifr) < 0) {
		printf("%s:%d [%s] errno=%d (%s)\n", __FUNCTION__, __LINE__, dev, errno, strerror(errno));
		goto out;
	}
	lpa = (unsigned int)(mii_data->val_out);

	if (bmcr & BMCR_ANENABLE){
		unsigned int nego;
		nego = mii_nway_result(advert & lpa);
		switch(nego){
		case LPA_10FULL:
		case LPA_10HALF:
			rate = NIC_RATE_10MBPS;
			//fprintf(stdout, "F:%s:%d, 10Mbps Net Link\n", __FUNCTION__, __LINE__);
			break;
		case LPA_100FULL:
		case LPA_100BASE4:
		case LPA_100HALF:
			rate = NIC_RATE_100MBPS;
			//fprintf(stdout, "F:%s:%d, 100Mbps Net Link\n", __FUNCTION__, __LINE__);
			break;
		default:
			/* default set link speed to 100Mbps */
			rate = NIC_RATE_10MBPS;
			//fprintf(stdout, "F:%s:%d, default set 10Mbps Net Link,%d\n", __FUNCTION__, __LINE__, lpa);
			break;
		}
	}else{
		rate = (bmcr & BMCR_SPEED100)?(NIC_RATE_100MBPS):(NIC_RATE_10MBPS);
		//fprintf(stdout, "F:%s:%d, default set Net Link,%d\n", __FUNCTION__, __LINE__, rate);
	}
out:
	if(skfd > 0) close(skfd);
	return rate;
}

static void _parse_iwconfig_output(char *fn, int *max_rate)
{
	char line[256];
	FILE *fp=NULL;
	char *p, *q;

	fp = fopen(fn, "r");
	if(NULL == fp){
		goto out;
	}

	while(fgets(line, 256, fp)){
		if((p = strstr(line, "Bit Rate:"))){
			/* parse the bit rate */
			p += strlen("Bit Rate:");
			if((q = strstr(p, "Mb/s"))){
				*q = '\0';
				*max_rate = atoi(p) * 1000;
			}if((q = strstr(p, "kb/s"))){
				* q = '\0';
				*max_rate = atoi(p);
			}else
				continue;
		}
	}
	if(fp) fclose(fp);
out:
	return;
}

#define TMP_IWCONFIG_OUTPUT_FMT "/tmp/iwconfig-%s.out"
static int _nic_wireless_max_bit_rate(char *dev)
{
	char cmd_buf[256];
	char tmp_iwconfig_output[128];

	int rate=1000;	/*default 1M */
	
	sprintf(tmp_iwconfig_output, TMP_IWCONFIG_OUTPUT_FMT, dev);
	unlink(tmp_iwconfig_output);
	
	sprintf(cmd_buf, "iwconfig %s >%s", dev, tmp_iwconfig_output);
	
	system(cmd_buf);

	_parse_iwconfig_output(tmp_iwconfig_output, &rate);
	return rate;
}

int securesoho_nic_max_bit_rate(void)
{
	int rate=0;
	char dev[64]={0};

	securesoho_lif_get(dev);

	/* wired */
	if(!strcmp(dev, LAN_PORT)){
		rate = _nic_wired_max_bit_rate(dev);
	}

	/* wireless */
	if(!strcmp(dev, WLAN_PORT)){
		rate = _nic_wireless_max_bit_rate(dev);
	}
	return rate;
}

int securesoho_nic_get_ipaddress_list(network_interface_t** addresslist)
{
	char szBuffer[16*sizeof(struct ifreq)];
	struct ifconf ifConf;
	struct ifreq ifReq;
	int nResult;
	int LocalSock;
	struct sockaddr_in LocalAddr;
	int ctr=0;
	int i;
	network_interface_t tempresults[16];

	/* Create an unbound datagram socket to do the SIOCGIFADDR ioctl on. */
	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0){
		printf("Create socket error!\n");
		return 0;
	}
	/* Get the interface configuration information... */
	ifConf.ifc_len = sizeof szBuffer;
	ifConf.ifc_ifcu.ifcu_buf = (caddr_t)szBuffer;
	nResult = ioctl(LocalSock, SIOCGIFCONF, &ifConf);
	if (nResult < 0){
		printf("ioctl error!\n");
		return 0;
	}
	/* Cycle through the list of interfaces looking for IP addresses. */
	for (i = 0;(i < ifConf.ifc_len);){
		struct ifreq *pifReq = (struct ifreq *)((caddr_t)ifConf.ifc_req + i);
		i += sizeof *pifReq;
		/* See if this is the sort of interface we want to deal with. */
		strcpy (ifReq.ifr_name, pifReq -> ifr_name);
		if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
			printf("Can't get interface flags!\n");
			return 0;
		}
		/* Skip loopback, point-to-point, down and not in used interfaces, */
		/* except don't skip down interfaces */
		/* if we're trying to get a list of configurable interfaces. */
#ifdef DMA_UPNP_SUPPORT_LOOPBACK
		if (                                    (!(ifReq.ifr_flags & IFF_UP)))
#else //DMA_UPNP_SUPPORT_LOOPBACK
		if ((ifReq.ifr_flags & IFF_LOOPBACK) || (!(ifReq.ifr_flags & IFF_UP)))
#endif //DMA_UPNP_SUPPORT_LOOPBACK
		{
			continue;
		}
		if (pifReq -> ifr_addr.sa_family == AF_INET){
			/* Get a pointer to the address... */
			memcpy (&LocalAddr, &pifReq -> ifr_addr, sizeof pifReq -> ifr_addr);
#ifdef DMA_UPNP_SUPPORT_LOOPBACK
			if (1)
#else //DMA_UPNP_SUPPORT_LOOPBACK
			if (LocalAddr.sin_addr.s_addr != htonl (INADDR_LOOPBACK))
#endif //DMA_UPNP_SUPPORT_LOOPBACK
			{
				tempresults[ctr].ipaddress = LocalAddr.sin_addr.s_addr;
				strcpy(tempresults[ctr].name,ifReq.ifr_name);
				ctr++;
			}
		}
	}
	close(LocalSock);
	if(ctr == 0){
		*addresslist = NULL;
	} else{
		*addresslist = (network_interface_t*)malloc(sizeof(network_interface_t)*(ctr));
		memcpy(*addresslist,tempresults,sizeof(network_interface_t)*ctr);
	}
	return(ctr);
}

int securesoho_nic_ifc_up(const char *devname, int timeout)
{
	int LocalSock;
	struct ifreq ifReq;
	char buf[64];

	printf("\033[1;32m%s[%d] ENTER, devname=[%s]\033[0m\n",__FUNCTION__,__LINE__, devname);
	if (devname == NULL || *devname == '\0') {
		printf("\033[1;32m%s[%d] invilad devname[%s]\033[0m\n",__FUNCTION__,__LINE__, devname);
		return -1;
	}
	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0) {
		snprintf(buf, sizeof(buf), "%s[%d] socket", __FUNCTION__, __LINE__);
		perror(buf);
		return -1;
	}

	do {
		strcpy (ifReq.ifr_name, devname);

		if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
			snprintf(buf, sizeof(buf), "\033[1;32m%s[%d] SIOCGIFFLAGS\033[0m", __FUNCTION__, __LINE__);
			perror(buf);
			close(LocalSock);
			return -1;
		}

		ifReq.ifr_flags |= IFF_UP;
		if (ioctl(LocalSock, SIOCSIFFLAGS, &ifReq) < 0) {
			snprintf(buf, sizeof(buf), "\033[1;32m%s[%d] SIOCSIFFLAGS\033[0m", __FUNCTION__, __LINE__);
			perror(buf);
			if (!timeout)  
				break;
			--timeout;
			sleep(1);
			continue;
		}

		if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
			snprintf(buf, sizeof(buf), "\033[1;32m%s[%d] SIOCGIFFLAGS\033[0m", __FUNCTION__, __LINE__);
			perror(buf);
			close(LocalSock);
			return -1;
		}

		if ((ifReq.ifr_flags & IFF_UP)){
			close(LocalSock);
			return 0;
		}			

		if (!timeout)  
			break;
		--timeout;
		sleep(1);
	} while (timeout);
	printf("%s[%d] Timeout\n",__FUNCTION__,__LINE__);

	close(LocalSock);
	return -1;
}

int securesoho_is_same_sub_network(unsigned long ip1, unsigned long ip2)
{
	char *maskstr = "255.255.0.0";
	unsigned long mask;

	mask = inet_addr(maskstr);
	if((ip1 & mask) == (ip2 & mask))
		return 1;
	return 0;
}

unsigned long securesoho_get_ip_from_url(const char* url)
{
	int	  i;
	char* p=NULL;
	char* tmpIPString=NULL;
	unsigned long outIP;
	char  tmpurl[strlen(url)+1];

	if(strlen(url)<14)
		return 0;
	outIP=0;
	sprintf(tmpurl,"%s",url);
	p=strtok(tmpurl,"/");
	i=0;
	while(p!=NULL){
		i++;
		if(i==2){
			tmpIPString=p;
			break;
		}
		p=strtok(NULL,"/");
	}
	p=strtok(tmpIPString,":");
	return inet_addr(p);
}

static unsigned long localipaddr = 0;
unsigned long securesoho_get_local_ip_addr()
{
	char szBuffer[16*sizeof(struct ifreq)];
	struct ifconf ifConf;
	struct ifreq ifReq;
	int nResult;
	int LocalSock;
	struct sockaddr_in LocalAddr;
	int i;
	char ifname[6];
	ifname[0] = 0;
	
	if(localipaddr > 0){
		return localipaddr;
	}
	ifname[0] = '\0';
	securesoho_string_get( "virdev_int", ifname );
	if (!ifname[0])
		strcpy(ifname, CONF_LAN_INTERFACE );

	if ((LocalSock = socket (AF_INET, SOCK_DGRAM, 0))<0){
		return 0;
	}
	ifConf.ifc_len = sizeof szBuffer;
	ifConf.ifc_ifcu.ifcu_buf = (caddr_t)szBuffer;
	nResult = ioctl(LocalSock, SIOCGIFCONF, &ifConf);
	if (nResult < 0){
		perror("ioctl:SIOCGIFCONF ");
		exit(1);
	}
	for (i = 0;(i < ifConf.ifc_len);){
		struct ifreq *pifReq = (struct ifreq *)((caddr_t)ifConf.ifc_req + i);
		i += sizeof *pifReq;
		strcpy (ifReq.ifr_name, pifReq -> ifr_name);
		if (strcmp (ifname, pifReq -> ifr_name)==0){
			if (ioctl (LocalSock, SIOCGIFFLAGS, &ifReq) < 0){
				perror("ioctl:SIOCGIFFLAGS ");
				exit(1);
			}
			if (!(ifReq.ifr_flags & IFF_UP)){
				break;
			}
			if (pifReq -> ifr_addr.sa_family == AF_INET){
				memcpy (&LocalAddr, &pifReq -> ifr_addr, sizeof pifReq -> ifr_addr);
				if (LocalAddr.sin_addr.s_addr != 0){
					localipaddr = LocalAddr.sin_addr.s_addr;
					return LocalAddr.sin_addr.s_addr;
				}
				break;
			}
		}
	}
	return 0;
}

int securesoho_check_url_for_proxy(char* url)
{
    int http_proxy_enable = 0;
    char http_proxy[256] = {0};	/* it should like: 172.19.144.52:8080 */
    unsigned long ip1 = 0, ip2 = 0;
    int is_same_sub_network = 0;
    int is_http_service_type = 0;
    int use_http_proxy = 0;

    if(NULL == url)
	    return use_http_proxy;
    http_proxy_enable = securesoho_int_get("HTTP_PROXY_ENABLE");
    securesoho_string_get("HTTP_PROXY_IP_PORT", http_proxy);

    ip2 = securesoho_get_local_ip_addr();
    ip1 = securesoho_get_ip_from_url(url);
    if(securesoho_is_same_sub_network(ip1, ip2)){
	    is_same_sub_network = 1;
    }
    if (!strncasecmp (url, "http://", 7)) {
	    is_http_service_type = 1;
    }
    use_http_proxy = (is_http_service_type && 0 == is_same_sub_network && http_proxy_enable && http_proxy[0] != '\0');

    return use_http_proxy;
}
