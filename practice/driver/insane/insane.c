/*
 * insane.c --	source for the "insane" module, a sample virtual interface
 *
 * Copyright (c) 2000 Alessandro Rubini (rubini@linux.it)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 */


/* #ifndef __KERNEL__ */
/* #  define __KERNEL__ */
/* #endif */

/* The Makefile takes care of adding -DMODULE */


#include "insane.h"
MODULE_LICENSE("GPL");
MODULE_AUTHOR("galen");

VOID				*pAd_Global = (VOID *) NULL;

RTMP_DRV_ABL_OPS RtmpDrvOps, *pRtmpDrvOps = &RtmpDrvOps;
RTMP_NET_ABL_OPS RtmpDrvNetOps, *pRtmpDrvNetOps = &RtmpDrvNetOps;

static int rt_probe(void);

// #if LINUX_VERSION_CODE < 0x020300 /* 2.2 compatibility */
// #  define net_device device
// #  define __dev_get_by_name dev_get
// #endif

// static struct net_device insane_dev; /* forward decl */
// 
// 
// /* --------------------------------------------------------------------------
//  * definition of the "private" data structure used by this interface
//  */
// struct insane_private {
//     struct net_device_stats priv_stats;
//     struct net_device *priv_device; /* interface used to xmit data */
//     int priv_mode; /* how to drop packets */
//     int priv_arg1; /* arguments to the dropping mode */
//     int priv_arg2;
// };
// 
// /* --------------------------------------------------------------------------
//  * open and close
//  */
// int insane_open(struct net_device *dev)
// {
//     /* mark the device as operational */
//     dev->start = 1;
//     dev->tbusy = 0;
//     MOD_INC_USE_COUNT;
//     return 0;
// }
// int insane_close(struct net_device *dev)
// {
//     dev->start = 0;
//     dev->tbusy = 1;
//     MOD_DEC_USE_COUNT;
//     return 0;
// }
// 
// /* --------------------------------------------------------------------------
//  * get_stats: return a pointer to the device statistics
//  */
// struct net_device_stats *insane_get_stats(struct net_device *dev)
// {
//     return &((struct insane_private *)dev->priv)->priv_stats;
// }
// 
// /* --------------------------------------------------------------------------
//  * header stuff: fall back on the slave interface to deal with this stuff
//  */
// int insane_hard_header(struct sk_buff *skb, struct net_device *dev, 
//	unsigned short type, void *daddr, void *saddr, unsigned len)
// {
//     struct insane_private *priv = dev->priv;
//     int retval;
// 
//     skb->dev = priv->priv_device;
//     retval = skb->dev->hard_header(skb, skb->dev, type, daddr, saddr, len);
//     skb->dev = dev;
//     return retval;
// }
// 
// static int insane_rebuild_header(struct sk_buff *skb)
// {
//     struct insane_private *priv = insane_dev.priv;
//     int retval;
// 
//     skb->dev = priv->priv_device;
//     retval = skb->dev->rebuild_header(skb);
//     skb->dev = &insane_dev;
//     return retval;
// }
// 
// /* --------------------------------------------------------------------------
//  * neighbors: this comes form shaper.c (Alan Cox) and is needed for ARP to work
//  */
// int insane_neigh_setup(struct neighbour *n)
// {
//     if (n->nud_state == NUD_NONE) {
//	n->ops = &arp_broken_ops;
//	n->output = n->ops->output;
//     }
//     return 0;
// }
// 
// int insane_neigh_setup_dev(struct net_device *dev, struct neigh_parms *p)
// {
//     if (p->tbl->family == AF_INET) {
//	p->neigh_setup = insane_neigh_setup;
//	p->ucast_probes = 0;
//	p->mcast_probes = 0;
//     }
//     return 0;
// }
// 
// 
// /* --------------------------------------------------------------------------
//  * xmit: actual delivery (or not) of the data packets
//  */
// int insane_xmit(struct sk_buff *skb, struct net_device *dev)
// {
//     struct insane_private *priv = dev->priv;
//     int accept; /* accept this packet or drop it */
//     static unsigned long randval;
// 
//     if (!priv->priv_device) { /* cannot send to anyone, just return */
//	priv->priv_stats.tx_errors++;
//	priv->priv_stats.tx_dropped++;
//	return 0;
//     }
// 
//     switch (priv->priv_mode) {
//	
//	case INSANE_PERCENT:
//	    if (!randval) randval = jiffies; /* a typical seed */
//	    /* hash the value, according to the TYPE_0 rule of glibc */
//	    randval = ((randval * 1103515245) + 12345) & 0x7fffffff;
//	    accept = (randval % 100) < priv->priv_arg1;
//	    break;
// 
//	case INSANE_TIME:
//	    randval = jiffies % (priv->priv_arg1 + priv->priv_arg2);
//	    accept = randval < priv->priv_arg1;
//	    break;
//	    
//	case INSANE_PASS:
//	default: /* unknown mode: default to pass */
//	    accept = 1;
//     }
//	    
//     if (!accept) {
//	priv->priv_stats.tx_errors++;
//	priv->priv_stats.tx_dropped++;
//	return 0;
//     }
//     /* else, pass it to the real interface */
// 
//     priv->priv_stats.tx_packets++;
//     priv->priv_stats.tx_bytes += skb->len;
// 
// #if 0
//     return priv->priv_device->hard_start_xmit(skb, priv->priv_device);
// #else
//     skb->dev = priv->priv_device;
//     skb->priority = 1;
//     dev_queue_xmit (skb);
//     return 0;
// #endif
// }
// 
// /* --------------------------------------------------------------------------
//  * ioctl: let user programs configure this interface
//  */
// int insane_ioctl(struct net_device *dev, struct ifreq *ifr, int cmd)
// {
//     int err;
//     struct net_device *slave;
//     struct insane_private *priv = dev->priv;
//     /* hold a local (kernel-space) copy of the configuration data */
//     struct insane_userinfo info;
//     /* and a pointer into user space as well */
//     struct insane_userinfo *uptr = (struct insane_userinfo *)ifr->ifr_data;
// 
// 
//     /* only authorized users can control the interface */
//     if (cmd == SIOCINSANESETINFO && !capable(CAP_NET_ADMIN))
//	return -EPERM;
//     
//     /* process the command */
//     switch(cmd) {
//	case SIOCINSANEGETINFO: /* return configuration to user space */
// 
//	    /* interface name */
//	    memset(info.name, 0, INSANE_NAMELEN);
//	    if (priv->priv_device)
//		strncpy(info.name, priv->priv_device->name, INSANE_NAMELEN-1);
//	    
//	    /* parameters */
//	    info.mode = priv->priv_mode;
//	    info.arg1 = priv->priv_arg1;
//	    info.arg2 = priv->priv_arg2;
// 
//	    /* return the data structure to  user space */
//	    err = copy_to_user(uptr, &info, sizeof(info));
//	    if (err) return err;
//	    break;
// 
//	case SIOCINSANESETINFO:
//	    /* retrieve the data structure from user space */
//	    err = copy_from_user(&info, uptr, sizeof(info));
//	    if (err) return err;
// 
//	    printk("name: %s, arg %i %i\n", info.name, info.arg1, info.arg2);
// 
//	    /* interface name */
//	    slave = __dev_get_by_name(info.name);
//	    if (!slave)
//		return -ENODEV;
//	    if (slave->type != ARPHRD_ETHER && slave->type != ARPHRD_LOOPBACK)
//		return -EINVAL;
// 
//	    /* The interface is good, get hold of it */
//	    priv->priv_device = slave;
//	    if (slave->hard_header)
//		dev->hard_header = insane_hard_header;
//	    else
//		dev->hard_header = NULL;
// 
//	    if (slave->rebuild_header)
//		dev->rebuild_header = insane_rebuild_header;
//	    else
//		dev->rebuild_header = NULL;
// 
//	    /* also, and clone its IP, MAC and other information */
//	    memcpy(dev->dev_addr,  slave->dev_addr,  sizeof(slave->dev_addr));
//	    memcpy(dev->broadcast, slave->broadcast, sizeof(slave->broadcast));
// 
//	    /* accept the parameters (no checks here) */
//	    priv->priv_mode = info.mode;
//	    priv->priv_arg1 = info.arg1;
//	    priv->priv_arg2 = info.arg2;
// 
//	    break;
// 
//	default:
//	    return -EOPNOTSUPP;
//     }
//     return 0;
// }
// 
// /* --------------------------------------------------------------------------
//  * The initialization function: it is used to assign fields in the structure
//  */
// 
// /*
//  * The __init attribute has no effect (by now) in modules; it is nonetheless
//  * good practice to declare initialization functions as such, when working
//  * in Linux kernel space.
//  */
// int __init insane_init(struct net_device *dev)
// {
//     /*
//	* fill the fields of the device structure that are used
//	*/
// 
//     /* priv is used to host the statistics, and packet dropping policy */
//     dev->priv = kmalloc(sizeof(struct insane_private), GFP_USER);
//     if (!dev->priv) return -ENOMEM;
//     memset(dev->priv, 0, sizeof(struct insane_private));
// 
//     ether_setup(dev); /* assign some of the fields as "generic ethernet" */
// 
//     /* dev->flags |= IFF_NOARP; */
// 
//     /* these are the device methods */
//     dev->open		    = insane_open;
//     dev->stop		    = insane_close;
//     dev->do_ioctl	    = insane_ioctl;
//     dev->get_stats	    = insane_get_stats;
// 
//     /* the hardware transmission method */
//     dev->hard_start_xmit    = insane_xmit;
// 
//     /* and finally this one, needed for ARP to work */
//     dev->neigh_setup = insane_neigh_setup_dev;
// 
//     return 0;
// }
// 
// /* --------------------------------------------------------------------------
//  * declaration of the interface data structure
//  */
// 
// static struct net_device insane_dev = {
//     name: "insane",
//     init: insane_init,
// };
/*
 * this way of defining fields is gcc-specific. "name" is the first
 * field, and "init" the 12th. Most drivers use something like
 *     struct net_device mydev = {"name", 0,0,0,0, 0,0,0,0,0, NULL, my_init};
 */

/* --------------------------------------------------------------------------
 * module entry points
 */


/* pAd MUST allow to be NULL */
NDIS_STATUS os_free_mem(
	IN VOID *pReserved,
	IN PVOID mem)
{
	ASSERT(mem);
	kfree(mem);

	return (NDIS_STATUS_SUCCESS);
}

VOID	RTMPFreeAdapter(
	IN	VOID		*pAdSrc)
{
	PRTMP_ADAPTER pAd = (PRTMP_ADAPTER)pAdSrc;
	POS_COOKIE os_cookie;
//	int index;

	os_cookie=(POS_COOKIE)pAd->OS_Cookie;

//	if (pAd->BeaconBuf)
//		os_free_mem(NULL, pAd->BeaconBuf);
// 
//	NdisFreeSpinLock(&pAd->MgmtRingLock);
// 
//	for (index =0 ; index < NUM_OF_TX_RING; index++)
//	{
//		NdisFreeSpinLock(&pAd->TxSwQueueLock[index]);
//		NdisFreeSpinLock(&pAd->DeQueueLock[index]);
//		pAd->DeQueueRunning[index] = FALSE;
//	}
// 
//	NdisFreeSpinLock(&pAd->irq_lock);
// 
// 
// #ifdef DOT11_N_SUPPORT
//	NdisFreeSpinLock(&pAd->mpdu_blk_pool.lock);
// #endif /* DOT11_N_SUPPORT */
// 
//	if (pAd->iw_stats)
//	{
//		os_free_mem(NULL, pAd->iw_stats);
//		pAd->iw_stats = NULL;
//	}
//	if (pAd->stats)
//	{
//		os_free_mem(NULL, pAd->stats);
//		pAd->stats = NULL;
//	}
// 
//	NdisFreeSpinLock(&TimerSemLock);
//	RTMP_OS_FREE_TIMER(pAd);
//	RTMP_OS_FREE_LOCK(pAd);
//	RTMP_OS_FREE_TASKLET(pAd);
//	RTMP_OS_FREE_TASK(pAd);
//	RTMP_OS_FREE_SEM(pAd);
//	RTMP_OS_FREE_ATOMIC(pAd);

//	RtmpOsVfree(pAd); /* pci_free_consistent(os_cookie->pci_dev,sizeof(RTMP_ADAPTER),pAd,os_cookie->pAd_pa); */
	if (os_cookie)
		os_free_mem(NULL, os_cookie);
}

void RtmpOSNetDevFree(PNET_DEV pNetDev) {
	ASSERT(pNetDev);
	free_netdev(pNetDev);
}

INT RtmpOSNetDevAlloc(IN PNET_DEV *new_dev_p,
		      IN UINT32 privDataSize) {
	/* assign it as null first. */
	*new_dev_p = NULL;

	DBGPRINT(RT_DEBUG_TRACE,
		 ("Allocate a net device with private data size=%d!\n",
		  privDataSize));
	*new_dev_p = alloc_etherdev(privDataSize);

	if (*new_dev_p)
		return NDIS_STATUS_SUCCESS;
	else
		return NDIS_STATUS_FAILURE;
}

INT RtmpOSNetDevOpsAlloc(IN PVOID *pNetDevOps) {
	*pNetDevOps = (PVOID) vmalloc(sizeof (struct net_device_ops));
	if (*pNetDevOps) {
		NdisZeroMemory(*pNetDevOps, sizeof (struct net_device_ops));
		return NDIS_STATUS_SUCCESS;
	} else {
		return NDIS_STATUS_FAILURE;
	}
}

PNET_DEV RtmpOSNetDevGetByName(PNET_DEV pNetDev,
			       PSTRING pDevName) {
	PNET_DEV pTargetNetDev = NULL;

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,26)
	pTargetNetDev = dev_get_by_name(dev_net(pNetDev), pDevName); /* >= 2.6.26 */
#else
	ASSERT(pNetDev);
	pTargetNetDev = dev_get_by_name(pNetDev->nd_net, pDevName); /* 2.6.24 - 2.6.25 */
#endif
#else
	pTargetNetDev = dev_get_by_name(pDevName); /* <= 2.6.23 */
#endif /* KERNEL_VERSION(2,6,24) */

	return pTargetNetDev;
}

void RtmpOSNetDeviceRefPut(PNET_DEV pNetDev) {
	/*
	   every time dev_get_by_name is called, and it has returned a valid struct
	   net_device*, dev_put should be called afterwards, because otherwise the
	   machine hangs when the device is unregistered (since dev->refcnt > 1).
	 */
	if (pNetDev)
		dev_put(pNetDev);
}

/*
  *	Assign the network dev name for created Ralink WiFi interface.
  */
static int RtmpOSNetDevRequestName(IN INT32 MC_RowID,
				   IN UINT32 *pIoctlIF,
				   IN PNET_DEV dev,
				   IN PSTRING pPrefixStr,
				   IN INT devIdx) {
	PNET_DEV existNetDev;
	STRING suffixName[IFNAMSIZ];
	STRING desiredName[IFNAMSIZ];
	int ifNameIdx,
	 prefixLen,
	 slotNameLen;
	int Status;

	prefixLen = strlen(pPrefixStr);
	ASSERT((prefixLen < IFNAMSIZ));

	for (ifNameIdx = devIdx; ifNameIdx < 32; ifNameIdx++) {
		memset(suffixName, 0, IFNAMSIZ);
		memset(desiredName, 0, IFNAMSIZ);
		strncpy(&desiredName[0], pPrefixStr, prefixLen);
		sprintf(suffixName, "%d", ifNameIdx);
		slotNameLen = strlen(suffixName);
		ASSERT(((slotNameLen + prefixLen) < IFNAMSIZ));
		strcat(desiredName, suffixName);

		existNetDev = RtmpOSNetDevGetByName(dev, &desiredName[0]);
		if (existNetDev == NULL)
			break;
		else
			RtmpOSNetDeviceRefPut(existNetDev);
	}

	if (ifNameIdx < 32) {

		strcpy(&dev->name[0], &desiredName[0]);
		Status = NDIS_STATUS_SUCCESS;
	} else {
		DBGPRINT(RT_DEBUG_ERROR,
			 ("Cannot request DevName with preifx(%s) and in range(0~32) as suffix from OS!\n",
			  pPrefixStr));
		Status = NDIS_STATUS_FAILURE;
	}

	return Status;
}

PNET_DEV RtmpOSNetDevCreate(IN INT32 MC_RowID,
			    IN UINT32 *pIoctlIF,
			    IN INT devType,
			    IN INT devNum,
			    IN INT privMemSize,
			    IN PSTRING pNamePrefix) {

	int status;
	struct net_device *pNetDev = NULL;

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	struct net_device_ops *pNetDevOps = NULL;
#endif

	/* allocate a new network device */
	status = RtmpOSNetDevAlloc(&pNetDev, 0 /*privMemSize */ );
	if (status != NDIS_STATUS_SUCCESS) {
		/* allocation fail, exit */
		DBGPRINT(RT_DEBUG_ERROR,
			 ("Allocate network device fail (%s)...\n",
			  pNamePrefix));
		return NULL;
	}
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	status = RtmpOSNetDevOpsAlloc((PVOID) & pNetDevOps);
	if (status != NDIS_STATUS_SUCCESS) {
		/* error! no any available ra name can be used! */
		DBGPRINT(RT_DEBUG_TRACE, ("Allocate net device ops fail!\n"));
		RtmpOSNetDevFree(pNetDev);

		return NULL;
	} else {
		DBGPRINT(RT_DEBUG_TRACE,
			 ("Allocate net device ops success!\n"));
		pNetDev->netdev_ops = pNetDevOps;
	}
#endif
	/* find a available interface name, max 32 interfaces */
	status =
	    RtmpOSNetDevRequestName(MC_RowID, pIoctlIF, pNetDev, pNamePrefix,
				    devNum);
	if (status != NDIS_STATUS_SUCCESS) {
		/* error! no any available ra name can be used! */
		DBGPRINT(RT_DEBUG_ERROR,
			 ("Assign interface name (%s with suffix 0~32) failed...\n",
			  pNamePrefix));
		RtmpOSNetDevFree(pNetDev);

		return NULL;
	} else {
		DBGPRINT(RT_DEBUG_TRACE,
			 ("The name of the new %s interface is %s...\n",
			  pNamePrefix, pNetDev->name));
	}

	return pNetDev;
}

PNET_DEV RtmpPhyNetDevMainCreate(
	IN VOID				*pAdSrc)
{
	PRTMP_ADAPTER pAd = (PRTMP_ADAPTER)pAdSrc;
	PNET_DEV pDevNew;
	UINT32 MC_RowID = 0, IoctlIF = 0;

	pAd = pAd;

	pDevNew = RtmpOSNetDevCreate((INT32)MC_RowID, (UINT32 *)&IoctlIF,
					INT_MAIN, 0, sizeof(PRTMP_ADAPTER), INF_MAIN_DEV_NAME);

	return pDevNew;
}

PNET_DEV RtmpPhyNetDevInit(
	IN VOID						*pAd,
	IN RTMP_OS_NETDEV_OP_HOOK	*pNetDevHook)
{
	struct net_device	*net_dev = NULL;
	ULONG InfId, OpMode;


//	RTMP_DRIVER_MAIN_INF_GET(pAd, &InfId); // same as below
	InfId = INT_MAIN;

//	RTMP_DRIVER_MAIN_INF_CREATE(pAd, &net_dev); // same as below
	net_dev = RtmpPhyNetDevMainCreate(pAd); /* not use pAd here */

	if (net_dev == NULL)
	{
		printk("RtmpPhyNetDevInit(): creation failed for main physical net device!\n");
		return NULL;
	}

	NdisZeroMemory((unsigned char *)pNetDevHook, sizeof(RTMP_OS_NETDEV_OP_HOOK));
//	pNetDevHook->open = MainVirtualIF_open;
//	pNetDevHook->stop = MainVirtualIF_close;
//	pNetDevHook->xmit = rt28xx_send_packets;
//	pNetDevHook->ioctl = rt28xx_ioctl;
	pNetDevHook->priv_flags = InfId; /*INT_MAIN; */
//	pNetDevHook->get_stats = RT28xx_get_ether_stats;
// 
	pNetDevHook->needProtcted = FALSE;

#if (WIRELESS_EXT < 21) && (WIRELESS_EXT >= 12)
	pNetDevHook->get_wstats = rt28xx_get_wireless_stats;
#endif

//	RTMP_DRIVER_OP_MODE_GET(pAd, &OpMode); // same as below
//	OpMode = pAd->OpMode;
	OpMode = OPMODE_STA;

#if WIRELESS_EXT >= 12
	if (OpMode == OPMODE_STA)
	{
//		pNetDevHook->iw_handler = (void *)&rt28xx_iw_handler_def;
	}
#endif /*WIRELESS_EXT >= 12 */

	RTMP_OS_NETDEV_SET_PRIV(net_dev, pAd); // net_dev->ml_priv = pAd

//	RTMP_DRIVER_NET_DEV_SET(pAd, net_dev); // same as below
//	pAd->net_dev =net_dev;

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
	SET_MODULE_OWNER(net_dev);
#endif

	return net_dev;
}


void RtmpOSNetDevDetach(PNET_DEV pNetDev) {

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	struct net_device_ops *pNetDevOps = (struct net_device_ops *)pNetDev->netdev_ops;
#endif

	unregister_netdev(pNetDev);

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	vfree(pNetDevOps);
#endif
}

BOOLEAN RtmpPhyNetDevExit(
	IN VOID			*pAd, 
	IN PNET_DEV		net_dev)
{
	/* Unregister network device */
	if (net_dev != NULL)
	{
		printk("RtmpOSNetDevDetach(): RtmpOSNetDeviceDetach(), dev->name=%s!\n", net_dev->name);
		RtmpOSNetDevDetach(net_dev);
	}

	return TRUE;
}

BOOLEAN RtmpRaDevCtrlExit(IN VOID *pAdSrc)
{
	PRTMP_ADAPTER	pAd = (PRTMP_ADAPTER)pAdSrc;

	RTMPFreeAdapter(pAd);

	return TRUE;
}

int RtmpOSNetDevAttach(IN UCHAR OpMode,
		       IN PNET_DEV pNetDev,
		       IN RTMP_OS_NETDEV_OP_HOOK *pDevOpHook) {
	int ret, rtnl_locked = FALSE;

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	struct net_device_ops *pNetDevOps = (struct net_device_ops *)pNetDev->netdev_ops;
#endif

	DBGPRINT(RT_DEBUG_TRACE, ("RtmpOSNetDevAttach()--->\n"));

	/* If we need hook some callback function to the net device structrue, now do it. */
	if (pDevOpHook) {
/*		PRTMP_ADAPTER pAd = NULL; */

/*		GET_PAD_FROM_NET_DEV(pAd, pNetDev); */

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
//		pNetDevOps->ndo_open = pDevOpHook->open;
//		pNetDevOps->ndo_stop = pDevOpHook->stop;
//		pNetDevOps->ndo_start_xmit =
//			(HARD_START_XMIT_FUNC) (pDevOpHook->xmit);
//		pNetDevOps->ndo_do_ioctl = pDevOpHook->ioctl;
#else
//		pNetDev->open = pDevOpHook->open;
//		pNetDev->stop = pDevOpHook->stop;
//		pNetDev->hard_start_xmit =
//			(HARD_START_XMIT_FUNC) (pDevOpHook->xmit);
//		pNetDev->do_ioctl = pDevOpHook->ioctl;
#endif

#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,18)
//		pNetDev->ethtool_ops = &RALINK_Ethtool_Ops;
#endif

		/* if you don't implement get_stats, just leave the callback function as NULL, a dummy 
		   function will make kernel panic.
		*/
		if (pDevOpHook->get_stats)
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
			pNetDevOps->ndo_get_stats = pDevOpHook->get_stats;
#else
		pNetDev->get_stats = pDevOpHook->get_stats;
#endif

		/* OS specific flags, here we used to indicate if we are virtual interface */
		pNetDev->priv_flags = pDevOpHook->priv_flags;

#if (WIRELESS_EXT < 21) && (WIRELESS_EXT >= 12)
/*		pNetDev->get_wireless_stats = rt28xx_get_wireless_stats; */
		pNetDev->get_wireless_stats = pDevOpHook->get_wstats;
#endif

#ifdef CONFIG_STA_SUPPORT
#if WIRELESS_EXT >= 12
		if (OpMode == OPMODE_STA) {
/*			pNetDev->wireless_handlers = &rt28xx_iw_handler_def; */
			pNetDev->wireless_handlers = pDevOpHook->iw_handler;
		}
#endif /*WIRELESS_EXT >= 12 */
#endif /* CONFIG_STA_SUPPORT */

#ifdef CONFIG_APSTA_MIXED_SUPPORT
#if WIRELESS_EXT >= 12
		if (OpMode == OPMODE_AP) {
/*			pNetDev->wireless_handlers = &rt28xx_ap_iw_handler_def; */
			pNetDev->wireless_handlers = pDevOpHook->iw_handler;
		}
#endif /*WIRELESS_EXT >= 12 */
#endif /* CONFIG_APSTA_MIXED_SUPPORT */

		/* copy the net device mac address to the net_device structure. */
		NdisMoveMemory(pNetDev->dev_addr, &pDevOpHook->devAddr[0],
			       MAC_ADDR_LEN);

		rtnl_locked = pDevOpHook->needProtcted;

	}
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24)
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31)
	pNetDevOps->ndo_validate_addr = NULL;
	/*pNetDev->netdev_ops = ops; */
#else
	pNetDev->validate_addr = NULL;
#endif
#endif

	if (rtnl_locked)
		ret = register_netdevice(pNetDev);
	else
		ret = register_netdev(pNetDev); /* do here */

	netif_stop_queue(pNetDev);

	DBGPRINT(RT_DEBUG_TRACE, ("<---RtmpOSNetDevAttach(), ret=%d\n", ret));
	if (ret == 0)
		return NDIS_STATUS_SUCCESS;
	else
		return NDIS_STATUS_FAILURE;
}

static int rt_probe(void)
{
	struct	net_device		*net_dev = NULL;
	RTMP_OS_NETDEV_OP_HOOK		netDevHook;
//	INT				res =1;
	INT				status;
	ULONG				OpMode;
	DBGPRINT(RT_DEBUG_TRACE, ("===>rt2870_probe()!\n"));

	net_dev = RtmpPhyNetDevInit(pAd_Global, &netDevHook);
	if (net_dev == NULL)
		goto err_out_free_radev;

	/* for supporting Network Manager.
	  * Set the sysfs physical device reference for the network logical device if set prior to registration will 
	  * cause a symlink during initialization.
	 */
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,0))
//	SET_NETDEV_DEV(net_dev, &(usb_dev->dev));
#endif

//	RTMP_DRIVER_STA_DEV_TYPE_SET(pAd, net_dev->type);
//	pAd_Global->StaCfg.OriDevType = net_dev->type;

//	OpMode = pAd->OpMode;
	OpMode = OPMODE_STA;

	status = RtmpOSNetDevAttach(OpMode, net_dev, &netDevHook);
	if (status != 0)
		goto err_out_free_netdev;

	return 0;

err_out_free_netdev:
	RtmpOSNetDevFree(net_dev);

err_out_free_radev:
	RTMPFreeAdapter(pAd_Global);

	return -1;
}

static void rt_disconnect(void)
{
	struct net_device *net_dev;

//	net_dev = (VOID *)(pAd->net_dev);
	PRTMP_ADAPTER pAd = (PRTMP_ADAPTER)pAd_Global;
	if(pAd == NULL) {
	       printk(KERN_ALERT "Goodbye, finalize net driver!\n");
	}
	net_dev = pAd->net_dev;

	RtmpPhyNetDevExit(pAd_Global, net_dev); /* unregister net dev */

	/* free the root net_device */
	RtmpOSNetDevFree(net_dev); /* free net dev */

	RtmpRaDevCtrlExit(pAd_Global); /* free pAd_Global */
}

//#ifdef MODULE /* It is defined, by Makefile, but you can try without it */

static int init_insane_module(void)
{
	printk(KERN_ALERT "Hello, initialize net driver!\n");
	rt_probe();
	return 0;
}

static void cleanup_insane_module(void)
{
	printk(KERN_ALERT "Goodbye, finalize net driver!\n");
	rt_disconnect();  // check memory issue!
}

module_init(init_insane_module);
module_exit(cleanup_insane_module);
