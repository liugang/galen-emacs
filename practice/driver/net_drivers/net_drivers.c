#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/init.h>
#include <linux/version.h>
#include <linux/string.h>
#include <linux/list.h>

#include <linux/ieee80211.h>
#include <linux/usb.h>
#include <linux/spinlock.h>
#include <linux/timer.h>
#include <linux/errno.h>
#include <linux/slab.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/ethtool.h>
#include <linux/wireless.h>
#include <linux/proc_fs.h>
#include <linux/delay.h>
#include <linux/if_arp.h>
#include <linux/ctype.h>
#include <linux/vmalloc.h>
#include <net/iw_handler.h>
/* #include <net/ifx_ppa_api.h> */
/* #include <net/ifx_ppa_hook.h> */
#include <linux/unistd.h>
#include <asm/uaccess.h>
#include <asm/types.h>
#include <asm/unaligned.h>	/* for get_unaligned() */
#include <linux/pid.h>
#include <net/mac80211.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/err.h>
#include <linux/kthread.h>
/* #include <hcd.h> */
#include <stdarg.h>
#include <linux/crypto.h>
#include <linux/scatterlist.h>
/* #include <linux/stdio.h> */
/* #include <linux/stdlib.h> */
#include <linux/time.h>
#include <asm/checksum.h>
#include <net/ip6_checksum.h>
/* #include <stdio.h> */
/* #include <string.h> */
/* #include <stdlib.h> */
#include <linux/types.h>
/* #include <linux/config.h> */
/* #include <netpro/apprehdr.h> */

MODULE_LICENSE("GPL");
MODULE_AUTHOR("galen");

/* OS definition re-declaration */
#ifndef NULL
#define NULL			0
#endif

/* struct module *m = &__this_module; */
/*
	This data structure mainly strip some callback function defined in
	"struct net_device" in kernel source "include/linux/netdevice.h".

	The definition of this data structure may various depends on different
	OS. Use it carefully.
*/
typedef struct _RTMP_OS_NETDEV_OP_HOOK_ {
	void *open;
	void *stop;
	void *xmit;
	void *ioctl;
	void *get_stats;
	void *priv;
	void *get_wstats;
	void *iw_handler;
	int priv_flags;
	unsigned char devAddr[6];
	unsigned char devName[16];
	unsigned char needProtcted;
} RTMP_OS_NETDEV_OP_HOOK, *PRTMP_OS_NETDEV_OP_HOOK;

/***********************************************************************************
 *	OS Memory Access related data structure and definitions
 ***********************************************************************************/
#define MEM_ALLOC_FLAG	    (GFP_ATOMIC) /*(GFP_DMA | GFP_ATOMIC) */

#define NdisMoveMemory(Destination, Source, Length) memmove(Destination, Source, Length)
#define NdisCopyMemory(Destination, Source, Length) memcpy(Destination, Source, Length)
#define NdisZeroMemory(Destination, Length)	    memset(Destination, 0, Length)
#define NdisFillMemory(Destination, Length, Fill)   memset(Destination, Fill, Length)
#define NdisCmpMemory(Destination, Source, Length)  memcmp(Destination, Source, Length)
#define NdisEqualMemory(Source1, Source2, Length)   (!memcmp(Source1, Source2, Length))
#define RTMPEqualMemory(Source1, Source2, Length)	(!memcmp(Source1, Source2, Length))

/* #define MlmeAllocateMemory(_pAd, _ppVA)		os_alloc_mem(_pAd, _ppVA, MGMT_DMA_BUFFER_SIZE) */
/* #define MlmeFreeMemory(_pAd, _pVA)			os_free_mem(_pAd, _pVA) */

#define COPY_MAC_ADDR(Addr1, Addr2)		memcpy((Addr1), (Addr2), MAC_ADDR_LEN)


PNET_DEV RtmpPhyNetDevInit(
	IN VOID						*pAd,
	IN RTMP_OS_NETDEV_OP_HOOK	*pNetDevHook)
{
	struct net_device	*net_dev = NULL;
/*	NDIS_STATUS		Status; */
	ULONG InfId, OpMode;


	RTMP_DRIVER_MAIN_INF_GET(pAd, &InfId);

/*	net_dev = RtmpOSNetDevCreate(pAd, INT_MAIN, 0, sizeof(PRTMP_ADAPTER), INF_MAIN_DEV_NAME); */
	RTMP_DRIVER_MAIN_INF_CREATE(pAd, &net_dev);
	if (net_dev == NULL)
	{
		printk("RtmpPhyNetDevInit(): creation failed for main physical net device!\n");
		return NULL;
	}

	NdisZeroMemory((unsigned char *)pNetDevHook, sizeof(RTMP_OS_NETDEV_OP_HOOK));
	pNetDevHook->open = MainVirtualIF_open;
	pNetDevHook->stop = MainVirtualIF_close;
	pNetDevHook->xmit = rt28xx_send_packets;
#ifdef IKANOS_VX_1X0
	pNetDevHook->xmit = IKANOS_DataFramesTx;
#endif /* IKANOS_VX_1X0 */
	pNetDevHook->ioctl = rt28xx_ioctl;
	pNetDevHook->priv_flags = InfId; /*INT_MAIN; */
	pNetDevHook->get_stats = RT28xx_get_ether_stats;

	pNetDevHook->needProtcted = FALSE;

#if (WIRELESS_EXT < 21) && (WIRELESS_EXT >= 12)
	pNetDevHook->get_wstats = rt28xx_get_wireless_stats;
#endif

	RTMP_DRIVER_OP_MODE_GET(pAd, &OpMode);

#ifdef CONFIG_STA_SUPPORT
#if WIRELESS_EXT >= 12
	if (OpMode == OPMODE_STA)
	{
		pNetDevHook->iw_handler = (void *)&rt28xx_iw_handler_def;
	}
#endif /*WIRELESS_EXT >= 12 */
#endif /* CONFIG_STA_SUPPORT */

#ifdef CONFIG_APSTA_MIXED_SUPPORT
#if WIRELESS_EXT >= 12
	if (OpMode == OPMODE_AP)
	{
		pNetDevHook->iw_handler = &rt28xx_ap_iw_handler_def;
	}
#endif /*WIRELESS_EXT >= 12 */
#endif /* CONFIG_APSTA_MIXED_SUPPORT */

	RTMP_OS_NETDEV_SET_PRIV(net_dev, pAd);
/*	pAd->net_dev = net_dev; */

	RTMP_DRIVER_NET_DEV_SET(pAd, net_dev);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,24)
	SET_MODULE_OWNER(net_dev);
#endif

	return net_dev;
}

int print_module_test(void)
{
//    struct module *mod;
//    list_for_each_entry(mod, &m->list, list) {
//	printk("%s\n", mod->name);
//    }
	struct	net_device		*net_dev = NULL;

	net_dev = RtmpPhyNetDevInit(pAd, &netDevHook);
	if (net_dev == NULL)
		goto err_out_free_radev;

err_out_free_radev:
	RTMPFreeAdapter(pAd);

	return 0;
}




/* init and exit function */
static int list_print_init(void)
{
    printk("load net_drivers module.\n");
    print_module_test();
    return 0;
}

static void list_print_exit(void)
{
    printk("unload net_drivers module.\n");
}

module_init(list_print_init);
module_exit(list_print_exit);
