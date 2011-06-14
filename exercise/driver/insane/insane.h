#ifndef __INSANE_H__
#define __INSANE_H__

#include <linux/kernel.h>    /* printk() */
#include <linux/slab.h>	   /* kmalloc() */
#include <linux/errno.h>     /* error codes */
#include <linux/netdevice.h> /* basic data structures */
#include <linux/init.h>	     /* __init */
#include <linux/if_arp.h>    /* ARPHRD_ETHER */
#include <net/arp.h>	     /* neighbor stuff */

#include <asm/uaccess.h>  /* memcpy and such */

#include <linux/module.h>
#include <linux/version.h>
#include <linux/string.h>
#include <linux/list.h>

#include <linux/ieee80211.h>
#include <linux/usb.h>
#include <linux/spinlock.h>
#include <linux/timer.h>
#include <linux/interrupt.h>
#include <linux/pci.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/ethtool.h>
#include <linux/wireless.h>
#include <linux/proc_fs.h>
#include <linux/delay.h>
#include <linux/ctype.h>
#include <linux/vmalloc.h>
#include <net/iw_handler.h>
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
#include <stdarg.h>
#include <linux/crypto.h>
#include <linux/scatterlist.h>
#include <linux/time.h>
#include <asm/checksum.h>
#include <net/ip6_checksum.h>
#include <linux/types.h>



enum insane_mode {
    INSANE_PASS = 0,   /* every packet is transmitted (default) */
    INSANE_PERCENT,    /* pass some percent of the packets */
    INSANE_TIME,       /* work (and fail) on a timely basis */
};

#define INSANE_NAMELEN 16

struct insane_userinfo {
    char name[INSANE_NAMELEN];
    int mode;
    int arg1;
    int arg2;
};

/* These are the two ioctl commands needed to interact with insane */
#define SIOCINSANESETINFO SIOCDEVPRIVATE
#define SIOCINSANEGETINFO (SIOCDEVPRIVATE+1)



#ifndef TRUE
#define TRUE						1
#endif
#ifndef FALSE
#define FALSE						0
#endif

#define RT_DEBUG_OFF	    0
#define RT_DEBUG_ERROR	    1
#define RT_DEBUG_WARN	    2
#define RT_DEBUG_TRACE	    3
#define RT_DEBUG_INFO	    4
#define RT_DEBUG_LOUD	    5

typedef struct os_cookie	* POS_COOKIE;

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

typedef unsigned char UINT8;
typedef unsigned short UINT16;
typedef unsigned int UINT32;
typedef unsigned long long UINT64;
typedef short INT16;
typedef int INT32;
typedef long long INT64;

typedef unsigned char UCHAR;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned long ULONG;

typedef char STRING;
typedef signed char CHAR;
typedef signed short SHORT;
typedef signed int INT;
typedef signed long LONG;
typedef signed long long LONGLONG;

typedef unsigned long long ULONGLONG;
typedef unsigned char BOOLEAN;
typedef void VOID;

typedef char *PSTRING;
typedef VOID *PVOID;
typedef CHAR *PCHAR;
typedef UCHAR *PUCHAR;
typedef USHORT *PUSHORT;
typedef LONG *PLONG;
typedef ULONG *PULONG;
typedef UINT *PUINT;


typedef struct net_device	* PNET_DEV;

/***********************************************************************************
 *	Compiler related definitions
 ***********************************************************************************/
#undef __inline
#define __inline		static inline
#define IN
#define OUT
#define INOUT
#define NDIS_STATUS		INT

#define MAC_ADDR_LEN			6

#define ASSERT(x)								\
{										\
    if (!(x))									\
    {										\
	printk(__FILE__ ":%d assert " #x "failed\n", __LINE__);	   \
    }										\
}

ULONG RTDebugLevel = RT_DEBUG_TRACE;

extern ULONG		RTDebugLevel;

#define DBGPRINT_RAW(Level, Fmt)    \
do{				      \
    if (Level <= RTDebugLevel)	    \
    {				    \
	printk Fmt;		  \
    }				    \
}while(0)

#define DBGPRINT(Level, Fmt)	DBGPRINT_RAW(Level, Fmt)


/* ========================================================================== */
/* operators used in NETIF module */
/* Note: No need to put any compile option here */
typedef struct _RTMP_DRV_ABL_OPS {

// NDIS_STATUS	(*RTMPAllocAdapterBlock)(
//	IN  PVOID					handle,
//	OUT	VOID					**ppAdapter);

// VOID	(*RTMPFreeAdapter)(
//	IN	VOID					*pAd);

// BOOLEAN (*RtmpRaDevCtrlExit)(
//	IN	VOID					*pAd);

// INT (*RtmpRaDevCtrlInit)(
//	IN	VOID					*pAd,
//	IN	RTMP_INF_TYPE			infType);

// VOID (*RTMPHandleInterrupt)(
//	IN	VOID					*pAd);

// INT (*RTMP_COM_IoctlHandle)(
//	IN	VOID					*pAd,
//	IN	RTMP_IOCTL_INPUT_STRUCT	*wrq,
//	IN	INT						cmd,
//	IN	USHORT					subcmd,
//	IN	VOID					*pData,
//	IN	ULONG					Data);

// int (*RTMPSendPackets)(
//	IN	NDIS_HANDLE				MiniportAdapterContext,
//	IN	PPNDIS_PACKET			ppPacketArray,
//	IN	UINT					NumberOfPackets,
//	IN	UINT32					PktTotalLen,
//	IN	RTMP_NET_ETH_CONVERT_DEV_SEARCH	Func);

// int (*MBSS_PacketSend)(
//	IN	PNDIS_PACKET				pPktSrc,
//	IN	PNET_DEV					pDev,
//	IN	RTMP_NET_PACKET_TRANSMIT	Func);

// int (*WDS_PacketSend)(
//	IN	PNDIS_PACKET				pPktSrc,
//	IN	PNET_DEV					pDev,
//	IN	RTMP_NET_PACKET_TRANSMIT	Func);

// int (*APC_PacketSend)(
//	IN	PNDIS_PACKET				pPktSrc,
//	IN	PNET_DEV					pDev,
//	IN	RTMP_NET_PACKET_TRANSMIT	Func);

// int (*MESH_PacketSend)(
//	IN	PNDIS_PACKET				pPktSrc,
//	IN	PNET_DEV					pDev,
//	IN	RTMP_NET_PACKET_TRANSMIT	Func);

// int (*P2P_PacketSend)(
//	IN	PNDIS_PACKET				pPktSrc,
//	IN	PNET_DEV					pDev,
//	IN	RTMP_NET_PACKET_TRANSMIT	Func);

// INT (*RTMP_AP_IoctlHandle)(
//	IN	VOID					*pAd,
//	IN	RTMP_IOCTL_INPUT_STRUCT	*wrq,
//	IN	INT						cmd,
//	IN	USHORT					subcmd,
//	IN	VOID					*pData,
//	IN	ULONG					Data);

// INT (*RTMP_STA_IoctlHandle)(
//	IN	VOID					*pAd,
//	IN	RTMP_IOCTL_INPUT_STRUCT	*wrq,
//	IN	INT						cmd,
//	IN	USHORT					subcmd,
//	IN	VOID					*pData,
//	IN	ULONG					Data,
//	IN  USHORT		    priv_flags);

// VOID (*RTMPDrvOpen)(
//	IN	VOID					*pAd);

// VOID (*RTMPDrvClose)(
//	IN	VOID					*pAd,
//	IN	VOID					*net_dev);

// VOID (*RTMPInfClose)(
//	IN	VOID					*pAd);

// int (*rt28xx_init)(
//	IN	VOID					*pAd,
//	IN	PSTRING					pDefaultMac,
//	IN	PSTRING					pHostName);
} RTMP_DRV_ABL_OPS;

typedef struct _RTMP_NET_ABL_OPS {

#ifdef RTMP_USB_SUPPORT
/* net complete handlers */
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkOutDataPacketComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkOutMLMEPacketComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkOutNullFrameComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkOutRTSFrameComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkOutPsPollComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpNetUsbBulkRxComplete;

/* drv complete handlers */
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkOutDataPacketComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkOutMLMEPacketComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkOutNullFrameComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkOutRTSFrameComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkOutPsPollComplete;
RTMP_DRV_USB_COMPLETE_HANDLER	RtmpDrvUsbBulkRxComplete;
#endif /* RTMP_USB_SUPPORT */

} RTMP_NET_ABL_OPS;

/* */
/*  The miniport adapter structure */
/* */
struct _RTMP_ADAPTER {
	PVOID OS_Cookie;	/* save specific structure relative to OS */
	PNET_DEV net_dev;
	ULONG VirtualIfCnt;
#if 0
//	RTMP_CHIP_OP chipOps;
//	RTMP_CHIP_CAP chipCap;



//#ifdef CONFIG_STA_SUPPORT
	USHORT ThisTbttNumToNextWakeUp;
//#endif /* CONFIG_STA_SUPPORT */


//	NDIS_SPIN_LOCK irq_lock;

	/*======Cmd Thread in PCI/RBUS/USB */
//	CmdQ CmdQ;
//	NDIS_SPIN_LOCK CmdQLock;	/* CmdQLock spinlock */
//	RTMP_OS_TASK cmdQTask;

/*****************************************************************************************/
/*	Both PCI/USB related parameters														  */
/*****************************************************************************************/
	/*RTMP_DEV_INFO			chipInfo; */
	RTMP_INF_TYPE infType;

/*****************************************************************************************/
/*	Driver Mgmt related parameters														  */
/*****************************************************************************************/
	RTMP_OS_TASK mlmeTask;
#ifdef RTMP_TIMER_TASK_SUPPORT
	/* If you want use timer task to handle the timer related jobs, enable this. */
	RTMP_TIMER_TASK_QUEUE TimerQ;
	NDIS_SPIN_LOCK TimerQLock;
	RTMP_OS_TASK timerTask;
#endif /* RTMP_TIMER_TASK_SUPPORT */

/*****************************************************************************************/
/*	Tx related parameters								*/
/*****************************************************************************************/
	BOOLEAN DeQueueRunning[NUM_OF_TX_RING];	/* for ensuring RTUSBDeQueuePacket get call once */
	NDIS_SPIN_LOCK DeQueueLock[NUM_OF_TX_RING];


	/* resource for software backlog queues */
	QUEUE_HEADER TxSwQueue[NUM_OF_TX_RING];	/* 4 AC + 1 HCCA */
	NDIS_SPIN_LOCK TxSwQueueLock[NUM_OF_TX_RING];	/* TxSwQueue spinlock */

	RTMP_DMABUF MgmtDescRing;	/* Shared memory for MGMT descriptors */
	RTMP_MGMT_RING MgmtRing;
	NDIS_SPIN_LOCK MgmtRingLock;	/* Prio Ring spinlock */

	UCHAR LastMCUCmd;

/*****************************************************************************************/
/*	Rx related parameters								*/
/*****************************************************************************************/



/*****************************************************************************************/
/*	ASIC related parameters								 */
/*****************************************************************************************/
	UINT32 MACVersion;	/* MAC version. Record rt2860C(0x28600100) or rt2860D (0x28600101).. */

	/* --------------------------- */
	/* E2PROM */
	/* --------------------------- */
	ULONG EepromVersion;	/* byte 0: version, byte 1: revision, byte 2~3: unused */
	ULONG FirmwareVersion;	/* byte 0: Minor version, byte 1: Major version, otherwise unused. */
	USHORT EEPROMDefaultValue[NUM_EEPROM_BBP_PARMS];
	UCHAR EEPROMAddressNum;	/* 93c46=6  93c66=8 */
	BOOLEAN EepromAccess;
	UCHAR EFuseTag;

	/* --------------------------- */
	/* BBP Control */
	/* --------------------------- */
/*#if defined(RTMP_RBUS_SUPPORT) || defined(RT3593) */
/*	UCHAR			BbpWriteLatch[256];	// record last BBP register value written via BBP_IO_WRITE/BBP_IO_WRITE_VY_REG_ID */
/*#else */
/*	UCHAR			BbpWriteLatch[140];	// record last BBP register value written via BBP_IO_WRITE/BBP_IO_WRITE_VY_REG_ID */
/*#endif // defined(RTMP_RBUS_SUPPORT) || defined(RT3593) */
	UCHAR BbpWriteLatch[MAX_BBP_ID + 1];	/* record last BBP register value written via BBP_IO_WRITE/BBP_IO_WRITE_VY_REG_ID */
	CHAR BbpRssiToDbmDelta;	/* change from UCHAR to CHAR for high power */
	BBP_R66_TUNING BbpTuning;

	/* ---------------------------- */
	/* RFIC control */
	/* ---------------------------- */
	UCHAR RfIcType;		/* RFIC_xxx */
	ULONG RfFreqOffset;	/* Frequency offset for channel switching */


	RTMP_RF_REGS LatchRfRegs;	/* latch th latest RF programming value since RF IC doesn't support READ */

	EEPROM_ANTENNA_STRUC Antenna;	/* Since ANtenna definition is different for a & g. We need to save it for future reference. */
	EEPROM_NIC_CONFIG2_STRUC NicConfig2;

	/* This soft Rx Antenna Diversity mechanism is used only when user set */
	/* RX Antenna = DIVERSITY ON */
	SOFT_RX_ANT_DIVERSITY RxAnt;

	CHANNEL_TX_POWER TxPower[MAX_NUM_OF_CHANNELS];	/* Store Tx power value for all channels. */
	CHANNEL_TX_POWER ChannelList[MAX_NUM_OF_CHANNELS];	/* list all supported channels for site survey */

	UCHAR ChannelListNum;	/* number of channel in ChannelList[] */
	UCHAR Bbp94;
	BOOLEAN BbpForCCK;
	ULONG Tx20MPwrCfgABand[MAX_TXPOWER_ARRAY_SIZE];
	ULONG Tx20MPwrCfgGBand[MAX_TXPOWER_ARRAY_SIZE];
	ULONG Tx40MPwrCfgABand[MAX_TXPOWER_ARRAY_SIZE];
	ULONG Tx40MPwrCfgGBand[MAX_TXPOWER_ARRAY_SIZE];




	BOOLEAN bAutoTxAgcA;	/* Enable driver auto Tx Agc control */
	UCHAR TssiRefA;		/* Store Tssi reference value as 25 temperature. */
	UCHAR TssiPlusBoundaryA[5];	/* Tssi boundary for increase Tx power to compensate. */
	UCHAR TssiMinusBoundaryA[5];	/* Tssi boundary for decrease Tx power to compensate. */
	UCHAR TxAgcStepA;	/* Store Tx TSSI delta increment / decrement value */
	CHAR TxAgcCompensateA;	/* Store the compensation (TxAgcStep * (idx-1)) */

	BOOLEAN bAutoTxAgcG;	/* Enable driver auto Tx Agc control */
	UCHAR TssiRefG;		/* Store Tssi reference value as 25 temperature. */
	UCHAR TssiPlusBoundaryG[5];	/* Tssi boundary for increase Tx power to compensate. */
	UCHAR TssiMinusBoundaryG[5];	/* Tssi boundary for decrease Tx power to compensate. */
	UCHAR TxAgcStepG;	/* Store Tx TSSI delta increment / decrement value */
	CHAR TxAgcCompensateG;	/* Store the compensation (TxAgcStep * (idx-1)) */

	signed char BGRssiOffset0;	/* Store B/G RSSI#0 Offset value on EEPROM 0x46h */
	signed char BGRssiOffset1;	/* Store B/G RSSI#1 Offset value */
	signed char BGRssiOffset2;	/* Store B/G RSSI#2 Offset value */

	signed char ARssiOffset0;	/* Store A RSSI#0 Offset value on EEPROM 0x4Ah */
	signed char ARssiOffset1;	/* Store A RSSI#1 Offset value */
	signed char ARssiOffset2;	/* Store A RSSI#2 Offset value */

	CHAR BLNAGain;		/* Store B/G external LNA#0 value on EEPROM 0x44h */
	CHAR ALNAGain0;		/* Store A external LNA#0 value for ch36~64 */
	CHAR ALNAGain1;		/* Store A external LNA#1 value for ch100~128 */
	CHAR ALNAGain2;		/* Store A external LNA#2 value for ch132~165 */



/*****************************************************************************************/
/*	802.11 related parameters							 */
/*****************************************************************************************/
	/* outgoing BEACON frame buffer and corresponding TXD */
	TXWI_STRUC BeaconTxWI;
	PUCHAR BeaconBuf;
	USHORT BeaconOffset[HW_BEACON_MAX_NUM];

	/* pre-build PS-POLL and NULL frame upon link up. for efficiency purpose. */
//#ifdef CONFIG_STA_SUPPORT
	PSPOLL_FRAME PsPollFrame;
//#endif /* CONFIG_STA_SUPPORT */
	HEADER_802_11 NullFrame;




/*=======STA=========== */
//#ifdef CONFIG_STA_SUPPORT
	/* ----------------------------------------------- */
	/* STA specific configuration & operation status */
	/* used only when pAd->OpMode == OPMODE_STA */
	/* ----------------------------------------------- */
	STA_ADMIN_CONFIG StaCfg;	/* user desired settings */
	STA_ACTIVE_CONFIG StaActive;	/* valid only when ADHOC_ON(pAd) || INFRA_ON(pAd) */
	CHAR nickname[IW_ESSID_MAX_SIZE + 1];	/* nickname, only used in the iwconfig i/f */
	NDIS_MEDIA_STATE PreMediaState;
//#endif /* CONFIG_STA_SUPPORT */

/*=======Common=========== */
	/* OP mode: either AP or STA */
	UCHAR OpMode;		/* OPMODE_STA, OPMODE_AP */

	NDIS_MEDIA_STATE IndicateMediaState;	/* Base on Indication state, default is NdisMediaStateDisConnected */


	/* configuration: read from Registry & E2PROM */
	BOOLEAN bLocalAdminMAC;	/* Use user changed MAC */
	UCHAR PermanentAddress[MAC_ADDR_LEN];	/* Factory default MAC address */
	UCHAR CurrentAddress[MAC_ADDR_LEN];	/* User changed MAC address */

	/* ------------------------------------------------------ */
	/* common configuration to both OPMODE_STA and OPMODE_AP */
	/* ------------------------------------------------------ */
	COMMON_CONFIG CommonCfg;
	MLME_STRUCT Mlme;

	/* AP needs those vaiables for site survey feature. */
	MLME_AUX MlmeAux;	/* temporary settings used during MLME state machine */

	/*About MacTab, the sta driver will use #0 and #1 for multicast and AP. */
	MAC_TABLE MacTab;	/* ASIC on-chip WCID entry table.  At TX, ASIC always use key according to this on-chip table. */
	NDIS_SPIN_LOCK MacTabLock;


	/* encryption/decryption KEY tables */
	CIPHER_KEY SharedKey[HW_BEACON_MAX_NUM + MAX_P2P_NUM][4];	/* STA always use SharedKey[BSS0][0..3] */

	/* RX re-assembly buffer for fragmentation */
	FRAGMENT_FRAME FragFrame;	/* Frame storage for fragment frame */

	/* various Counters */
	COUNTER_802_3 Counters8023;	/* 802.3 counters */
	COUNTER_802_11 WlanCounters;	/* 802.11 MIB counters */
	COUNTER_RALINK RalinkCounters;	/* Ralink propriety counters */
	COUNTER_DRS DrsCounters;	/* counters for Dynamic TX Rate Switching */
	PRIVATE_STRUC PrivateInfo;	/* Private information & counters */

	/* flags, see fRTMP_ADAPTER_xxx flags */
	ULONG Flags;		/* Represent current device status */
	ULONG PSFlags;		/* Power Save operation flag. */
	ULONG MoreFlags;	/* Represent specific requirement */

	/* current TX sequence # */
	USHORT Sequence;

	/* Control disconnect / connect event generation */
	/*+++Didn't used anymore */
	ULONG LinkDownTime;
	/*--- */
	ULONG LastRxRate;
	ULONG LastTxRate;
	/*+++Used only for Station */
	BOOLEAN bConfigChanged;	/* Config Change flag for the same SSID setting */
	/*--- */

	ULONG ExtraInfo;	/* Extra information for displaying status */
	ULONG SystemErrorBitmap;	/* b0: E2PROM version error */

	/*+++Didn't used anymore */
	ULONG MacIcVersion;	/* MAC/BBP serial interface issue solved after ver.D */
	/*--- */


	BOOLEAN HTCEnable;

	/*****************************************************************************************/
	/*	Statistic related parameters							 */
	/*****************************************************************************************/

	BOOLEAN bUpdateBcnCntDone;

	ULONG macwd;
	/* ---------------------------- */
	/* DEBUG paramerts */
	/* ---------------------------- */
	/*ULONG		DebugSetting[4]; */
	BOOLEAN bPromiscuous;

	/* ---------------------------- */
	/* rt2860c emulation-use Parameters */
	/* ---------------------------- */
	/*ULONG		rtsaccu[30]; */
	/*ULONG		ctsaccu[30]; */
	/*ULONG		cfendaccu[30]; */
	/*ULONG		bacontent[16]; */
	/*ULONG		rxint[RX_RING_SIZE+1]; */
	/*UCHAR		rcvba[60]; */
	BOOLEAN bLinkAdapt;
	BOOLEAN bForcePrintTX;
	BOOLEAN bForcePrintRX;
	/*BOOLEAN		bDisablescanning;		//defined in RT2870 USB */
	BOOLEAN bStaFifoTest;
	BOOLEAN bProtectionTest;
	BOOLEAN bHCCATest;
	BOOLEAN bGenOneHCCA;
	BOOLEAN bBroadComHT;
	/*+++Following add from RT2870 USB. */
	ULONG BulkOutReq;
	ULONG BulkOutComplete;
	ULONG BulkOutCompleteOther;
	ULONG BulkOutCompleteCancel;	/* seems not use now? */
	ULONG BulkInReq;
	ULONG BulkInComplete;
	ULONG BulkInCompleteFail;
	/*--- */

	struct wificonf WIFItestbed;

	UCHAR		TssiGain;



	/* statistics count */

	VOID *iw_stats;
	VOID *stats;


	ULONG TbttTickCount;	/* beacon timestamp work-around */


	/* for detect_wmm_traffic() BE TXOP use */
	ULONG OneSecondnonBEpackets;	/* record non BE packets per second */
	UCHAR is_on;

	/* for detect_wmm_traffic() BE/BK TXOP use */


	UINT8 FlgCtsEnabled;
	UINT8 PM_FlgSuspend;

	EXT_CAP_INFO_ELEMENT ExtCapInfo;


	UINT8 RFICType;


	UINT32 ContinueMemAllocFailCount;

	struct {
		INT IeLen;
		UCHAR *pIe;
	} ProbeRespIE[MAX_LEN_OF_BSS_TABLE];

	/* purpose: We free all kernel resources when module is removed */
	LIST_HEADER RscTimerMemList;	/* resource timers memory */
	LIST_HEADER RscTaskMemList;	/* resource tasks memory */
	LIST_HEADER RscLockMemList;	/* resource locks memory */
	LIST_HEADER RscTaskletMemList;	/* resource tasklets memory */
	LIST_HEADER RscSemMemList;	/* resource semaphore memory */
	LIST_HEADER RscAtomicMemList;	/* resource atomic memory */

	/* purpose: Cancel all timers when module is removed */
	LIST_HEADER RscTimerCreateList;	/* timers list */


#ifdef P2P_SUPPORT
	RT_P2P_CONFIG			P2pCfg;
	RT_P2P_TABLE			P2pTable;
	ULONG					GOBeaconBufNoALen;
	CHAR					GoBeaconBuf[512]; /* NOTE: BeaconBuf should be 4-byte aligned */
	ULONG					BeaconBufLen;
	ULONG					GoBeaconBufLen;
	BOOLEAN					bIsClearScanTab;   /* TURE, we need to force Scan */
	BOOLEAN					flg_p2p_init;
	ULONG					flg_p2p_OpStatusFlags;
	UCHAR					P2PChannel;
#ifdef DOT11_N_SUPPORT
	UINT8					P2PExtChOffset;
#endif /* DOT11_N_SUPPORT */
	UCHAR					P2PCurrentAddress[MAC_ADDR_LEN];	  /* User changed MAC address */
	PNET_DEV				p2p_dev;
#endif /* P2P_SUPPORT */


#endif

};

typedef struct _RTMP_ADAPTER *PRTMP_ADAPTER;

/***********************************************************************************
 *	Network related constant definitions
 ***********************************************************************************/
#define NDIS_STATUS_SUCCESS			0x00
#define NDIS_STATUS_FAILURE			0x01
#define NDIS_STATUS_INVALID_DATA		0x02
#define NDIS_STATUS_RESOURCES			0x03


/* After Linux 2.6.9,
 * VLAN module use Private (from user) interface flags (netdevice->priv_flags).
 * #define IFF_802_1Q_VLAN 0x1	       --    802.1Q VLAN device.  in if.h
 * ref to ip_sabotage_out() [ out->priv_flags & IFF_802_1Q_VLAN ] in br_netfilter.c
 *
 * For this reason, we MUST use EVEN value in priv_flags
 */
#define INT_MAIN			0x0100
#define INT_MBSSID			0x0200
#define INT_WDS				0x0300
#define INT_APCLI			0x0400
#define INT_MESH			0x0500
#define INT_P2P				0x0600


#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,29)
#define RTMP_OS_NETDEV_GET_PRIV(_pNetDev)		((_pNetDev)->ml_priv)
#define RTMP_OS_NETDEV_SET_PRIV(_pNetDev, _pPriv)	((_pNetDev)->ml_priv = (_pPriv))
#else
#define RTMP_OS_NETDEV_GET_PRIV(_pNetDev)		((_pNetDev)->priv)
#define RTMP_OS_NETDEV_SET_PRIV(_pNetDev, _pPriv)	((_pNetDev)->priv = (_pPriv))
#endif
#define RTMP_OS_NETDEV_GET_DEVNAME(_pNetDev)	((_pNetDev)->name)
#define RTMP_OS_NETDEV_GET_PHYADDR(_pNetDev)	((_pNetDev)->dev_addr)

/***********************************************************************************
 *	OS Memory Access related data structure and definitions
 ***********************************************************************************/
#define NdisMoveMemory(Destination, Source, Length) memmove(Destination, Source, Length)
#define NdisCopyMemory(Destination, Source, Length) memcpy(Destination, Source, Length)
#define NdisZeroMemory(Destination, Length)	    memset(Destination, 0, Length)
#define NdisFillMemory(Destination, Length, Fill)   memset(Destination, Fill, Length)
#define NdisCmpMemory(Destination, Source, Length)  memcmp(Destination, Source, Length)
#define NdisEqualMemory(Source1, Source2, Length)   (!memcmp(Source1, Source2, Length))
#define RTMPEqualMemory(Source1, Source2, Length)	(!memcmp(Source1, Source2, Length))

#define MlmeAllocateMemory(_pAd, _ppVA)				os_alloc_mem(_pAd, _ppVA, MGMT_DMA_BUFFER_SIZE)
#define MlmeFreeMemory(_pAd, _pVA)					os_free_mem(_pAd, _pVA)

#define COPY_MAC_ADDR(Addr1, Addr2)			memcpy((Addr1), (Addr2), MAC_ADDR_LEN)

#define INF_MAIN_DEV_NAME		"ra"
#define INF_MBSSID_DEV_NAME		"ra"
#define INF_WDS_DEV_NAME		"wds"
#define INF_APCLI_DEV_NAME		"apcli"
#define INF_MESH_DEV_NAME		"mesh"
#define INF_P2P_DEV_NAME		"p2p"

#define OPMODE_STA		    0
#define OPMODE_AP		    1
#define OPMODE_APSTA		    2	/* as AP and STA at the same time */

#endif /* __INSANE_H__ */
