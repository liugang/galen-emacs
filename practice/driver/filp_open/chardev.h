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

#define RT_DEBUG_OFF	    0
#define RT_DEBUG_ERROR	    1
#define RT_DEBUG_WARN	    2
#define RT_DEBUG_TRACE	    3
#define RT_DEBUG_INFO	    4
#define RT_DEBUG_LOUD	    5

ULONG RTDebugLevel = RT_DEBUG_ERROR;

/***********************************************************************************
 *	OS file operation related data structure definitions
 ***********************************************************************************/
typedef struct file* RTMP_OS_FD;

typedef struct _OS_FS_INFO_
{
	int				fsuid;
	int				fsgid;
	mm_segment_t	fs;
} OS_FS_INFO;

#define RTMP_OS_FS_INFO				OS_FS_INFO

extern ULONG		RTDebugLevel;

#define DBGPRINT_RAW(Level, Fmt)    \
do{				      \
    if (Level <= RTDebugLevel)	    \
    {				    \
	printk Fmt;		  \
    }				    \
}while(0)

#define DBGPRINT(Level, Fmt)	DBGPRINT_RAW(Level, Fmt)

typedef char *PSTRING;


#define RTMP_FILE_RDONLY			0x0F01
#define RTMP_FILE_WRONLY			0x0F02
#define RTMP_FILE_CREAT				0x0F03
#define RTMP_FILE_TRUNC				0x0F04

#define STA_PROFILE_PATH			"/tmp/RT2870STA.dat"

#define IS_FILE_OPEN_ERR(_fd)	((_fd == NULL) || IS_ERR((_fd)))

/***********************************************************************************
 *	Network related constant definitions
 ***********************************************************************************/
#define NDIS_STATUS_SUCCESS			0x00
#define NDIS_STATUS_FAILURE			0x01
#define NDIS_STATUS_INVALID_DATA		0x02
#define NDIS_STATUS_RESOURCES			0x03

typedef char STRING;

typedef signed char CHAR;

typedef signed short SHORT;
typedef signed int INT;
typedef signed long LONG;
typedef signed long long LONGLONG;

typedef unsigned long long ULONGLONG;

typedef unsigned char BOOLEAN;
typedef void VOID;

#define MAX_INI_BUFFER_SIZE		4096
#define MAX_PARAM_BUFFER_SIZE		(2048)	/* enough for ACL (18*64) */

#ifndef TRUE
#define TRUE						1
#endif
#ifndef FALSE
#define FALSE						0
#endif

/***********************************************************************************
 *	Compiler related definitions
 ***********************************************************************************/
#undef __inline
#define __inline		static inline
#define IN
#define OUT
#define INOUT
#define NDIS_STATUS		INT

typedef char *PSTRING;
typedef VOID *PVOID;
typedef CHAR *PCHAR;
typedef UCHAR *PUCHAR;
typedef USHORT *PUSHORT;
typedef LONG *PLONG;
typedef ULONG *PULONG;
typedef UINT *PUINT;

#define ASSERT(x)								\
{										\
    if (!(x))									\
    {										\
	printk(__FILE__ ":%d assert " #x "failed\n", __LINE__);	   \
    }										\
}
