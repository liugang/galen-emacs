/* 
 * Copyright (C) 2006-2010, Alphanetworks, inc.
 * vim:cindent:ts=4:
 */
#ifndef __SECURESOHO_FIRMWARE_H__
#define __SECURESOHO_FIRMWARE_H__

#define LENGTH_NAME_FIRMWARE               52
typedef struct FIRMWARE_HEADER {
	union {
		struct {
			char magic_name[12];	/* magic name: REDSONIC */
			char version[LENGTH_NAME_FIRMWARE];/* 0~52 */
			int has_bootloader;		/* 53~56 */
			int has_kernel;			/* 57~60 */
			int has_rootfs;			/* 61~64 */
			int has_para0;			/* 65~68 */
			unsigned int size_bootloader;	/* 69~72 */
			unsigned int size_kernel;	/* 73~76 */
			unsigned int size_rootfs;	/* 77~80 */
			unsigned int size_para0;	/* 81~84 */
			int do_clean_para0;		/* 85~88 */
			unsigned long CRC;
			int has_splash;			
			unsigned int size_splash;	

		} s;
		char unused[256];
	} u;
} FIRMWARE_HEADER;

#define  m_magic_name          u.s.magic_name        
#define  m_version             u.s.version           
#define  m_has_bld             u.s.has_bootloader    
#define  m_has_kernel          u.s.has_kernel        
#define  m_has_rootfs          u.s.has_rootfs        
#define  m_has_para0           u.s.has_para0        
#define  m_has_splash          u.s.has_splash        
#define  m_size_bld            u.s.size_bootloader   
#define  m_size_kernel         u.s.size_kernel       
#define  m_size_rootfs         u.s.size_rootfs       
#define  m_size_para0          u.s.size_para0        
#define  m_size_splash         u.s.size_splash        
#define  m_clean_para0         u.s.do_clean_para0    
#define  m_crc                 u.s.CRC               

#define  m_unused              u.unused              

#ifdef __x86__
#define MTD_BLOCK_0		"/tmp/mtd_block_0"
#define BOOTLD_BLOCK		"/tmp/mtd_block_1"
#define KERNEL_BLOCK		"/tmp/kernel"
#define ROOTFS_BLOCK		"/tmp/rootfs"
#define PARA_0_BLOCK		"/tmp/para0"
#undef CONF_BOOTLD_BLOCK 
#undef CONF_YAMON_BLOCK 
#undef CONF_KERNEL_BLOCK 
#undef CONF_KERNEL2_BLOCK 
#undef CONF_SPLASH_BLOCK 
#undef CONF_SPLASH2_BLOCK 
#undef CONF_ROOTFS_BLOCK 
#undef CONF_ROOTFS2_BLOCK 
#undef CONF_CONFIG_BLOCK 
#undef CONF_CONFIG1_BLOCK 
#undef CONF_CONFIG2_BLOCK

#define	CONF_BOOTLD_BLOCK	"/tmp/sigmblocka"
#define	CONF_YAMON_BLOCK	"/tmp/sigmblockc"
#define	CONF_KERNEL_BLOCK	"/tmp/sigmblockd"
#define	CONF_KERNEL2_BLOCK	"/tmp/sigmblocke"
#define	CONF_SPLASH_BLOCK	"/tmp/sigmblockf"
#define	CONF_SPLASH2_BLOCK	"/tmp/sigmblockg"
#define	CONF_ROOTFS_BLOCK	"/tmp/sigmblockh"
#define	CONF_ROOTFS2_BLOCK	"/tmp/sigmblocki"
#define	CONF_CONFIG_BLOCK	"/tmp/sigmblockj"
#define	CONF_CONFIG1_BLOCK	"/tmp/sigmblockk"
#define	CONF_CONFIG2_BLOCK	"/tmp/sigmblockl"
#else
#ifndef CONF_MTD_BLOCK_0
#define MTD_BLOCK_0		"/dev/mtdblock/0"
#else
#define MTD_BLOCK_0           CONF_MTD_BLOCK_0
#endif

#ifndef CONF_BOOTLD_BLOCK
#define BOOTLD_BLOCK		"/dev/mtdblock/1"
#else
#define BOOTLD_BLOCK		CONF_BOOTLD_BLOCK
#endif

#ifndef CONF_KERNEL_BLOCK
#define KERNEL_BLOCK ""
#else
#define KERNEL_BLOCK           CONF_KERNEL_BLOCK
#endif

#ifndef CONF_ROOTFS_BLOCK
#define ROOTFS_BLOCK "/dev/mtdblock/3"
#else
#define ROOTFS_BLOCK           CONF_ROOTFS_BLOCK
#endif

#ifndef CONF_PARA_0_BLOCK
#define PARA_0_BLOCK 		"/dev/mtdblock/2"
#else
#define PARA_0_BLOCK 		CONF_PARA_0_BLOCK
#endif
#endif

#ifndef CONF_SPLASH_BLOCK
#define SPLASH_BLOCK "/dev/null"
#else
#define SPLASH_BLOCK           CONF_SPLASH2_BLOCK
#endif

#define DEVICE_REGION_DATA_VERSION	'A'

typedef struct _device_region_data {
	unsigned char version;	/* version: 'A' char */
	unsigned char tv_mode;	/* ntsc, pal, secam */
	unsigned char region;	/* DUSA, WW, EU etc */
	unsigned char location;	/* USA, CA etc */
} device_region_data;

typedef enum {
	DTM_NTSC = 0,
	DTM_PAL,
	DTM_SECAM
} device_tv_mode;

typedef enum {
	DR_AMERICA = 0,
	DR_WW,
	DR_EU,
	DR_ASIA,
	DR_OCEANIA,
	DR_HEBREW = 100
} device_region;

/* device location will use MODEL_NUMBER for compatible issue 
 * model number will only check last 4 bits: nm & 0xf.
 * so the new added item should less than 16, 
 * or don't override the last 4 bits with old one, e.g.: MN_AUS 0x2E.
 */
typedef enum {
	MN_EU=1,
	MN_WW,
	MN_WWN,
	MN_WWP,
	MN_CA,
	MN_TW,		/* 5 */
	MN_USA = 15,
	MN_UNKNOWN = 32,
	MN_AUS = 46,	/* 0x2E, the last 4 bits is 0xE */
	MN_RUS = 62	/* 0x3E, the last 4 bits is 0xE */
} MODEL_NUMBER;

typedef struct {
	int                 incomplete;
	char                url[2048];

	size_t               bootloader_written;
	size_t               kernel1_written;
	size_t               kernel2_written;
	size_t               rootfs1_written;
	size_t               rootfs2_written;
	size_t               para0_written;
	size_t               splash_written;
} upgrade_resume_info_t;


extern int securesoho_get_device_region_data(device_region_data *data);
extern int securesoho_set_device_region_data(device_region_data *data);

int securesoho_get_upgrade_resume_info(upgrade_resume_info_t *ri);
int securesoho_set_upgrade_resume_info(upgrade_resume_info_t *ri);

#endif				//__SECURESOHO_FIRMWARE_H__
