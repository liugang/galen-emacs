#ifndef __SECURESOHO_SERVER_H
#define __SECURESOHO_SERVER_H

#define SECURESOHO_ST_VENDOR_UNKNOWN   0x00000000
#define SECURESOHO_ST_VENDOR_REDSONIC  0x00000100
#define SECURESOHO_ST_VENDOR_RHAPSODY  0x00000200
#define SECURESOHO_ST_VENDOR_INTEL     0x00000400
#define SECURESOHO_ST_VENDOR_WMC       0x00000800
#define SECURESOHO_ST_VENDOR_TWONKY    0x00001000
#define SECURESOHO_ST_VENDOR_AMC       0x00002000
#define SECURESOHO_ST_VENDOR_IMS       0x00003000	/* iMediaServer */
#define SECURESOHO_ST_VENDOR_VTUNER    0x00004000
#define SECURESOHO_ST_VENDOR_IPS       0x00005000	/* IPS */
#define SECURESOHO_ST_VENDOR_LIVE365   0x00008000
#define SECURESOHO_ST_VENDOR_YOUTUBE   0x0000D000
#define SECURESOHO_ST_VENDOR_CNTV      0x00010000
#define SECURESOHO_ST_VENDOR_VOOLETV   0X00006000	/* vooletv online service */
int securesoho_get_current_server_type(int *p_type);
int securesoho_set_current_server_type(int type);

/* FIXME. Add this for check online server */  
int securesoho_get_online_server_type(int *p_type);
int securesoho_set_online_server_type(int type);
#endif
