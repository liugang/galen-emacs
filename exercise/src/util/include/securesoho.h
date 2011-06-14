/* 
 * Copyright (C) 2006, Alphanetworks, inc.
 * History:
 *    Split the header file into securesoho_wireless.h securesoho_config.h
 *    securesoho_network.h secureoho_firmware.h
 * vim:cindent:ts=8:sw=8
 */
#ifndef __SECURESOHO_H__
#define __SECURESOHO_H__

#include "Target.h"
#include "dma_common.h"
#include "log_print_client.h"
#define MSG_ERROR		0x00000001
#define MSG_DMACFG		0x00000002
#define MSG_SEURESOHO_WIRELESS	0x00000004

/**
 * The return code.
 */
/* Correct */
#define ESEC_NONE              0
/* error at open or copy files or other IO transcation */
#define ESEC_IO_ERROR          1
/* The requested value is not found in the database */
#define ESEC_VALUE_NOT_FOUND   2

#define SYS_MAX_PATH_LENGTH      1024
#define MAX_LINE_LENGTH          512
#define MAX_LLENGTH		 512

#define    CONFIG_PICTURE_FIT      "PICFIT"
#define    CONFIG_VIDEO_FIT      "VIDEOFIT"
#define    CONFIG_MUSIC_PLAYMODE   "CONFIG_MUSIC_PLAYMODE"
#define    CONFIG_VIDEO_PLAYMODE   "CONFIG_VIDEO_PLAYMODE"
#define    CONFIG_BD_PLAYLIST   "CONFIG_BD_PLAYLIST"
#define    SLIDESHOW_SHUFFLE       "SLIDESHOW_SHUFFLE"

extern void udn16_maker(const char *token, char *udn_container);
extern void udn_maker(const char *token, char *udn_container);
extern void str_dec(char *s);
extern void str_inc(char *s);

extern int  ip_addr_exist(char *ifname);

extern int  decode_str(char *buf, char *key, char *value, const char *delim);
extern int  my_system(char *cmd, ...);
extern int is_file_exist(const char *name);
extern int  exist(const char *filename);
extern int touch(const char *name);
extern int  ps_exist(const char *pidfile);
extern int  do_cmd(char *fmt, ...);
extern int  do_cmd_quiet(char *fmt, ...);
extern void do_cmd_bg(char *fmt, ...);
extern int  set_pid_to_file(const char *pidfile);
extern int  kill_pidfile(char *file, int signal);
extern int  kill_pidfile_nowait(char *file, int signal);

extern int  do_mount(char *source, char *target, char *fstype, long flags,
		     int fakeIt, int mountall, int loop);
extern int  do_mkdir(char *dirname);
extern int  do_mkdirhier(char *dirname);
extern int  do_mknod(char *name, char *type, char *major, char *minor);
extern long getul(char *num, int lower, int upper);
extern int  do_umount (char *target);
extern char *do_get_mount_source_by_target(char *target);

/*
 * This API is used to do factory default
 *   1) replace the config with factory default ('/conf_src/config_factory_default'->'/conf/config')
 *   2) remove wireless profile settings ('/conf/wireless_profile.xml')
 */
int  securesoho_factory_default(void);

int securesoho_mass_product_mode(void);

void securesoho_wcn_status_led(int ok);
int  securesoho_save_cert_dir(char *i_dir, char *o_filename);

#ifdef __USE_CRC_CONFIG__
#define CONF_BLOCK 0
#define CONF_NONBLOCK 1
#define MY_SYSTEM my_system
#endif				// __USE_CRC_CONFIG__

extern  int     securesoho_copy(const char *src, const char *dst);
extern  int     securesoho_remove(char *file);

extern	int	    DMA_ConfLoad(void);
extern	void	DMA_ConfSave(void);
void PRINTDEBUGMSG(unsigned int level, const char* fmt, ...);

int securesoho_dma_ex_lock();
int securesoho_dma_ex_unlock();
int securesoho_dma_ex_wait_lock();

#include "securesoho_config.h"
#include "securesoho_firmware.h"
#include "securesoho_network2.h"
#include "securesoho_nic_helper.h"
#include "securesoho_wireless.h"
#include "securesoho_server.h"
#include "securesoho_xenv.h"
#include "flash_monitor.h"
#include "config_access.h"
#include "securesoho_firmware.h"
#include "osd_key_event.h"
#include "ILibLinkedList.h"
#endif				// __SECURESOHO_H__
