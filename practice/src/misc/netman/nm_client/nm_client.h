/*
 * =====================================================================================
 *
 *       Filename:  nm_client.h
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  10/23/2009 06:30:43 PM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Joshua Lee (http://www.alphanetworks.com), Joshua_Lee@Alphanetworks.com
 *        Company:  Alpha Networks(Chengdu) Co., LTD Shanghai Branch
 *
 * =====================================================================================
 */
#ifndef _NM_CLIENT_H_
#define _NM_CLIENT_H_

#include "securesoho.h"
#include "mydbus.h"

enum{
	NM_SCAN_IDLE,
	NM_SCAN_SCANNING,
	NM_SCAN_COMPLETE,
};

int nm_client_signal_setup(DBusConnection *conn);
int nm_client_register_interface(DBusConnection *conn);

int nm_client_scan(const char *iface);
int nm_client_get_scan_results(struct SITE_OBJ **list, int *count);
int nm_client_scan_results_init(WIRELESS_TYPE type, int wps_support);
int nm_client_scan_results_destroy(void);
	
int nm_client_is_wlan_available(void);
int nm_client_get_scan_status(void);
int nm_client_start_notification(void);

int nm_client_get_wps_status(void);
int nm_client_gen_wpspin(char *pin, int size);

/* get wireless signal */
int nm_client_get_wireless_signal(void);
#endif

