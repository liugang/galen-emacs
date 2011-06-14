#ifndef __NETWORK_UTIL_H
#define __NETWORK_UTIL_H

#include "securesoho.h"

int is_exit_thread( void );
void cfg_stop_dhcp_client( void );
void cfg_network_set_state(NETWORK_STATE value);
NETWORK_STATE cfg_network_get_state(void);
int cfg_network_update_wireless_state(int success);
int cfg_network_init(void);
int cfg_network_restart(int timeout);
int cfg_network_reset(void);
int cfg_network_cancel(void);
NETWORK_STATE cfg_network_check_set_dhcp( void );
int cfg_network_sync_restart( void );
#endif
