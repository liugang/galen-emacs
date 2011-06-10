/* Copyright (C) 2006, Alphanetworks, inc.
 * Author:	AlphaNetworks
 *   debug for x86
 */
#include <stdio.h>
#include <stdlib.h>
#include "securesoho.h"

int securesoho_network_init_host(char *host)
{
	return 0;

}

int securesoho_network_init()
{
	return 0;
}

int network_cancel(void)
{
	return 0;
}


int network_restart(int timeout)
{
	return 0;
}

int network_restart2(int timeout)
{
	return 0;
}

int network_sync_restart( void )
{
	return 0;
}

NETWORK_STATE network_get_state()
{
	return NETWORK_STATE_DONE;
}

int network_daemon_inactive(void)
{
	return 0;
}

int network_daemon_active(void)
{
	return 0;
}
