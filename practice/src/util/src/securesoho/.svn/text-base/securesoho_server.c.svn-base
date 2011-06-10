#include "securesoho.h"

#define SECURESOHO_CFG_TAG  "SECURESOHO_CURRENT_SERVER_TYPE"
#define SECURESOHO_CFG_TAG_ONLINE  "SECURESOHO_ONLINE_SERVER_TYPE"

int securesoho_get_current_server_type(int *p_type)
{
	*p_type = securesoho_int_get(SECURESOHO_CFG_TAG);
	return 0;
		
}

int securesoho_set_current_server_type(int type)
{
	securesoho_int_set(SECURESOHO_CFG_TAG, type);
	return 0;
}

int securesoho_get_online_server_type(int *p_type)
{
	*p_type = securesoho_int_get(SECURESOHO_CFG_TAG_ONLINE);
	return 0;
		
}

int securesoho_set_online_server_type(int type)
{
	securesoho_int_set(SECURESOHO_CFG_TAG_ONLINE, type);
	return 0;
}
