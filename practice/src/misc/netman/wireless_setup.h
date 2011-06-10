#ifndef __WIRELESS_SETUP_H
#define __WIRELESS_SETUP_H

#include "securesoho.h"

#define WIRELESS_ASSOCIATE_TIMEOUT       10

int wireless_setup_get_link_status(char *lif, int len);
int wireless_setup_set_profile(SecureSOHO_Wireless *conf, int timeout);

#endif
