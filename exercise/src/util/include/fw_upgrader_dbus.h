/* 
 * Copyright (C) 2010, AlphaNetworks, inc.
 * Brief: 
 *         firmware upgrading D-BUS header file 
 * Author: 
 *         john_ge@alphanetworks.com
 * Format:
 *         vim:cindent:ts=8:sw=8:
 * History:
 *
 */
#ifndef FW_UPGRADER_DBUS_H
#define FW_UPGRADER_DBUS_H

#include <mydbus.h>

/*
 * fw_ugprader daemon dbus: NAME/PATH/INTERFACE/ERROR
 */

#define FIRMWARE_DBUS_NAME				(MYDBUS_BASE_NAME ".firmware")
#define FIRMWARE_DBUS_PATH				(MYDBUS_BASE_PATH "/firmware")
#define FIRMWARE_DBUS_INTERFACE			(MYDBUS_BASE_INTERFACE ".firmware.upgrading")
#define FIRMWARE_DBUS_ERROR_INTERFACE	(FIRMWARE_DBUS_INTERFACE ".Error")


#endif /* FW_UPGRADER_DBUS_H */
