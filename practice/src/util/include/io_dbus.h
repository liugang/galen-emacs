/* 
 * Copyright (C) 2010, AlphaNetworks, inc.
 * Brief: 
 *         IO DBUS header file 
 * Author: 
 *         wills_yin@alphanetworks.com
 * Format:
 *         vim:cindent:ts=8:sw=8:
 * History:
 *
 */
#ifndef __IO_DBUS_H__
#define __IO_DBUS_H__

#include "mydbus.h"

/*
 * IO daemon dbus: NAME/PATH/INTERFACE/ERROR
 */
#define IODBUS_NAME            MYDBUS_BASE_NAME  ".IO"
#define IODBUS_PATH            MYDBUS_BASE_PATH "/IO"
#define IODBUS_INTERFACE       MYDBUS_BASE_INTERFACE ".IO"
#define IODBUS_ERROR_INTERFACE IODBUS_INTERFACE ".Error"

/*
 * IO dbus signals
 */
#define IO_SIGNAL_KEYCODE      "IOKeycode"
#define IO_SIGNAL_EVENTS       "IOEvents"


/*
 * IO dbus Event type
 */
enum IOEvent {
	IO_EVENT_NONE = 0,
	IO_EVENT_POWERON,
	IO_EVENT_SYSTEMREADY,
	IO_EVENT_SHUTTINGDOWN,
	IO_EVENT_POWEROFF,
	IO_EVENT_POWEROFF_AV,
	IO_EVENT_USB_STORAGE_PLUGIN,
	IO_EVENT_USB_STORAGE_MOUNT,
	IO_EVENT_USB_STORAGE_UNPLUG,
	IO_EVENT_USB_STORAGE_UMOUNT,
	IO_EVENT_ETHERNET_CABLE_IN,
	IO_EVENT_RM_READKEY,
	IO_EVENT_FWUP,

	IO_EVENT_MAX
};

#endif /*__IO_DBUS_H__ */
