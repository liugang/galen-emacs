#ifndef __NM_DBUS_SERVICE_H
#define __NM_DBUS_SERVICE_H

#include <dbus/dbus.h>
#include "libudev.h"
#include "mydbus.h"
#ifdef CONF_IO_DAEMON
#include "io_dbus.h"
#endif

int nm_dbus_srv_init(void);
void nm_dbus_srv_close(void);
void nm_dbus_srv_handler(void);
int nm_dbus_srv_get_fd(void);
DBusConnection *nm_srv_get_connection(void);



#ifdef CONF_IO_DAEMON
int nm_dbus_signal_ioevents(enum IOEvent event);
#endif

int nm_dbus_signal_status(const char *);
int nm_dbus_signal_device_event(struct udev_device *device);
int nm_dbus_signal_scan_results(const char *);
int nm_dbus_signal_usb_dongle_added(const char *iface);
int nm_dbus_signal_usb_dongle_removed(const char *iface);
int nm_dbus_signal_wireless_status(const char *status);
int nm_dbus_signal_wps_status(const int status);
int nm_dbus_signal_device_status(const char *iface, int status);
int nm_dbus_signal_network_status(int status);
int nm_dbus_signal_wireless_signal(int signal);
#endif
