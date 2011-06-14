/*
 * =====================================================================================
 *
 *       Filename:  udev_util.c
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  10/20/2009 02:31:02 PM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Joshua Lee Joshua_Lee@Alphanetworks.com
 *        Company:  Alpha Networks(Chengdu) Co., LTD Shanghai Branch
 *
 * =====================================================================================
 * vim:cindent:ts=8:sw=8:
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <getopt.h>
#include <syslog.h>
#include <fcntl.h>
#include <sys/select.h>

#include "libudev.h"

#include "securesoho.h"
#include "dbus_service.h"
#include "udev_util.h"

#define MYLOG_CATEGORY_NAME "net_daemon"
#include "mylog.h"

static int get_vid_pid(struct udev_device *device, 
			unsigned int *vid,
			unsigned int  *pid){
	const char *str, *t;
	unsigned int u;
	str = udev_device_get_property_value(device, "PRODUCT");
	if(NULL == str) {
		fprintf(stderr, "Failed to get device's PRODUCT.\n");
		return -1;
	}
	t = str;
	if (!strncmp(t, "0x", 2))
		t += 2;

	if (sscanf(t, "%04x", &u) != 1 || u > 0xFFFFU) {
		fprintf(stderr, "Failed to parse vid on %s.\n", str);
		return -1;
	}

	*vid = u;
	t = strstr(str, "/");
	if (!t){
		fprintf(stderr, "Failed to parse pid on %s.\n", str);
		return -1;

	} 
	t += 1; //skip '/'
	if (sscanf(t, "%04x", &u) != 1 || u > 0xFFFFU) {
		fprintf(stderr, "Failed to parse pid on %s.\n", str);
		return -1;
	}

	*pid = u;

	return 0;
}

static void wlan_driver_add_callback(void *arg){
	struct wireless_driver_ops *wlan = (struct wireless_driver_ops *)arg;
	struct wireless_driver_ops *list = wlan_driver_list_get();

	if (!wlan_driver_contains_of(list, wlan)){
		mylog_debug("%s driver %s is not is the driver list", __func__, wlan->wlan_chip_id);
		wlan_driver_add(list, wlan);
	}
	wlan->wlan_insert();
	wlan->wlan_init();
	nm_dbus_signal_usb_dongle_added(wlan->wlan_name);
}

static void wlan_driver_remove_callback(void *arg){
	struct wireless_driver_ops *wlan = (struct wireless_driver_ops *)arg;
	wlan->wlan_remove();
	wlan_driver_clear();//clear the driver list execpt dummy driver
	nm_dbus_signal_usb_dongle_removed(wlan->wlan_name);
}

static int usb_dongle_detect(struct udev_device *device){
	unsigned vid, pid;
	const char *str;

	get_vid_pid(device, &vid, &pid);
	printf("vid is 0x%04x, pid is 0x%04x\n", vid, pid);

	str = udev_device_get_action(device);
	if (str != NULL){
		if (!strcmp(str, "add")){
			wlan_driver_match(vid, pid, wlan_driver_add_callback);
		} else if (!strcmp(str, "remove")){
			wlan_driver_match(vid, pid, wlan_driver_remove_callback);
		} else {
			return 0;
		}
	}
	return 0;
}

static int usb_dongle_probe_print_list(struct udev_enumerate *enumerate)
{
	struct udev_list_entry *list_entry;
	unsigned int vid, pid;
	int ret = -1;

	udev_list_entry_foreach(list_entry, udev_enumerate_get_list_entry(enumerate)) {
		struct udev_device *device;

		device = udev_device_new_from_syspath(udev_enumerate_get_udev(enumerate),
				udev_list_entry_get_name(list_entry));
		if (device != NULL) {
			mylog_info("device: '%s' (%s)\n",
					udev_device_get_syspath(device),
					udev_device_get_subsystem(device));
			get_vid_pid(device, &vid, &pid);
			ret = wlan_driver_match(vid, pid, wlan_driver_add_callback);
			udev_device_unref(device);
			if (!ret)
				break;
		}
	}
	return ret;
}

static int usb_dongle_probe(struct udev *udev, const char *subsystem)
{
	struct udev_enumerate *udev_enumerate;
	int ret;

	mylog_trace("%s enumerate '%s'", __func__, subsystem == NULL ? "<all>" : subsystem);
	udev_enumerate = udev_enumerate_new(udev);
	if (udev_enumerate == NULL)
		return -1;
	udev_enumerate_add_match_subsystem(udev_enumerate, subsystem);
	udev_enumerate_scan_devices(udev_enumerate);
	ret = usb_dongle_probe_print_list(udev_enumerate);
	udev_enumerate_unref(udev_enumerate);
	return ret;
}

int udev_handle_init(struct udev_handle *uh)
{
	if (!uh)
		return -1;
	uh->udev_ctx = udev_new();
	if (uh->udev_ctx == NULL)
	{
		printf("Error creating libudev context: %m");
		goto error;
	}

	usb_dongle_probe(uh->udev_ctx, "usb");

	uh->udev_monitor = udev_monitor_new_from_netlink(uh->udev_ctx, "kernel");
	if (uh->udev_monitor == NULL)
	{
		printf("no socket\n");
		goto error;
	}
	if (udev_monitor_filter_add_match_subsystem_devtype(uh->udev_monitor, "usb", "usb_device") < 0) {
		printf("filter failed\n");
		goto error;
	}
	if (udev_monitor_enable_receiving(uh->udev_monitor) < 0) {
		printf("bind failed\n");
		goto error;
	}


	return 0;

error:
	if (uh->udev_ctx)
		udev_unref(uh->udev_ctx);
	return -1;
}

int udev_handle_fini(struct udev_handle *uh)
{
	udev_monitor_unref(uh->udev_monitor);
	udev_unref(uh->udev_ctx);
	return 0;
}

int udev_handle_get_fd(struct udev_handle *uh)
{
	int fd = -1;
	if (!uh || !uh->udev_ctx || !uh->udev_monitor)
		return -1;
	fd = udev_monitor_get_fd(uh->udev_monitor);
	return fd;
}



int udev_handle_events(struct udev_handle *uh)
{
	struct udev_device *device;

	if (!uh || !uh->udev_ctx | !uh->udev_monitor)
		return -1;

	device = udev_monitor_receive_device(uh->udev_monitor);
	if (device == NULL)
	{
		return -1;
	}
	usb_dongle_detect(device);
	udev_device_unref(device);
	return 0;
}

