#ifndef _UDEV_UTIL_H
#define _UDEV_UTIL_H

#include "libudev.h"

struct udev_handle {
	struct udev *udev_ctx;
	struct udev_monitor *udev_monitor;
};

int udev_handle_init(struct udev_handle *uh);
int udev_handle_fini(struct udev_handle *uh);
int udev_handle_get_fd(struct udev_handle *uh);
int udev_handle_events(struct udev_handle *uh);
#endif
