/* Copyright (C) 2005, Alphanetworks, inc.
 * vim:cindent:ts=8:sw=8:
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <string.h>
#include <dirent.h>
#include <string.h>
#include "securesoho.h"
#include "config_access.h"

/*
 */
int securesoho_get_device_region_data(device_region_data *data)
{
	data->version = bs_config_int_get(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_VERSION");
	data->tv_mode = bs_config_int_get(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_TVMODE");
	data->region = bs_config_int_get(CONF_DEVICE_MP_CONFIG,  "CONF_DEVICE_MP_REGION");
	data->location = bs_config_int_get(CONF_DEVICE_MP_CONFIG,"CONF_DEVICE_MP_LOCATION");
	fprintf(stderr, "get device region data: version: %d(char: %c), tv_mode: %d, region: %d, location: %d\n", 
		data->version, data->version, data->tv_mode, data->region, data->location);
	return 0;
}

/* write from block 0, which is writable. */
int securesoho_set_device_region_data(device_region_data *data)
{
	bs_config_int_set(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_VERSION", data->version);
	bs_config_int_set(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_TVMODE",  data->tv_mode);
	bs_config_int_set(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_REGION",  data->region);
	bs_config_int_set(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_LOCATION", data->location);
	return 0;
}

int securesoho_get_upgrade_resume_info(upgrade_resume_info_t *ri)
{
	memset(ri, 0, sizeof(upgrade_resume_info_t));
	ri->incomplete = securesoho_int_get("FWUP_INCOMPLETE");
	if (1 == ri->incomplete) {	/* there exists an incomplete upgrade */
		securesoho_string_get("FWUP_INCOMPLETE_URL", ri->url);
		ri->bootloader_written = securesoho_int_get("FWUP_BOOTLOADER_WRITTEN");
		ri->kernel1_written = securesoho_int_get("FWUP_KERNEL1_WRITTEN");
		ri->kernel2_written = securesoho_int_get("FWUP_KERNEL2_WRITTEN");
		ri->rootfs1_written = securesoho_int_get("FWUP_ROOTFS1_WRITTEN");
		ri->rootfs2_written = securesoho_int_get("FWUP_ROOTFS2_WRITTEN");
		ri->para0_written = securesoho_int_get("FWUP_PARA0_WRITTEN");
		ri->splash_written = securesoho_int_get("FWUP_SPLASH_WRITTEN");
	}
	return 0;
}

int securesoho_set_upgrade_resume_info(upgrade_resume_info_t *ri)
{
	securesoho_int_set("FWUP_INCOMPLETE", ri->incomplete);

	if (0 == ri->incomplete) {	/* upgrade done, clear all fields about FWUP */
		memset(ri, 0, sizeof(upgrade_resume_info_t));
	}

	securesoho_string_set("FWUP_INCOMPLETE_URL", ri->url);

	securesoho_int_set("FWUP_BOOTLOADER_WRITTEN", ri->bootloader_written);
	securesoho_int_set("FWUP_KERNEL1_WRITTEN", ri->kernel1_written);
	securesoho_int_set("FWUP_KERNEL2_WRITTEN", ri->kernel2_written);
	securesoho_int_set("FWUP_ROOTFS1_WRITTEN", ri->rootfs1_written);
	securesoho_int_set("FWUP_ROOTFS2_WRITTEN", ri->rootfs2_written);
	securesoho_int_set("FWUP_PARA0_WRITTEN", ri->para0_written);
	securesoho_int_set("FWUP_SPLASH_WRITTEN", ri->splash_written);

	return 0;
}
