##############################################################################
# Copyright (C) Alpha Networks 2006-2010
# All Rights Reserved -- Company Confidential
# Author:  
#     joshua_lee@alphanetworks.com
# @brief
#     Network manager object rules makefile fragments
##############################################################################

##############################################################################
#
# object rules section 
#

# Comment/uncomment the following line to disable/enable debugging
DEBUG = n

CFLAGS += -O
CFLAGS_$(DEBUG) += -g -D_DEBUG 

WIRELESS_TOOLS_PATH := wireless_tools
VPATH  +=  $(WIRELESS_TOOLS_PATH)
##############################################################################

#
# include search path section
#
INCLUDES += \
	-I$(TOP)                           \
	-I$(TOP_UTILS_DIR)/include         \
	-I$(TOP_UTILS_DIR)/src/securesoho  \
	-I$(NM_DIR)/$(WIRELESS_TOOLS_PATH)	\
	-I$(NM_DIR)                        \
	-I$(NM_DIR)/nm_client

INCLUDES_$(CONF_DBUS) += \
	-I$(SYSLIB_PREFIX)/include/dbus-1.0 \
	-I$(SYSLIB_PREFIX)/lib/dbus-1.0/include

INCLUDES_$(CONF_GLIB) += \
	-I$(SYSLIB_PREFIX)/include/glib-2.0 \
	-I$(SYSLIB_PREFIX)/lib/glib-2.0/include
	
INCLUDES += $(INCLUDES_y)

#
# CFLAGS
#
CFLAGS	+= \
	-DUSE_WIRELESS_TOOL -D__SIMPLEINIT__ \
	-DWLAN_PORT=\"$(WLAN_INTERFACE)\"    \
	-DLAN_PORT=\"$(LAN_INTERFACE)\"      \
	-DPPPOE_IF=\"$(PPPOE_INTERFACE)\"

# shipment option
CFLAGS_$(CONF_DISABLE_DEBUG_OUTPUT) += \
	-D__DISABLE_DEBUG_OUTPUT__

CFLAGS_$(BRIDGE_LAN_WLAN) += \
	-DBRIDGE_LAN_WLAN

CFLAGS 	+= $(CFLAGS_y) 

#
# objects list  section
#

WIRELESS_TOOLS_OBJS := iwlib.o

NETWORK_DAEMON_OBJS	:= \
	network_daemon.o   \
	network_util.o     \
	$(WIRELESS_TOOLS_OBJS) \
	nm_rtnl.o

NETWORK_DAEMON_OBJS_$(CONF_UDEV) += \
	udev_util.o

NETWORK_DAEMON_OBJS_$(CONF_DBUS) += \
	dbus_service.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS) += \
	wireless_setup_tool.o           \
	wireless/wireless_driver.o      \
	wireless/util_wireless.o        \
	wireless/driver_dummy.o         \
	wireless/driver_wext.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RT2870)  += \
	wireless/driver_rt2870.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RT3070)  += \
	wireless/driver_rt3070.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RT3572)  += \
	wireless/driver_rt3572.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RT5370)  += \
	wireless/driver_rt5370.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RT2880)  += \
	wireless/driver_rt2880.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_MARVELL) += \
	wireless/driver_marvell.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_AR5413)  += \
	wireless/driver_atheros.o

NETWORK_DAEMON_OBJS_$(CONF_WIRELESS_RALINK) += \
	wireless/driver_ralink.o

NETWORK_DAEMON_OBJS += $(NETWORK_DAEMON_OBJS_y)

DHCP_CLIENT_OBJS        := \
	dhcp-client.o
PPPOE_CLIENT_OBJS       := \
	pppoe-client.o
LIB_NM_CLIENT_OBJS      := \
	nm_client/nm_client.o 

OBJECTS_ALL	:= \
	$(DHCP_CLIENT_OBJS) \
	$(NETWORK_DAEMON_OBJS) \
	$(LIB_NM_CLIENT_OBJS)

