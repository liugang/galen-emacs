##############################################################################
# Copyright (C) Alpha Networks 2006-2010
# All Rights Reserved -- Company Confidential
# @author
#     joshua_lee@alphanetworks.com
# @brief
#     Network daemontop-level Makefile
##############################################################################

##############################################################################
#
# global variables section 
#

NM_DIR        := $(shell pwd)
TOP           := $(shell pwd)/../../..#

LIB_NM_CLIENT := libnm_client

COMPONENT     := misc/dmacfg# 
LEAF_LIBS     := $(LIB_NM_CLIENT)
LEAF_BINS     := network_daemon 
LEAF_BINS_$(CONF_DHCPCD) += dhcp-client
LEAF_BINS_$(CONF_PPPOE)  += pppoe-client

LEAF_BINS   += $(LEAF_BINS_y)

include $(TOP)/Target.mk
include $(TOP)/Target.options

##############################################################################
#
# object rules section 
#

include objects.mk

##############################################################################
#
# common rules section 
#

include $(TOP_SCRIPTS_DIR)/rules.mk

##############################################################################
#
# target section 
#

COMMON_SYSLIBS_$(CONF_DBUS) += -ldbus-1 -lrt
COMMON_SYSLIBS_$(CONF_LOG4C) +=  -llog4c
COMMON_SYSLIBS += $(COMMON_SYSLIBS_y)
  
LEAF := network_daemon 
$(LEAF)_OBJECTS := $(NETWORK_DAEMON_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS)  -lpthread -lmxml -ludev -lm 
$(BUILD_BIN_CMD)

LEAF := dhcp-client
$(LEAF)_OBJECTS := $(DHCP_CLIENT_OBJS)
$(LEAF)_DEPLIBS := libconfig_access.a libcommon_util.a
$(BUILD_BIN_CMD)

LEAF := pppoe-client
$(LEAF)_OBJECTS := $(PPPOE_CLIENT_OBJS)
$(LEAF)_DEPLIBS := libconfig_access.a libcommon_util.a
$(BUILD_BIN_CMD)

LEAF := $(LIB_NM_CLIENT)
$(LEAF)_OBJECTS := $(LIB_NM_CLIENT_OBJS)
$(BUILD_LIB_CMD)

##############################################################################
#
# install section 
#

install:
	mkdir -p $(TOP_INSTALL_ROOTFS_DIR)/usr/bin
	mkdir -p $(TOP_INSTALL_ROOTFS_DIR)/bin
	mkdir -p $(TOP_INSTALL_ROOTFS_DIR)/etc/sysconfig/config
ifeq (x$(CONF_DHCPCD),xy)
	cp -f $(BIN_DIR)/dhcp-client $(TOP_INSTALL_ROOTFS_DIR)/usr/bin
	chmod +x $(TOP_INSTALL_ROOTFS_DIR)/usr/bin/dhcp-client
endif
ifeq (x$(CONF_PPPOE),xy)
	cp -f $(BIN_DIR)/pppoe-client $(TOP_INSTALL_ROOTFS_DIR)/usr/bin
	chmod +x $(TOP_INSTALL_ROOTFS_DIR)/usr/bin/pppoe-client
endif
	cp -f $(BIN_DIR)/network_daemon $(TOP_INSTALL_ROOTFS_DIR)/usr/bin
	chmod +x $(TOP_INSTALL_ROOTFS_DIR)/usr/bin/network_daemon

