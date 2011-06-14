##############################################################################
# Copyright (C) alphanetworks 2006-- 
# All Rights Reserved -- Company Confidential
# Author:  wills_yin@alphanetworks.com
# util top-level Makefile
##############################################################################

############################# global variables section #######################

LIB_COMMON_UTIL   := libcommon_util
LIB_CONFIG_ACCESS := libconfig_access
LIB_EVENT_PROXY := lib_event_proxy

BIN_AUTOTEST    := autotest
BIN_STRINGTEST    := stringtest
BIN_MKCONFIG    := mkconfig
BIN_BINCOPY     := bincopy
BIN_CONFIGTOOL  := config_tool
BIN_MYLOG_TEST  := mylog_test
BIN_SIG_TEST    := sig_test
BIN_SETXENV     := setxenv
BIN_WIRELESS_PROFILE_TEST     := wireless_profile_access_test

TOP        := $(shell pwd)/../..#
COMPONENT  := util# comment to remove trailing space
LEAF_LIBS  := $(LIB_COMMON_UTIL) $(LIB_CONFIG_ACCESS) $(LIB_EVENT_PROXY)
LEAF_BINS  := $(BIN_STRINGTEST) $(BIN_AUTOTEST) $(BIN_BINCOPY) $(BIN_CONFIGTOOL)  mem_allocate $(BIN_MYLOG_TEST)

LEAF_BINS += $(BIN_WIRELESS_PROFILE_TEST)

include $(TOP)/Target.mk
include $(TOP)/Target.options

LEAF_BINS_$(CONF_BACKTRACE) += $(BIN_SIG_TEST)
LEAF_BINS_$(CONF_SETXENV2) += $(BIN_SETXENV)
LEAF_BINS += $(LEAF_BINS_y)

############################ object rules section ############################

include objects.mk

############################ common rules section ############################

SUBDIRS_$(CONF_SIMPLEINIT) += src/simpleinit
SUBDIRS += $(SUBDIRS_y)

include $(TOP_SCRIPTS_DIR)/rules.mk

LOG4C_SYSLIB_$(CONF_LOG4C) := -llog4c
COMMON_SYSLIBS +=  $(LOG4C_SYSLIB_y)


########################### target section ###################################

LEAF := $(LIB_CONFIG_ACCESS)
$(LEAF)_OBJECTS := $($(LIB_CONFIG_ACCESS)_OBJECTS)
$(BUILD_LIB_CMD)

LEAF := $(LIB_EVENT_PROXY)
$(LEAF)_OBJECTS := $($(LIB_EVENT_PROXY)_OBJECTS)
$(BUILD_LIB_CMD)


LEAF := $(LIB_COMMON_UTIL)
$(LEAF)_OBJECTS := $($(LIB_COMMON_UTIL)_OBJECTS)
$(BUILD_LIB_CMD)

LEAF := autotest
$(LEAF)_OBJECTS := $(AUTOTEST_OBJ)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := -lpthread -lm -lmxml
$(BUILD_BIN_CMD)

LEAF := stringtest
$(LEAF)_OBJECTS := $(STRINGTEST_OBJ)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS) -lpthread -lm -lmxml -lglib-2.0
$(BUILD_BIN_CMD)

LEAF := mkconfig
$(LEAF)_OBJECTS := $(MKCONFIG_MAIN_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := -lpthread -lm
$(BUILD_BIN_CMD)

LEAF := bincopy
$(LEAF)_OBJECTS := $(BINCOPY_OBJ)
$(LEAF)_SYSLIBS := -lm
$(BUILD_BIN_CMD)

LEAF := config_tool
$(LEAF)_OBJECTS := $(CONFIG_TOOL_OBJ)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS :=  $(COMMON_SYSLIBS) -lpthread -lm -lmxml
$(BUILD_BIN_CMD)

LEAF := mem_allocate
$(LEAF)_OBJECTS := $(MEM_ALLOCATE_OBJ)
$(LEAF)_DEPLIBS := 
$(LEAF)_SYSLIBS := 
$(BUILD_BIN_CMD)

LEAF := $(BIN_MYLOG_TEST)
$(LEAF)_OBJECTS := $(MYLOG_TEST_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS)
$(BUILD_BIN_CMD)

LEAF := $(BIN_SIG_TEST)
$(LEAF)_OBJECTS := $(SIG_TEST_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS)
$(BUILD_BIN_CMD)

LEAF := $(BIN_SETXENV)
$(LEAF)_OBJECTS := $(SETXENV_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS)
$(BUILD_BIN_CMD)

LEAF := $(BIN_WIRELESS_PROFILE_TEST)
$(LEAF)_OBJECTS := $(WIRELESS_PROFILE_TEST_OBJS)
$(LEAF)_DEPLIBS := libcommon_util.a 
$(LEAF)_SYSLIBS := $(COMMON_SYSLIBS) -lmxml
$(BUILD_BIN_CMD)


########################### instal section ###################################

install: 
	@echo "Installing the config_tool ..." 
	@mkdir -p $(TOP_INSTALL_ROOTFS_DIR)/sbin
	cp $(BIN_DIR)/config_tool $(TOP_INSTALL_ROOTFS_DIR)/sbin/  
	@echo "Installing the config_tool done"


