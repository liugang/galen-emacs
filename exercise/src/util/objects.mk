##############################################################################
# Copyright (C) alphanetworks 2006-- 
# All Rights Reserved -- Company Confidential
# Author:  wills_yin@alphanetworks.com
# util object rules makefile fragments
# Note:
#     This file would update all the CFLAGS, VPATH, INCLUDES and OBJECTS_ALL 
# based on all the features. 
#     Please use CFLAGS += because part of CFLAGS may be defined elsewhere.
##############################################################################

############################# global variables section #######################

# Comment/uncomment the following line to disable/enable debugging
DEBUG = n
CFLAGS +=
CFLAGS_$(DEBUG) += -g -D_DEBUG 

############################# base rules section #############################

#
# VPATH section
#
VPATH += \
	./src                 \
	./src/apiwrapper      \
	./src/httpAPI         \
	./src/string          \
	./src/intl            \
	./src/mpl             \
	./src/ping            \
	./src/securesoho      \
	./src/event_proxy     \
	./src/voice_tips      \
	./src/hw_cursor       \
	./src/os/linux        \
	./src/sha1            \
	./src/locale          \
	./src/wireless        \
	./src/debug           \
	./src/mydebug         \
	./src/mylog           \
	./src/local_browse_thumb \
	./src/dlna		 \
	./src/log_system	\
	./src/local_file_filter \
	./src/cobject         \
	./src/remux \
	./src/xenv2           \
	./src/dbus             \
	./src/crypto_wrapper

#
# include search path section
#
INCLUDES += \
	-I./include         \
	-I./src/thumbnailer \
	-I./src/transcode   \
	-I./src/image       \
	-I./src/mpl         \
	-I $(TOP)           \
	-include $(TOP)/Target.h

INCLUDES +=  \
	-I$(SYSLIB_PREFIX)/include              \
	-I$(SYSLIB_PREFIX)/include/freetype2    \
	-I$(TOP)/src/mediaplayer/include 	    \

INCLUDES_$(CONF_DBUS) += \
	-I$(SYSLIB_PREFIX)/include/dbus-1.0 \
	-I$(SYSLIB_PREFIX)/lib/dbus-1.0/include
	
INCLUDES_$(CONF_GLIB) += \
	-I$(SYSLIB_PREFIX)/include/glib-2.0 \
	-I$(SYSLIB_PREFIX)/lib/glib-2.0/include
#
# CFLAGS section
#
CFLAGS += \
	-DSIMPLE_WRITE_CACHE                     \
	-DDATA_SOURCE_FRAMEWORK                  \
	-DDMA_UPNP_SUPPORT_LOOPBACK              \
	-DWLAN_PORT=\"$(WLAN_INTERFACE)\"        \
	-DLAN_PORT=\"$(LAN_INTERFACE)\"          \
	-DHAVE_STRERROR -DHAVE_STRDUP            \
	-DBSD_COMP -DHAVE_GETTIMEOFDAY           \
	-D__$(CURRENT_SET)__										          

ifneq (x$(CONF_CONFIG_MTD_PARTITION), x)
CFLAGS += 	-DTARGET_FILE=\"$(CONF_CONFIG_MTD_PARTITION)\" 
endif
ifneq (x$(CONF_CONFIG_MTD_PARTITION_BACKUP), x)
CFLAGS += 	-DTARGET_FILE2=\"$(CONF_CONFIG_MTD_PARTITION_BACKUP)\" 
endif

#
# objects list  section
#
ifeq ($(ARCH),i386)
SECURESOHO_NETWORK_OBJS = securesoho_network2_x86.o
else
SECURESOHO_NETWORK_OBJS = securesoho_network2.o
endif

CONFIG_ACCESS_OBJS := \
	securesoho.o \
	config_access.o

#
# objects for flash_op
#
FLASH_OP_OBJS := flash_op_dummy.o

ifeq ($(CONF_USE_C2_SDK), y)
FLASH_OP_OBJS := flash_op_c2.o
endif

#
# objects for securesoho
#
SECURESOHO_OBJS := \
	fwup_postprocess.o           \
	securesoho_config.o          \
	securesoho_firmware.o        \
	securesoho_server.o          \
	securesoho_udn.o             \
	flash_monitor.o              \
	$(SECURESOHO_NETWORK_OBJS)   \
	securesoho_nic_helper.o      \
	wireless_profile_access.o    \
	securesoho.o                 \
	ILibLinkedList.o             \
	$(FLASH_OP_OBJS)

RPC_UTILITY_OBJS :=\
	dma_rpc_utility.o            

OS_OBJS := \
	directory.o \
	thread.o    \
	malloc.o    \
	network.o

DEBUG_OBJS := \
		debug_io.o

TRANSCODE_OBJS := \
		transcode_core.o \
		transcoding.o

MKCONFIG_API_OBJS := mkconfig_api.o
FLAC_API_OBJS := 	\
	lb_thumb_plugin_flac.o    \
	album_art_flac.o

APE_API_OBJS :=	\
	ape_parser.o	\
	ape_decoder.o

MKV_API_OBJS := 	\
	matroska_demux.o \
	album_art_mkv.o	 \
	lb_thumb_plugin_mkv.o

MP4_API_OBJS :=		\
	album_art_mp4.o	\
	lb_thumb_plugin_mp4.o

WMA_API_OBJS :=		\
	album_art_wma.o	\
	lb_thumb_plugin_wma.o

CHAPTER_TOOL_OBJS :=	\
	chapter_tool.o

EVENT_PROXY_OBJS := \
	event_proxy.o         \
	event_transfer_api.o  \
	fifo_event_process.o  \
	list_event_process.o 

IFO_OBJS := \
	ifo_parser.o        \
	ifo_helper.o        \
	BD_Parser.o

LOG_CLIENT_OBJS:= \
	log_print_client.o	\
	log_plugin_console.o	   \
	log_plugin_file.o	\
	log_control_list.o	

MYLOG_TEST_OBJS:= \
	mylog_test.o	

SIG_TEST_OBJS:= \
	sig_test.o	

SETXENV_OBJS:= \
	setxenv2.o

BASE64_OBJS:= \
	crypto_base64.o

WIRELESS_PROFILE_TEST_OBJS := \
	wireless_profile_access_test.o

MAKE_IMAGE_OBJS := \
	./image/make_image.o

#don't add the files directly into OBJECTS.Please group it by components
OBJECTS := \
	ping.o               \
	minigettext.o        \
	minigettextP.o       \
	llist.o              \
	options.o            \
	url.o                \
	util.o               \
	locale_util.o        \
	MyString.o           \
	Random.o             \
	TimeUtil.o           \
	AudioFormat.o        \
	ImageFormat.o        \
	VideoFormat.o        \
	album_art.o          \
	libmp4.o             \
	MPL.o                \
	mpegts.o             \
	mpeg_hdr.o           \
	ts_parse.o           \
	parser.o             \
	sha1_v2.o            \
	mydebug.o            \
	dlna.o               \
	string_codeset.o     \
	remux_file.o         \
	cobject.o            \
	video_playback_info.o\
	$(MAKE_IMAGE_OBJS)   \
	$(OS_OBJS)           \
	$(MKCONFIG_API_OBJS) \
	$(SECURESOHO_OBJS)   \
	$(EVENT_PROXY_OBJS)  \
	$(DEBUG_OBJS)

ifeq ($(CONF_FORMAT_FLV), y) 
OBJECTS += \
	flv_demuxer.o 
endif 

SAP_OBJS := \
	sap.o \
	sdp.o

LB_THUMB_OBJS := \
	./thumbnailer/thumbnailer.o \
	./thumbnailer/thumbnailer_bmp.o \
	./transcode/thumbnail.o     \
	lb_thumb.o                  \
	lb_thumb_plugin_mp3.o       \
	lb_thumb_plugin_jpg.o       \
	lb_thumb_plugin_tiff.o      \
	lb_thumb_plugin_bmp.o       \
	lb_thumb_clean_cache.o

LB_PLAYLIST_OBJS := \
	PlayListParser.o \
	PLM3UParser.o \
	PLPLSParser.o \
	PLWPLParser.o \
	PLASXParser.o \
	PLCUEParser.o \
	PLREFParser.o

LB_DOWNLOADLIST_OBJS := \
	DownloadListParser.o \
	DLDLSParser.o

LOCAL_FILE_FILTER_OBJS := \
	local_file_filter.o

SMB_CONF_OP_OBJS := \
	smbconf_op.o

SQLITE_OBJS := \
	sqlite3_wrapper.o \
	sqlite_terminator.o

SUBTITLE_OBJS := \
	subtitle/subtitle_api.o	   \
	subtitle/subtitle_srt.o	   \
	subtitle/subtitle_smi.o    \
	subtitle/subtitle_ssa.o    \
	subtitle/subtitle_ass.o    \
	subtitle/subtitle_subviewer.o    \
	subtitle/subtitle_subviewer2.o   \
	subtitle/subtitle_pgs.o    \
	subtitle/subtitle_txt.o	   \
        subtitle/subtitle_get_language.o    \
        subtitle/shooter_subtitle_download.o

OBJECTS_$(CONF_AUDIO_LRC_OPTION) += subtitle/subtitle_lrc.o
OBJECTS_$(CONF_SAMBA) += $(SMB_CONF_OP_OBJS)
OBJECTS_$(CONF_DMA_SUBTITLE)	+= $(SUBTITLE_OBJS)
OBJECTS_$(CONF_DMA_SUBTITLE_VOBSUB)	+= \
	subtitle/subtitle_vobsub.o 
ifeq (x$(CONF_DIVX_SUBTITLE_SUPPORT), xy)
OBJECTS_$(CONF_DMA_SUBTITLE) += subtitle_xsub.o
endif
	
WCN_OBJS := wcn_led_api.o
GPIO_API_OBJS := gpio_api.o

OBJECTS_$(CONF_BACKTRACE) += sig_handler.o 
OBJECTS_$(CONF_OSD_VOICE_TIPS) += voice_tips.o
OBJECTS_$(CONF_OSD_HW_CURSOR) += bmpreader.o hw_cursor.o
OBJECTS_$(CONF_SQLITE) += $(SQLITE_OBJS)
OBJECTS_$(CONF_SPU_API) += spu_draw_api.o
OBJECTS_$(CONF_ENABLE_BLURAY_NAVIGATION) += PLMPLSParser.o

OBJECTS_$(CONF_THUMB_GIF) += lb_thumb_plugin_png.o
OBJECTS_$(CONF_THUMB_PNG) += lb_thumb_plugin_gif.o

OBJECTS_$(CONF_XENV2) +=  	\
	fastsha256.o		\
	rwbin.o     		\
	xenv2.o 		\
	securesoho_xenv.o

OBJECTS_$(CONF_DBUS) +=  	\
	mydbus.o                \
	dbus-dict.o


AUTOTEST_OBJ=autotest.o
STRINGTEST_OBJ=stringtest.o
MKCONFIG_MAIN_OBJS=mkconfig-main.o
BINCOPY_OBJ=bincopy.o
CONFIG_TOOL_OBJ=config_tool.o
MEM_ALLOCATE_OBJ=mem_allocate.o

#libiconv
LIBS_ICONV		:= -liconv
COMMON_SYSLIBS_$(CONF_LIBICONV) += $(LIBS_ICONV)

COMMON_SYSLIBS += $(COMMON_SYSLIBS_y)
############################# feature rules section #############################
#
# INCLUDES
#

#
# VPATH
#
VPATH_$(CONF_DMA_EPG)        += ./src/sap 
VPATH_$(CONF_SAVEMETHOD_CRC) += ./src/mkconfig
VPATH_$(CONF_SIMPLEINIT)     += ./src/simpleinit
VPATH                        += ./src/region ./src/rpc
VPATH_$(CONF_WCN)            += ./src/wcn_leds
VPATH_$(CONF_DMA_GPIO_API)   += ./src/gpio_api
VPATH_$(CONF_DMA_LOCAL_BROWSE_THUMB)   += ./src/local_browse_thumb
VPATH_$(CONF_LOCAL_PLAYLIST)   += ./src/playlist
VPATH_$(CONF_LOCAL_DOWNLOADLIST)   += ./src/downloadlist
VPATH_$(CONF_ISO_FOLDER_SUPPORTED) +=  ./src/ifo_parser
VPATH_$(CONF_SQLITE) += ./src/db
VPATH_$(CONF_BACKTRACE) 	+= ./src/sighandler
VPATH_$(CONF_SPU_API) 	+= ./src/spu_api
VPATH_$(CONF_DMA_SUBTITLE) 	+= ./src/subtitle
VPATH_$(CONF_MKV_CHAPTER_SUPPORT)	+= ./src/chapter_tool
#
# CFLAGS
#
CFLAGS_$(CONF_SAVEMETHOD_CRC) += -D__USE_CRC_CONFIG__ 
CFLAGS_$(CONF_SIMPLEINIT)     += -D__SIMPLEINIT__
CFLAGS_$(CONF_DMA_DTCP)       += -D__DTCP__
CFLAGS_$(CONF_EVENT_PROXY)    += -DFLAVOUR_EVENT_PROXY
CFLAGS_$(BRIDGE_LAN_WLAN)     += -DBRIDGE_LAN_WLAN
CFLAGS_$(USE_WIRELESS_TOOL)   += -DUSE_WIRELESS_TOOL

# shipment option
CFLAGS_$(CONF_DISABLE_DEBUG_OUTPUT) += -D__DISABLE_DEBUG_OUTPUT__

#
# OBJECTS
#
OBJECTS_y += os_net_util.o os_misc_util.o
OBJECTS_$(CONF_DMA_EPG)               += $(SAP_OBJS)
OBJECTS                               += $(CARD_READER_OBJS)
OBJECTS_$(CONF_WCN)                   += $(WCN_OBJS)
OBJECTS_$(CONF_DMA_GPIO_API)          += $(GPIO_API_OBJS)
OBJECTS_$(CONF_DMA_LOCAL_BROWSE_THUMB)   += $(LB_THUMB_OBJS)
OBJECTS_$(CONF_MUSIC_FLAC)          += $(FLAC_API_OBJS)
OBJECTS_$(CONF_MUSIC_APE)          += $(APE_API_OBJS)
OBJECTS_$(CONF_THUMB_MP4)          += $(MP4_API_OBJS)
OBJECTS_$(CONF_THUMB_WMA)          += $(WMA_API_OBJS)
OBJECTS_$(CONF_FORMAT_MATROSKA)    += $(MKV_API_OBJS)
OBJECTS_$(CONF_LOCAL_PLAYLIST)   += $(LB_PLAYLIST_OBJS)
OBJECTS_$(CONF_LOCAL_DOWNLOADLIST)   += $(LB_DOWNLOADLIST_OBJS)
OBJECTS_$(CONF_ISO_FOLDER_SUPPORTED) += $(IFO_OBJS)
OBJECTS_$(CONF_LIBICONV) += $(TRANSCODE_OBJS)
OBJECTS_$(CONF_RPC_UTILITY)) += $(RPC_UTILITY_OBJS)
OBJECTS += $(LOG_CLIENT_OBJS)
OBJECTS += $(LOCAL_FILE_FILTER_OBJS)
OBJECTS += $(MYLOG_TEST_OBJS)
OBJECTS_$(CONF_BACKTRACE) += $(SIG_TEST_OBJS)
OBJECTS_$(CONF_OPENSSL)   += $(BASE64_OBJS)
OBJECTS_$(CONF_MKV_CHAPTER_SUPPORT)		+= $(CHAPTER_TOOL_OBJS)
############################# target rules section ###########################

CFLAGS      += $(CFLAGS_y)
VPATH       += $(VPATH_y)
INCLUDES    += $(INCLUDES_y)
OBJECTS     += $(OBJECTS_y)

$(LIB_CONFIG_ACCESS)_OBJECTS := $(CONFIG_ACCESS_OBJS)
$(LIB_EVENT_PROXY)_OBJECTS := $(EVENT_PROXY_OBJS)
$(LIB_COMMON_UTIL)_OBJECTS := $(OBJECTS) $(CONFIG_ACCESS_OBJS) $(EVENT_PROXY_OBJS)

OBJECTS_ALL  := $(OBJECTS) $(AUTOTEST_OBJ) $(MKCONFIG_MAIN_OBJS) $(CONFIG_CMD_OBJ) 	$(CONFIG_TOOL_OBJ) $(CONFIG_ACCESS_OBJS) $(BINCOPY_OBJ) $(MEM_ALLOCATE_OBJ)  $(WIRELESS_PROFILE_TEST_OBJS)
