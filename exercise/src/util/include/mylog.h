/* Copyright (C) 2003-2009, Alpha Networks Co., LTD.
 *
 * Author: Joshua Lee <joshua_lee@alphanetworks.com>
 * $Header$
 * vim:cindent:ts=8:sw=8:
 */

#ifndef __MYLOG_H__
#define __MYLOG_H__

#ifdef  __cplusplus     
# define MYLOG_BEGIN_DECLS  extern "C" {
# define MYLOG_END_DECLS    }
#else
# define MYLOG_BEGIN_DECLS
# define MYLOG_END_DECLS
#endif

#include "Target.h"
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <stdarg.h>

#ifdef CONF_LOG4C
#include "log4c.h"
#endif

#ifdef CONF_GLIB_MAINLOOP
#include <glib.h>
#endif 

MYLOG_BEGIN_DECLS

#ifdef CONF_GLIB_MAINLOOP
#define ESC_START 
#define ESC_END 
#define COLOR_FATAL  
#define COLOR_ALERT 
#define COLOR_CRIT 
#define COLOR_ERROR
#define COLOR_WARN
#define COLOR_NOTICE
#define COLOR_INFO 
#define COLOR_DEBUG
#define COLOR_TRACE
#else
#define ESC_START "\033["
#define ESC_END "\033[0m"
#define COLOR_FATAL  "31;40;1m"
#define COLOR_ALERT  "31;40;1m"
#define COLOR_CRIT   "31;40;1m"
#define COLOR_ERROR  "32;40;1m"
#define COLOR_WARN   "33;40;1m"
#define COLOR_NOTICE "34;40;1m"
#define COLOR_INFO   "35;40;1m"
#define COLOR_DEBUG  "36;40;1m"
#define COLOR_TRACE  "37;40;1m"
#endif
#ifndef MYLOG_CATEGORY_NAME
#define MYLOG_CATEGORY_NAME "root"
#endif

#define MYLOG_INLINE __inline
#define MYLOG_API    extern
#define MYLOG_DATA   extern


#ifdef CONF_LOG4C
#define MYLOG_PRIORITY_FATAL LOG4C_PRIORITY_FATAL //fatal
#define MYLOG_PRIORITY_ALERT LOG4C_PRIORITY_ALERT //alert
#define MYLOG_PRIORITY_ERROR  LOG4C_PRIORITY_ERROR //error
#define MYLOG_PRIORITY_CRIT LOG4C_PRIORITY_CRIT //crit
#define MYLOG_PRIORITY_WARN  LOG4C_PRIORITY_WARN //warn
#define MYLOG_PRIORITY_NOTICE  LOG4C_PRIORITY_NOTICE //notice
#define MYLOG_PRIORITY_INFO  LOG4C_PRIORITY_INFO //info
#define MYLOG_PRIORITY_DEBUG LOG4C_PRIORITY_DEBUG //debug
#define MYLOG_PRIORITY_TRACE LOG4C_PRIORITY_TRACE //trace
#else /* compatible with glib log */
#define MYLOG_PRIORITY_FATAL (1<<1) //fatal
#define MYLOG_PRIORITY_ALERT (1<<1) //alert
#define MYLOG_PRIORITY_ERROR (1<<3) //error
#define MYLOG_PRIORITY_CRIT  (1<<3) //crit
#define MYLOG_PRIORITY_WARN  (1<<4) //warn
#define MYLOG_PRIORITY_NOTICE (1<<5) //notice
#define MYLOG_PRIORITY_INFO  (1<<5) //info
#define MYLOG_PRIORITY_DEBUG (1<<7) //debug
#define MYLOG_PRIORITY_TRACE (1<<7) //trace
#endif

#define mylog_fatal(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_FATAL, ESC_START COLOR_FATAL format ESC_END, ##args)
#define mylog_alert(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_ALERT, ESC_START COLOR_ALERT format ESC_END , ##args)
#define mylog_crit(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_CRIT, ESC_START COLOR_CRIT format ESC_END , ##args)
#define mylog_error(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_ERROR, ESC_START COLOR_ERROR format ESC_END , ##args)
#define mylog_warn(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_WARN, ESC_START COLOR_WARN format ESC_END , ##args)
#define mylog_notice(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_NOTICE, ESC_START COLOR_NOTICE format ESC_END , ##args)
#define mylog_info(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_INFO, ESC_START COLOR_INFO format ESC_END , ##args)
#define mylog_debug(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_DEBUG, ESC_START COLOR_DEBUG format ESC_END , ##args)
#define mylog_trace(format, args...) mylog_log(MYLOG_CATEGORY_NAME, MYLOG_PRIORITY_TRACE, ESC_START COLOR_TRACE format ESC_END , ##args)

#ifdef CONF_GLIB_MAINLOOP
static void dummy_log(const gchar *log_domain, GLogLevelFlags log_level, 
                const gchar *message, gpointer user_data)
{
        return;
}
#endif

static MYLOG_INLINE void mylog_set_level(const char *domain, int level)
{
#ifdef CONF_GLIB_MAINLOOP
        unsigned int i = 1 << (~G_LOG_LEVEL_MASK);
        unsigned int max_log_level = level;
        unsigned int log_level = 0;

        for(; i > max_log_level; i--)
        {
                log_level = 1 << i;
                g_log_set_handler(domain, (GLogLevelFlags)log_level, dummy_log, NULL);
        }
#endif
        return;
}

static MYLOG_INLINE int mylog_init(){
#ifdef CONF_LOG4C
	setenv("LOG4C_RCPATH", "/etc", 1);
	return(log4c_init());
#else
	return 0;
#endif
}

static MYLOG_INLINE int mylog_fini(){
#ifdef CONF_LOG4C
	return(log4c_fini());
#else
	return 0;
#endif
}

static MYLOG_INLINE void mylog_msg(char *catName,int a_priority, char *msg){
#if defined(CONF_LOG4C)
	log4c_category_log(log4c_category_get(catName), a_priority, msg);
#elif defined(CONF_GLIB_MAINLOOP)
        g_log(catName, (GLogLevelFlags)a_priority, msg);
#else
	printf(msg);
#endif
}

static MYLOG_INLINE int mylog_setappender(char *catName, char *appName){
#ifdef CONF_LOG4C
	log4c_category_set_appender(log4c_category_get(catName)
			,log4c_appender_get(appName));
	return(0);
#else
	return(0);
#endif                                  
}
static MYLOG_INLINE void mylog_log(char *catName,int a_priority,
		const char* a_format,...){
#if defined(CONF_LOG4C)
	const log4c_category_t* a_category = log4c_category_get(catName);
	if (log4c_category_is_priority_enabled(a_category, a_priority)) {
		va_list va;
		va_start(va, a_format);
		log4c_category_vlog(a_category, a_priority, a_format, va);
		va_end(va);
	}
#elif defined(CONF_GLIB_MAINLOOP)
	va_list va;
	va_start(va, a_format);
	g_logv(catName, (GLogLevelFlags)a_priority, a_format, va);
	va_end(va);
#else
	va_list va;
	va_start(va, a_format);
	vprintf(a_format, va);
	va_end(va);
#endif                                  
}

MYLOG_END_DECLS
#endif
