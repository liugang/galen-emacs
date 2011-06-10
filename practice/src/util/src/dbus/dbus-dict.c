/*
 * =====================================================================================
 *
 *       Filename:  dbus-dict.c
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  10/25/2009 08:24:06 AM
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
#include <errno.h>
#include <stdlib.h> 
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <dbus/dbus.h>
#include <mydbus.h>

#define MYLOG_CATEGORY_NAME "mydbus"
#include "mylog.h"

static void append_variant(DBusMessageIter *iter, int type, void *val)
{
	DBusMessageIter value;
	char sig[2] = { type, '\0' };

	dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT, sig, &value);

	dbus_message_iter_append_basic(&value, type, val);

	dbus_message_iter_close_container(iter, &value);
}

static void append_array_variant(DBusMessageIter *iter, int type, void *val)
{
	DBusMessageIter variant, array;
	char type_sig[2] = { type, '\0' };
	char array_sig[3] = { DBUS_TYPE_ARRAY, type, '\0' };
	const char ***str_array = val;
	int i;

	dbus_message_iter_open_container(iter, DBUS_TYPE_VARIANT,
						array_sig, &variant);

	dbus_message_iter_open_container(&variant, DBUS_TYPE_ARRAY,
						type_sig, &array);

	for (i = 0; (*str_array)[i]; i++)
		dbus_message_iter_append_basic(&array, type,
						&((*str_array)[i]));

	dbus_message_iter_close_container(&variant, &array);

	dbus_message_iter_close_container(iter, &variant);
}

void mydbus_dict_append_entry(DBusMessageIter *dict,
			const char *key, int type, void *val)
{
	DBusMessageIter entry;

	if (type == DBUS_TYPE_STRING) {
		const char *str = *((const char **) val);
		if (str == NULL)
			return;
	}

	dbus_message_iter_open_container(dict, DBUS_TYPE_DICT_ENTRY,
							NULL, &entry);

	dbus_message_iter_append_basic(&entry, DBUS_TYPE_STRING, &key);

	append_variant(&entry, type, val);

	dbus_message_iter_close_container(dict, &entry);
}

void mydbus_dict_append_array(DBusMessageIter *dict, const char *key, int type,
			void *val, int n_elements)
{
	DBusMessageIter entry;

	dbus_message_iter_open_container(dict, DBUS_TYPE_DICT_ENTRY,
						NULL, &entry);

	dbus_message_iter_append_basic(&entry, DBUS_TYPE_STRING, &key);

	append_array_variant(&entry, type, val);

	dbus_message_iter_close_container(dict, &entry);
}

int mydbus_emit_signal_dict(DBusConnection *conn,
					const char *path,
					const char *interface,
					const char *name,
					const char *key,
					int type, void *value)
{
	DBusMessage *signal;
	DBusMessageIter iter;

	signal = dbus_message_new_signal(path, interface, name);

	if (!signal) {
		mylog_error("Unable to allocate new %s.PropertyChanged signal",
				interface);
		return FALSE;
	}

	dbus_message_iter_init_append(signal, &iter);

	dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &key);

	append_variant(&iter, type, value);

	return mydbus_send_message(conn, signal);
}

int mydbus_emit_signal_dict_array(DBusConnection *conn,
					const char *path,
					const char *interface,
					const char *name,
					const char *key,
					int type, void *value)
{
	DBusMessage *signal;
	DBusMessageIter iter;

	signal = dbus_message_new_signal(path, interface, name);

	if (!signal) {
		mylog_error("Unable to allocate new %s.PropertyChanged signal",
				interface);
		return FALSE;
	}

	dbus_message_iter_init_append(signal, &iter);

	dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &key);

	append_array_variant(&iter, type, value);

	return mydbus_send_message(conn, signal);
}
