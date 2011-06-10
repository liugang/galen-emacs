#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>

#include <dbus/dbus.h>


#include "mydbus.h"
#include "securesoho.h"
#include "dbus_service.h"
#include "network_util.h"

#define MYLOG_CATEGORY_NAME "net_daemon"
#include "mylog.h"

#define VERSION "0.1"

static DBusConnection *connection;

static struct nm_scan_context{
	struct SITE_OBJ *list;
	int count;
	int requests; //record the request
	pthread_t pid;
	pthread_mutex_t lock;
}nm_scan_ctx;

static void *
scan_thread_body(void *data)
{
	const char *str="scan complete";
	while(1) {
		if (nm_scan_ctx.requests){
			pthread_mutex_lock(&nm_scan_ctx.lock);
				mylog_trace("try to get the site survey result");
			if (nm_scan_ctx.list)
				securesoho_free_site_obj(nm_scan_ctx.list);
			nm_scan_ctx.list = securesoho_site_survey(&nm_scan_ctx.count, WIRELESS_BOTH);
			pthread_mutex_unlock(&nm_scan_ctx.lock);
			mylog_trace("get site survey result: %d", nm_scan_ctx.count);
			nm_dbus_signal_scan_results(str);
			nm_scan_ctx.requests = 0;
		}else{
			sleep(1);
		}	
	}
}
static int
process_wps_profile(void *data)
{
	SecureSOHO_Wireless sw;
	struct wireless_driver_ops *wlan = securesoho_get_wireless_card();
	
	securesoho_wireless_get(&sw);

	if(NULL == wlan){
		mylog_error("No wireless card available");
		goto out;
	}

	if ((sw.wps_mode != WIRELESS_WPS_PBC) && 
			(sw.wps_mode != WIRELESS_WPS_PIN) &&
			(sw.wps_mode != WIRELESS_WIFI_DIRECT_WPS_PBC)){
		mylog_error("Wrong WPS mode %d", sw.wps_mode);
		goto out;
	}

	wlan->wlan_set_profile_wep(&sw, 0);
	return 0;

out:
	return -1;
}

static int
attach_scan_results(const char *iface, DBusMessageIter *iter)
{
	DBusMessageIter array, dict;
	const char *property;
	int value;
	struct SITE_OBJ *site;
	int i = 0;

	dbus_message_iter_open_container(iter, DBUS_TYPE_ARRAY,
			DBUS_TYPE_ARRAY_AS_STRING
			DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
			DBUS_TYPE_STRING_AS_STRING
			DBUS_TYPE_VARIANT_AS_STRING
			DBUS_DICT_ENTRY_END_CHAR_AS_STRING,
			&array);

	pthread_mutex_lock(&nm_scan_ctx.lock);
	for (site = nm_scan_ctx.list; site; site=site->next) {
		dbus_message_iter_open_container(&array, DBUS_TYPE_ARRAY,
				DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
				DBUS_TYPE_STRING_AS_STRING
				DBUS_TYPE_VARIANT_AS_STRING
				DBUS_DICT_ENTRY_END_CHAR_AS_STRING,
				&dict);

		property = site->essid;
		mydbus_dict_append_entry(&dict, "ESSID", DBUS_TYPE_STRING, &property);

		property = site->bssid;
		mydbus_dict_append_entry(&dict, "BSSID", DBUS_TYPE_STRING, &property);

		value = site->encrypt_type;
		mydbus_dict_append_entry(&dict, "EncryptType", DBUS_TYPE_INT32, &value);

		value = site->auth_mode;
		mydbus_dict_append_entry(&dict, "AuthMode", DBUS_TYPE_INT32, &value);

		value = site->signal_percentage;
		mydbus_dict_append_entry(&dict, "SignalStrength", DBUS_TYPE_INT32, &value);

		value = site->channel;
		mydbus_dict_append_entry(&dict, "Chananel", DBUS_TYPE_INT32, &value);

		value = site->wireless_mode;
		mydbus_dict_append_entry(&dict, "WirelessMode", DBUS_TYPE_INT32, &value);

		value = site->wireless_type;
		mydbus_dict_append_entry(&dict, "WirelessType", DBUS_TYPE_INT32, &value);

		value = site->support_wps;
		mydbus_dict_append_entry(&dict, "SupportWPS", DBUS_TYPE_INT32, &value);

		dbus_message_iter_close_container(&array, &dict);
	}
	pthread_mutex_unlock(&nm_scan_ctx.lock);

	dbus_message_iter_close_container(iter, &array);
	return i;
}

static DBusMessage *scan_results(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	DBusMessage *reply;
	DBusMessageIter args;
	DBusError err;
	char *iface;

	dbus_error_init(&err);
	if (!dbus_message_get_args(msg, &err, DBUS_TYPE_STRING, &iface,
				DBUS_TYPE_INVALID))
		return mydbus_create_error(msg, 
				NM_ERROR_INTERFACE ".InvalidArguments",
				"Invalid arguments in method call");	
	if (!nm_scan_ctx.list)
		return mydbus_create_error(msg, 
				NM_ERROR_INTERFACE ".NoAccessPoints",
				"No Access Points");	
			
	reply = dbus_message_new_method_return(msg);
	dbus_message_iter_init_append(reply, &args);
	attach_scan_results(iface, &args);
	return reply;
}

static DBusMessage *scan(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	nm_scan_ctx.requests++;
	return dbus_message_new_method_return(msg);
}

static DBusMessage *incoming_client(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	struct wireless_driver_ops *wlan;

	mylog_trace("start %s", __func__);
	if ((wlan = wlan_driver_check()) != NULL)
		nm_dbus_signal_usb_dongle_added(wlan->wlan_name);
	else
		nm_dbus_signal_usb_dongle_removed("dummy");

	return dbus_message_new_method_return(msg);
}

static DBusMessage *generate_wps_pin(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	DBusMessage *reply;
	DBusMessageIter args;
	struct wireless_driver_ops *wlan;
	char pin[9];
	const char *value;

	mylog_trace("start %s", __func__);
	// read the arguments
	// do nothing

	memset(pin, '\0', sizeof(pin));
	if ((wlan = wlan_driver_check()) != NULL)
		wlan->wlan_get_pincode(pin, sizeof(pin));

	mylog_trace("the pin code is %s", pin);
	value = strndup(pin, sizeof(pin));

	// create a reply from the message
	reply = dbus_message_new_method_return(msg);

	// add the arguments to the reply
	dbus_message_iter_init_append(reply, &args);
	if (!dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &value))
	{
		mylog_error("Out Of Memory!");
	}

	// send the reply && flush the connection
	return reply;
}


static DBusMessage *start_wpspbc(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	mylog_trace("start %s", __func__);
	process_wps_profile(NULL);
	return dbus_message_new_method_return(msg);
}

static DBusMessage *start_wpspin(DBusConnection *conn,
                     DBusMessage *msg, void *data)
{
	mylog_trace("start %s", __func__);
	process_wps_profile(NULL);
	return dbus_message_new_method_return(msg);
}

static void make_device_msg(DBusMessageIter *dict, struct udev_device *device)
{
	const char *key, *str;
	struct udev_list_entry *list_entry;
	mylog_trace("start %s", __func__);
	mylog_trace("%s add action", __func__);
	str = udev_device_get_action(device);
	if (str != NULL)
		mydbus_dict_append_entry(dict, "action", DBUS_TYPE_STRING, &str);

	mylog_trace("%s add subsystem", __func__);
	str = udev_device_get_subsystem(device);
	if (str != NULL)
		mydbus_dict_append_entry(dict, "subsystem", DBUS_TYPE_STRING, &str);

	mylog_trace("%s add properties", __func__);
	udev_list_entry_foreach(list_entry,
			udev_device_get_properties_list_entry(device))
	{
		key = udev_list_entry_get_name(list_entry);
		str = udev_list_entry_get_value(list_entry);
		mydbus_dict_append_entry(dict, 
				key,
				DBUS_TYPE_STRING, 
				&str);
	}
	mylog_trace("%s done", __func__);
}

static DBusMessage *
version(DBusConnection *con, DBusMessage *msg, void *user_data)
{
	DBusMessage *reply;
	const char *ver = VERSION;

	mylog_debug("request the NM version");
	reply = dbus_message_new_method_return(msg);
	dbus_message_append_args(reply,
	    DBUS_TYPE_STRING, &ver,
	    DBUS_TYPE_INVALID);
	return reply;
}


static MyDBusMethodTable nm_methods[] = {
	{ "GetVersion", "", "s", version,
		MYDBUS_METHOD_FLAG_NONE },
	{ "Scan",              "s", "", scan,
		MYDBUS_METHOD_FLAG_NONE },
	{ "ScanResults", "s", "a(a{sv})", scan_results,
		MYDBUS_METHOD_FLAG_NONE },
	{ "StartupNotification", "", "", incoming_client,
		MYDBUS_METHOD_FLAG_NONE },
	{ "StartWPSPBC", "", "", start_wpspbc,
		MYDBUS_METHOD_FLAG_NONE },
	{ "StartWPSPIN", "", "", start_wpspin,
		MYDBUS_METHOD_FLAG_NONE },
	{ "GenerateWPSPin", "s", "s", generate_wps_pin,
		MYDBUS_METHOD_FLAG_NONE },
	{ NULL, NULL, NULL, NULL }
};

static MyDBusSignalTable nm_signals[] = {
	{ "ScanResults", "a(a{sv})"},
	{ "DeviceEvent","a{sv}"},
	{ "StatusChanged","s"},
	{ "USBDongleAdded","s"},
	{ "USBDongleRemoved","s"},
	{ "WirelessStatus","s"},
	{ "WirelessWPSStatus","i"},
	{ "NetworkStatus","i"},
	{ "DeviceStatus","si"},
	{ "WirelessSignal","i"},
	{ NULL, NULL }
};

#ifdef CONF_IO_DAEMON
static MyDBusSignalTable iodbus_signals[] = {
	{ IO_SIGNAL_KEYCODE, "i"},
	{ IO_SIGNAL_EVENTS,  "i"},
	{ NULL, NULL }
};
#endif

static void iface_destroy(void *user_data)
{
	//do nothing
}

int
nm_dbus_srv_init(void)
{
	DBusError err;

	dbus_error_init(&err);

	mylog_trace("Setup dbus-service for %s", NM_NAME);
 	connection = mydbus_setup_bus(DBUS_BUS_SYSTEM, NM_NAME,  &err);
	dbus_threads_init_default ();
 	mydbus_register_interface(connection,
                     NM_PATH, NM_INTERFACE,
                     nm_methods,
                     nm_signals,
                     NULL,
                     iface_destroy);

#ifdef CONF_IO_DAEMON
 	mydbus_register_interface(connection,
                     IODBUS_PATH, IODBUS_INTERFACE,
                     NULL,
                     iodbus_signals,
                     NULL,
                     NULL);
#endif

	nm_scan_ctx.list = NULL;
	pthread_mutex_init(&nm_scan_ctx.lock, NULL);
	pthread_create(&nm_scan_ctx.pid, NULL, scan_thread_body, (void *)0);
	return 0;
}

void
nm_dbus_srv_close(void)
{
	if (connection) {
		dbus_connection_unref(connection);
		connection = NULL;
	}
	pthread_cancel(nm_scan_ctx.pid);
	if (nm_scan_ctx.list)
		securesoho_free_site_obj(nm_scan_ctx.list);
	pthread_mutex_destroy(&nm_scan_ctx.lock);
}

void
nm_dbus_srv_handler(void){
	mylog_trace("start %s", __func__);
	do {
		dbus_connection_read_write_dispatch(connection, 500);
	} while (dbus_connection_get_dispatch_status(connection) == DBUS_DISPATCH_DATA_REMAINS);
	mylog_trace("%s done", __func__);
}

DBusConnection *nm_srv_get_connection(void){
	return connection;
}

int nm_dbus_srv_get_fd(void){
	int fd = -1;
	dbus_connection_get_unix_fd(connection, &fd);

	return fd;
}

#ifdef CONF_IO_DAEMON
int nm_dbus_signal_ioevents(enum IOEvent event)
{
	return mydbus_emit_signal(connection, 
			IODBUS_PATH, IODBUS_INTERFACE, IO_SIGNAL_EVENTS,
			DBUS_TYPE_INT32, 
			&event,
			DBUS_TYPE_INVALID);
}
#endif

int
nm_dbus_signal_status(const char *status)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "StatusChanged",
			DBUS_TYPE_STRING, 
			&status,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_scan_results(const char *iface)
{
	DBusMessage *msg;
	DBusMessageIter iter;

	mylog_trace("start %s", __func__);
	msg = dbus_message_new_signal(NM_PATH, NM_INTERFACE,
			"ScanResults");
	if (msg == NULL) {
		mylog_error("failed to make a device event message");
		return -1;
	}

	dbus_message_iter_init_append(msg, &iter);
	attach_scan_results(iface, &iter);
	if (!dbus_connection_send(connection, msg, NULL))
		mylog_error("failed to send status to dbus");

	dbus_message_unref(msg);
	mylog_trace("%s done", __func__);
	return 0;
}

int
nm_dbus_signal_device_event(struct udev_device *device)
{
	DBusMessage *msg;
	DBusMessageIter iter, dict;

	mylog_trace("start %s", __func__);
	msg = dbus_message_new_signal(NM_PATH, NM_INTERFACE,
			"DeviceEvent");
	if (msg == NULL) {
		mylog_error("failed to make a device event message");
		return -1;
	}
	dbus_message_iter_init_append(msg, &iter);
	dbus_message_iter_open_container(&iter, DBUS_TYPE_ARRAY,
			DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
			DBUS_TYPE_STRING_AS_STRING 
			DBUS_TYPE_VARIANT_AS_STRING
			DBUS_DICT_ENTRY_END_CHAR_AS_STRING, 
			&dict);

	make_device_msg(&dict, device);
	dbus_message_iter_close_container(&iter, &dict);

	if (!dbus_connection_send(connection, msg, NULL))
		mylog_error("failed to send status to dbus");
	dbus_message_unref(msg);
	mylog_trace("%s done", __func__);
	return 0;
}

int
nm_dbus_signal_usb_dongle_added(const char *iface)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "USBDongleAdded",
			DBUS_TYPE_STRING, 
			&iface,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_usb_dongle_removed(const char *iface)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "USBDongleRemoved",
			DBUS_TYPE_STRING, 
			&iface,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_wireless_status(const char *status)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "WirelessStatus",
			DBUS_TYPE_STRING, 
			&status,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_wps_status(const int status)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "WirelessWPSStatus",
			DBUS_TYPE_INT32, 
			&status,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_device_status(const char *iface, int status)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "DeviceStatus",
			DBUS_TYPE_STRING, 
			&iface,
			DBUS_TYPE_INT32, 
			&status,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_network_status(int status)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "NetworkStatus",
			DBUS_TYPE_INT32, 
			&status,
			DBUS_TYPE_INVALID);
}

int
nm_dbus_signal_wireless_signal(int signal)
{
	return mydbus_emit_signal(connection, 
			NM_PATH, NM_INTERFACE, "WirelessSignal",
			DBUS_TYPE_INT32, 
			&signal,
			DBUS_TYPE_INVALID);
}
