#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <semaphore.h>

#include <dbus/dbus.h>

#include "securesoho.h"
#include "mydbus.h"
#include "nm_client.h"

#define MYLOG_CATEGORY_NAME "nm_client"
#include "mylog.h"

#define NM_CLIENT_MAX_SCAN_RESULTS (48)

static DBusConnection *connection = NULL;
static int wlan_card_plug_status = 0;
static int wireless_link_status = 0;//0 for disconnected,  -1 for connecting, 1 for connected
static int wireless_wps_status = 0;
static int m_wireless_signal=0;
static int network_status = NETWORK_STATE_UNKNOWN;
static char *wlan_iface = NULL;


//FIXME: it is deprecated, we'll remove it later.
//Add it just for compatible with the old code
int b_network_link_down=0; //deprecated variable


extern int osd_trigger_event(int key);

static struct nm_scan_results_s {
	struct SITE_OBJ *list;
	int count;
	int size;
	int status; 
	int type;
	int wps_support;
}nm_scan_results;

static int nm_client_scan_results_fill(struct SITE_OBJ *list);

struct SITE_OBJ *
site_obj_new(int channel, int encrypt_type, int auth_mode,
		int signal, int wireless_type,	int wireless_mode, 
		int support_wps, const char *essid, const char *bssid){
	struct SITE_OBJ *site = calloc(sizeof(struct SITE_OBJ), 1);

	site->channel = channel;
	site->encrypt_type = encrypt_type;
	site->auth_mode = auth_mode;
	site->wireless_type = wireless_type;
	site->wireless_mode = wireless_mode;
	site->support_wps = support_wps;
	site->signal_percentage = signal;
	strncpy(site->essid, essid, sizeof(site->essid));
	strncpy(site->bssid, bssid, sizeof(site->bssid));

	return site;
}

struct SWAP_OBJ swap_site(struct SITE_OBJ *site_head, struct SITE_OBJ *site_pre, 
			struct SITE_OBJ *site, struct SITE_OBJ *site2_pre, 
			struct SITE_OBJ *site2)
{
	struct SITE_OBJ *tmp;
	struct SWAP_OBJ swap;
	int continus_site = 0;
	
	tmp = site->next;
	if (site->next == site2)
		continus_site =1;
	
	if (site_head == site)
		site_head = site2;
	else
		site_pre->next = site2;
	site->next = site2->next;
	
	if (continus_site){
		site2->next = site;
	}else{
		site2->next = tmp;
		site2_pre->next = site;
	}
	tmp = site2;
	site2 = site;
	site = tmp;
	swap.site1 = site;
	swap.site2 = site2;
	swap.site_head = site_head;
	
	return swap;
}

struct SITE_OBJ *sort_site(struct SITE_OBJ *site, SITE_SORT_TYPE_E sort_type)
{
	struct SITE_OBJ *site_head, *ptr, *ptr_pre, *ptr2, *ptr2_pre;
	struct SWAP_OBJ swap;
	int swap_flag;

	if (site == NULL)
		return NULL;

	site_head = site;
	ptr_pre = site_head;
	
	ptr = site;
	while (ptr->next){
		ptr2_pre = ptr;
		ptr2 = ptr->next;
		while (ptr2){
			swap_flag = 0;
			switch(sort_type){
			case SITE_SORT_TYPE_SSID:
				if (strcmp(ptr->essid,ptr2->essid)>0){
					swap_flag = 1;
				}
				break;
			case SITE_SORT_TYPE_SIGNAL:
				if(ptr->signal_percentage < ptr2->signal_percentage){
					swap_flag = 1;
				}
				break;
			default:
				break;
			}
			if(swap_flag){
				swap = swap_site(site_head, ptr_pre, ptr, ptr2_pre, ptr2);
				ptr = swap.site1;
				ptr2 = swap.site2;
				site_head = swap.site_head;
			}else{
				ptr2_pre = ptr2;
				ptr2 = ptr2->next;
			}
		}
		ptr_pre = ptr;
		ptr = ptr->next;
	}
	return site_head;
}

void securesoho_free_site_obj(struct SITE_OBJ *site)
{
	struct SITE_OBJ *p;
	
	while (site != NULL){
		p = site;
		site = site->next;
		free(p);
	}
}


static int     
basic_iter_get(DBusMessageIter *iter,
		int type, void *arg)
{		    
	if (dbus_message_iter_get_arg_type(iter) == type) {
		dbus_message_iter_get_basic(iter, arg);
		dbus_message_iter_next(iter);
		return 0;
	}	
	return -1;	     
}

static int 
scan_results_get(DBusMessage *msg){
	DBusMessageIter args, array, dict, item, value;
	struct SITE_OBJ *list = NULL, *new = NULL;
	int count = 0;

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args)){
		mylog_error("Message has no arguments!\n");
		return -1;
	}	
	if (dbus_message_iter_get_arg_type(&args) != DBUS_TYPE_ARRAY){
		mylog_error("Invalid AP list");
		return -1;
	}
	dbus_message_iter_recurse(&args, &array);
	while (dbus_message_iter_get_arg_type(&array) == DBUS_TYPE_ARRAY) {
		//alloc site
		int channel, encrypt_type, auth_mode, signal_strenth, wireless_mode, wireless_type, support_wps;
		char *essid, *bssid;
		dbus_message_iter_recurse(&array, &dict);
		while (dbus_message_iter_get_arg_type(&dict) == DBUS_TYPE_DICT_ENTRY) {
			dbus_message_iter_recurse(&dict, &item);
			while (dbus_message_iter_get_arg_type(&item) != DBUS_TYPE_INVALID) {
				const char *key;
				if (basic_iter_get(&item, DBUS_TYPE_STRING, &key) < 0 )
					break;
				if (dbus_message_iter_get_arg_type(&item) !=
						DBUS_TYPE_VARIANT)
					break;
				dbus_message_iter_recurse(&item, &value);
				if (strcmp(key, "ESSID") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_STRING, &essid) < 0 )
						break;
				}else if (strcmp(key, "BSSID") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_STRING, &bssid) < 0 )
						break;
				}else if (strcmp(key, "SignalStrength") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &signal_strenth) < 0 )
						break;
				}else if (strcmp(key, "EncryptType") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &encrypt_type) < 0 )
						break;
				}else if (strcmp(key, "AuthMode") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &auth_mode) < 0 )
						break;
				}else if (strcmp(key, "Channel") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &channel) < 0 )
						break;
				}else if (strcmp(key, "WirelessType") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &wireless_type) < 0 )
						break;
				}else if (strcmp(key, "WirelessMode") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &wireless_mode) < 0 )
						break;
				}else if (strcmp(key, "SupportWPS") == 0){
					if (basic_iter_get(&value, DBUS_TYPE_INT32, &support_wps) < 0 )
						break;
				}
			}
			dbus_message_iter_next(&dict);
		}
		new = site_obj_new(channel, encrypt_type, auth_mode, signal_strenth, wireless_type,  
				wireless_mode, support_wps, essid, bssid);

		new->next = list;
		list = new;
		count ++;
		dbus_message_iter_next(&array);
	}

	list = sort_site(list, SITE_SORT_TYPE_SIGNAL);
	nm_client_scan_results_fill(list);
	securesoho_free_site_obj(list);

	mylog_trace("%s done", __func__);
	return 0;
}

int 
update_scan_results(void)
{
	DBusMessage* msg;
	DBusMessage *reply;
	DBusMessageIter args;
	DBusPendingCall* pending;
	const char *iface="notused";

	if (!connection)
		return -1;

	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"ScanResults"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}
	// append arguments
	dbus_message_iter_init_append(msg, &args);
	if (!dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &iface)) {
		mylog_error( "Out Of Memory!\n");
		mylog_error("Out of memory");
		return -1;
	}

	// send message and get a handle for a reply
	if (!dbus_connection_send_with_reply (connection, msg, &pending, -1)) { // -1 is default timeout
		mylog_error("Out of memory");
		return -1;
	}
	if (NULL == pending) {
		mylog_error( "Pending Call Null\n");
		return -1;
	}
	dbus_connection_flush(connection);

	mylog_trace("Request Sent\n");

	// free message
	dbus_message_unref(msg);

	// block until we recieve a reply
	dbus_pending_call_block(pending);

	// get the reply message
	reply = dbus_pending_call_steal_reply(pending);
	if (NULL == reply) {
		mylog_error( "Reply Null\n");
		return -1;
	}

	scan_results_get(reply);
	// free reply 
	// free the pending message handle
	dbus_pending_call_unref(pending);
	dbus_message_unref(reply);
	return 0;
}


static DBusHandlerResult
handle_scan_results (DBusConnection *connection,
		DBusMessage    *message,
		void	       *user_data)
{
	mylog_trace("start %s", __func__);

	scan_results_get(message);
	
	mylog_trace("%s done", __func__);
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_device_event (DBusConnection *connection,
		DBusMessage    *message,
		void	       *user_data)
{
	mylog_info("handle scan results");

	return DBUS_HANDLER_RESULT_HANDLED;

}

static DBusHandlerResult
handle_status_changed (DBusConnection *connection,
		DBusMessage    *message,
		void	       *user_data)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	mylog_info("handle scan results");

	return DBUS_HANDLER_RESULT_HANDLED;

}

#define NETWORK_WIRED_TIMEOUT	    20
#define NETWORK_WIRELESS_TIMEOUT    80
static int nm_network_start(void){
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	int timeout;
	int is_wired_link = securesoho_nic_check_ifc_link(CONF_LAN_INTERFACE, strlen(CONF_LAN_INTERFACE));
	SecureSOHO_Wireless w;

	securesoho_wireless_get(&w);

	if (is_wired_link) {
		printf("----- wired cable plugged -----\n");
		//do nothing	
	}else { 
		printf("----- wired cable unplugged -----\n");
		/*
		 * change to wireless mode
		 */
		securesoho_wireless_get(&w);
		if(w.ssid[0] == '\0' && w.bssid[0] == '\0'){
			//There's no wireless settings in the config file, do nothing
			return 0;
		}

		securesoho_lif_type_set(CON_WIRELESS);
		timeout = NETWORK_WIRELESS_TIMEOUT;
		network_restart(timeout);
	}
	return 0;
}
static DBusHandlerResult
handle_usb_dongle_added (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	char *iface;
	int osd_event;

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_STRING !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not string", __func__);
	else
		dbus_message_iter_get_basic(&args, &iface);

	mylog_trace("Got Signal with value %s", iface);

	wlan_iface = strdup(iface);
	wlan_card_plug_status = 1; //the wireless card is ready

	nm_network_start();
	osd_event = 0x80|76;	 //0x80|76, osde_usb_dongle_add input_keybaord.c
	osd_trigger_event(osd_event);
	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_usb_dongle_removed (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	const char *iface;
	int osd_event = 0;

	mylog_trace("start %s", __func__);
	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_STRING !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not string", __func__);
	else
		dbus_message_iter_get_basic(&args, &iface);

	mylog_trace("Got Signal with value %s", iface);

	free(wlan_iface); 
	wlan_iface = NULL;
	if (wlan_card_plug_status == 1){
		osd_event = 0x80|77;	 //0x80|77, osde_usb_dongle_remove input_keybaord.c
	}
	wlan_card_plug_status = 0; //the wireless card is ready
	if (osd_event != 0)
		osd_trigger_event(osd_event);

	return DBUS_HANDLER_RESULT_HANDLED;
}
static DBusHandlerResult
handle_wireless_status (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	char *status;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_STRING !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not string", __func__);
	else
		dbus_message_iter_get_basic(&args, &status);

	mylog_trace("Got Signal with value %s", status);
	printf("\033[1;45m ===============  %s(), %d : %s...================== \033[0m	\n", __FUNCTION__, __LINE__, status);

	if (!strcmp(status, "Connected"))
		wireless_link_status = 1;
	else if (!strcmp(status, "Disconnected"))
		wireless_link_status = 0;
	else if (!strcmp(status, "Connecting"))
		wireless_link_status = -1;
	else 
		mylog_error("Wrong wireless link status");

	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_wps_status (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	DBusMessageIter args;
	int status = STATUS_WSC_UNKNOWN;
	int osd_event;

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_INT32 !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not int", __func__);
	else
		dbus_message_iter_get_basic(&args, &status);

	mylog_trace("Got Signal with value %d", status);

	wireless_wps_status = status;
	if (status == STATUS_WSC_CONFIGURED){
		osd_event = 0x80|70;	 //0x80|70, osde_wps_status_success input_keybaord.c
		osd_trigger_event(osd_event);
	}else if (status == STATUS_WSC_EAP_FAILED){
		osd_event = 0x80|71;	 //0x80|71, osde_wps_status_fail input_keybaord.c
		osd_trigger_event(osd_event);
	}
	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_device_status (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	int status = 0;
	char *iface;

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_STRING !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not string", __func__);
	else
		dbus_message_iter_get_basic(&args, &iface);

	dbus_message_iter_next(&args);

	if (DBUS_TYPE_INT32 !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not int", __func__);
	else
		dbus_message_iter_get_basic(&args, &status);

	mylog_trace("Got Signal with value %s:%d", iface, status);

	if (!strcmp(CONF_LAN_INTERFACE, iface)){
		if (status == 'k')
			b_network_link_down = 0;
		else if (status == 'm')
			b_network_link_down = 1;

		osd_trigger_event(status);
	}

	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_network_status (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	int status = NETWORK_STATE_UNKNOWN;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	mylog_trace("start %s", __func__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if (DBUS_TYPE_INT32 !=
			dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not int", __func__);
	else
		dbus_message_iter_get_basic(&args, &status);

	mylog_trace("Got Signal with value %d", status);
	
	network_status = status;
	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
handle_wireless_signal (DBusConnection *connection,
		DBusMessage    *msg,
		void	       *user_data)
{
	DBusMessageIter args;
	int signal=0;
//	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	if (!dbus_message_iter_init(msg, &args))
		mylog_error("%s Message Has No Parameters", __func__);
	else if ( DBUS_TYPE_INT32 != dbus_message_iter_get_arg_type(&args))
		mylog_error("%s Argument is not int", __func__);
	else
		dbus_message_iter_get_basic(&args, &signal);

	mylog_trace("Got Signal with value %d", signal);
	
	m_wireless_signal = signal;

	// free the message
	return DBUS_HANDLER_RESULT_HANDLED;
}

static DBusHandlerResult
message_filter (DBusConnection *connection,
		DBusMessage    *message,
		void	       *user_data)
{
//	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	mylog_info("message filter");

	if (dbus_message_is_signal(message, NM_NAME,
				"ScanResults"))
		return handle_scan_results(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"USBDongleAdded"))
		return handle_usb_dongle_added(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"USBDongleRemoved"))
		return handle_usb_dongle_removed(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"DeviceEvent"))
		return handle_device_event(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"StatusChanged"))
		return handle_status_changed(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"WirelessStatus"))
		return handle_wireless_status(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"WirelessWPSStatus"))
		return handle_wps_status(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"DeviceStatus"))
		return handle_device_status(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"NetworkStatus"))
		return handle_network_status(connection, message, user_data);
	else if (dbus_message_is_signal(message, NM_NAME,
				"WirelessSignal"))
		return handle_wireless_signal(connection, message, user_data);
	else
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
	//FIXME, maybe we need check the eroor/or method call here too
}

int
nm_client_signal_setup(DBusConnection *conn)
{
	DBusError err;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	mylog_trace("start %s", __func__);
	dbus_error_init(&err);
	if (!connection)
		connection = conn;

	dbus_bus_add_match (connection,
			"type='signal'",
			&err);
	if (dbus_error_is_set (&err))
		return -1;
	dbus_bus_add_match (connection,
			"type='error'",
			&err);
	if (dbus_error_is_set (&err))
		return -1;

	if (!dbus_connection_add_filter(connection, message_filter, NULL, NULL)) {
		mylog_error("after dbus_connection_add_filter error");
		return -1;
	}
	dbus_error_free(&err);
	mylog_trace("%s done", __func__);
	return 0;
}

int 
nm_client_register_interface(DBusConnection *conn){
	int ret = 0;
	DBusError err;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	if (!connection)
		connection = conn;

	dbus_error_init(&err);
	ret = dbus_bus_request_name(connection, NM_NAME,
			DBUS_NAME_FLAG_REPLACE_EXISTING, &err);
	if (dbus_error_is_set(&err)) {
		mylog_error("%s", err.message);
		return -1;
	}
	dbus_error_free(&err);
	nm_client_start_notification(); //telst nm that i'm alive
	return ret;
}

int nm_client_scan(const char *iface){
	DBusMessage *msg;
	DBusMessageIter args;

	const char *str = "ra0";
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	mylog_trace("start %s", __func__);
	if (!connection)
		return -1;

	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"Scan"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}

	// append arguments
	dbus_message_iter_init_append(msg, &args);
	if (!dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &str))
	{
		mylog_error("Out Of Memory!");
		return -1;
	}

	nm_scan_results.status = NM_SCAN_SCANNING;
	mylog_trace("%s done", __func__);
	return mydbus_send_message(connection, msg);
}

int nm_client_start_notification(void){
	DBusMessage *msg;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	if (!connection)
		return -1;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"StartupNotification"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}

	return mydbus_send_message(connection, msg);
}

int nm_client_start_wpspbc(void){
	DBusMessage *msg;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	if (!connection)
		return -1;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	wireless_wps_status = STATUS_WSC_UNKNOWN;
	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"StartWPSPBC"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}
	wireless_wps_status = STATUS_WSC_IN_PROGRESS;

	return mydbus_send_message(connection, msg);
}

int nm_client_start_wpspin(void){
	DBusMessage *msg;

	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	if (!connection)
		return -1;
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);

	wireless_wps_status = STATUS_WSC_UNKNOWN;
	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"StartWPSPIN"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}
	wireless_wps_status = STATUS_WSC_IN_PROGRESS;
	return mydbus_send_message(connection, msg);
}

int nm_client_get_wps_status(void){
//	printf("\033[1;45m ===============  %s(), %d...wireless_wps_status = %d ================== \033[0m  \n", __FUNCTION__, __LINE__, wireless_wps_status);
	return wireless_wps_status;
}

int nm_client_scan_results_init(WIRELESS_TYPE type, int wps_support){
	printf("\033[1;45m ===============  %s(), %d...================== \033[0m  \n", __FUNCTION__, __LINE__);
	mylog_trace("start %s, type:%d, wps:%d", __func__, type, wps_support);
	nm_scan_results.type = type;
	nm_scan_results.wps_support = wps_support;
	if (!nm_scan_results.list){
		nm_scan_results.size = NM_CLIENT_MAX_SCAN_RESULTS;
		nm_scan_results.list = calloc(sizeof(struct SITE_OBJ), nm_scan_results.size);
		if (!nm_scan_results.list){
			mylog_error("Failed to alloc %d bytes for scan results", sizeof(struct SITE_OBJ) * nm_scan_results.size);
			nm_scan_results.size = 0;
			return -1;
		}
	}
	return 0;
}

int nm_client_scan_results_destroy(void){
	if (nm_scan_results.list){
		nm_scan_results.size = 0;
		free(nm_scan_results.list);
		nm_scan_results.list = NULL;
	}
	return 0;
}
	

int nm_client_get_scan_results(struct SITE_OBJ **list, int *count)
{
	*list = nm_scan_results.list;
	*count = nm_scan_results.count;
	nm_scan_results.status = NM_SCAN_IDLE;
	return 0;
}

static int nm_client_scan_results_fill(struct SITE_OBJ *list)
{
	struct SITE_OBJ *site = list;
	int count;
	mylog_trace("filter option: wps support(%d), ap type(%d)", nm_scan_results.wps_support, nm_scan_results.type);
	for (count = 0; site != NULL && count < nm_scan_results.size; site = site->next){
		mylog_trace("SSID:(%s), WPS(%d), WIRELESS_TYPE(%d)", site->essid, site->support_wps, site->wireless_type);
		if (nm_scan_results.wps_support == 1) //filter out the no wps AP
			if (site->support_wps == 0)
				continue;
		if (nm_scan_results.type != WIRELESS_BOTH) //filter out the wireless type
			if (nm_scan_results.type != site->wireless_type)
				continue;
		memcpy(&nm_scan_results.list[count++], site, sizeof(struct SITE_OBJ));
	}
	nm_scan_results.count = count;
	nm_scan_results.status = NM_SCAN_COMPLETE;
	return 0;
}

int nm_client_get_scan_status(void)
{
	return nm_scan_results.status;;
}

int nm_client_is_wlan_available(void)
{
	return wlan_card_plug_status;
}

int nm_client_gen_wpspin(char *pin, int size){
	DBusMessage* msg;
	DBusMessage *reply;
	DBusMessageIter args;
	DBusPendingCall* pending;
	const char *iface="notused";
	char *retval = "";

	if (!connection)
		return -1;

	// create a new method call and check for errors
	msg = dbus_message_new_method_call(NM_NAME, // target for the method call
			NM_PATH, // object to call on
			NM_INTERFACE, // interface to call on
			"GenerateWPSPin"); // method name
	if (NULL == msg) {
		mylog_error("Message Null");
		return -1;
	}
	// append arguments
	dbus_message_iter_init_append(msg, &args);
	if (!dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &iface)) {
		mylog_error("Out of memory");
		return -1;
	}

	// send message and get a handle for a reply
	if (!dbus_connection_send_with_reply (connection, msg, &pending, -1)) { // -1 is default timeout
		mylog_error("Out of memory");
		return -1;
	}
	if (NULL == pending) {
		mylog_error( "Pending Call Null\n");
		return -1;
	}
	dbus_connection_flush(connection);

	mylog_trace("Request Sent\n");

	// free message
	dbus_message_unref(msg);

	// block until we recieve a reply
	dbus_pending_call_block(pending);

	// get the reply message
	reply = dbus_pending_call_steal_reply(pending);
	if (NULL == reply) {
		mylog_error( "Reply Null\n");
		return -1;
	}

	// read the parameters
	if (!dbus_message_iter_init(reply, &args)){
		mylog_error("Message has no arguments!");
		return -1;
	}else if (DBUS_TYPE_STRING != dbus_message_iter_get_arg_type(&args)){
		mylog_error("Argument is not boolean!");
		return -1;
	}else
		dbus_message_iter_get_basic(&args, &retval);

	mylog_trace("Got Reply: %s", retval);
	strncpy(pin, retval, size);

	// free reply 
	// free the pending message handle
	dbus_pending_call_unref(pending);
	dbus_message_unref(reply);
	return 0;
}

int nm_client_get_wireless_signal(void)
{
	return m_wireless_signal;
}
