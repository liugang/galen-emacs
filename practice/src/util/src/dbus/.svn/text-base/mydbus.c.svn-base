#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

#include <dbus/dbus.h>

#include "kernel_list.h"
#include "mydbus.h"

#define MYLOG_CATEGORY_NAME "mydbus"
#include "mylog.h"

struct interface_data {
	struct list_head list;
	char *name;
	MyDBusMethodTable *methods;
	MyDBusSignalTable *signals;
	void *user_data;
	MyDBusDestroyFunction destroy;
};


struct generic_data {
	unsigned int refcount;
	struct interface_data *interfaces;
	char *introspect;
};

typedef struct mystr_s{
	char *str;
	size_t len;
	size_t size;
}mystr_t;

#define STR_BASE (4096 * 4)

static mystr_t *
mystr_new (const char *init)
{
	mystr_t *string;
	
	mylog_trace("start %s", __func__);
	string = malloc(sizeof(mystr_t));

	string->len = strlen (init);
	if (string->len > STR_BASE)
		string->size = string->len + STR_BASE;
	else 
		string->size = STR_BASE; //a simple way to reduce calling the alloc function
	string->str = malloc(string->size);
	strcpy(string->str, init);

	return string;
}

static char *
mystr_free (mystr_t *string, int free_segment)
{
	char *segment;
	mylog_trace("start %s", __func__);

	if (free_segment){
		free(string->str);
		segment = NULL;
	}else
		segment = string->str;
	free(string);
	return segment;
}

static	void
mystr_append_printf (mystr_t     *string,
		const char *format,
		...)
{
	va_list args;
	char *buf;
	int len;

	mylog_trace("start %s", __func__);
	va_start (args, format);
	len = vasprintf (&buf, format, args);

	if (len >= 0)
	{
		if (string->len + len >= string->size){
			string->size += len + STR_BASE;
			string->str = realloc(string->str, string->size);
		}
		memcpy (string->str + string->len, buf, len + 1);
		string->len += len;
		free (buf);
	}

	va_end (args);
}

static void print_arguments(mystr_t *gstr, const char *sig,
		const char *direction)
{
	int i;

	mylog_trace("start %s", __func__);
	for (i = 0; sig[i]; i++) {
		char type[32];
		int struct_level, dict_level;
		unsigned int len;
		int complete;

		complete = FALSE;
		struct_level = dict_level = 0;
		memset(type, 0, sizeof(type));

		/* Gather enough data to have a single complete type */
		for (len = 0; len < (sizeof(type) - 1) && sig[i]; len++, i++) {
			switch (sig[i]){
				case '(':
					struct_level++;
					break;
				case ')':
					struct_level--;
					if (struct_level <= 0 && dict_level <= 0)
						complete = TRUE;
					break;
				case '{':
					dict_level++;
					break;
				case '}':
					dict_level--;
					if (struct_level <= 0 && dict_level <= 0)
						complete = TRUE;
					break;
				case 'a':
					break;
				default:
					if (struct_level <= 0 && dict_level <= 0)
						complete = TRUE;
					break;
			}

			type[len] = sig[i];

			if (complete)
				break;
		}


		if (direction)
			mystr_append_printf(gstr,
					"\t\t\t<arg type=\"%s\" direction=\"%s\"/>\n",
					type, direction);
		else
			mystr_append_printf(gstr,
					"\t\t\t<arg type=\"%s\"/>\n",
					type);
	}
}

static void generate_interface_xml(mystr_t *gstr, struct interface_data *iface)
{
	MyDBusMethodTable *method;
	MyDBusSignalTable *signal;

	mylog_trace("start %s", __func__);
	for (method = iface->methods; method && method->name; method++) {
		if (!strlen(method->signature) && !strlen(method->reply))
			mystr_append_printf(gstr, "\t\t<method name=\"%s\"/>\n",
					method->name);
		else {
			mystr_append_printf(gstr, "\t\t<method name=\"%s\">\n",
					method->name);
			print_arguments(gstr, method->signature, "in");
			print_arguments(gstr, method->reply, "out");
			mystr_append_printf(gstr, "\t\t</method>\n");
		}
	}

	for (signal = iface->signals; signal && signal->name; signal++) {
		if (!strlen(signal->signature))
			mystr_append_printf(gstr, "\t\t<signal name=\"%s\"/>\n",
					signal->name);
		else {
			mystr_append_printf(gstr, "\t\t<signal name=\"%s\">\n",
					signal->name);
			print_arguments(gstr, signal->signature, NULL);
			mystr_append_printf(gstr, "\t\t</signal>\n");
		}
	}
}

static void generate_introspection_xml(DBusConnection *conn,
		struct generic_data *data, const char *path)
{   
	struct interface_data *iface_list = data->interfaces;
	struct interface_data *iface;
	struct list_head *pos, *q;
	mystr_t *gstr;
	char **children;
	int i;

	mylog_trace("start %s", __func__);

	free(data->introspect);

	gstr = mystr_new(DBUS_INTROSPECT_1_0_XML_DOCTYPE_DECL_NODE);

	mystr_append_printf(gstr, "<node name=\"%s\">\n", path);

	list_for_each_safe(pos, q, &(iface_list->list)){
		iface = list_entry(pos, struct interface_data, list);

		mystr_append_printf(gstr, "\t<interface name=\"%s\">\n",
				iface->name);

		generate_interface_xml(gstr, iface);

		mystr_append_printf(gstr, "\t</interface>\n");
	}

	if (!dbus_connection_list_registered(conn, path, &children))
		goto done;

	for (i = 0; children[i]; i++)
		mystr_append_printf(gstr, "\t<node name=\"%s\"/>\n",
				children[i]);

	dbus_free_string_array(children);

done:
	mystr_append_printf(gstr, "</node>\n");

	data->introspect = mystr_free(gstr, FALSE);
}

static DBusMessage *introspect(DBusConnection *connection,
		DBusMessage *message, void *user_data)
{
	struct generic_data *data = user_data;
	DBusMessage *reply;

	mylog_trace("start %s", __func__);
	if (!dbus_message_has_signature(message, DBUS_TYPE_INVALID_AS_STRING)) {
		mylog_error("Unexpected signature to introspect call");
		return NULL;
	}

	if (!data->introspect)
		generate_introspection_xml(connection, data,
				dbus_message_get_path(message));

	reply = dbus_message_new_method_return(message);
	if (!reply)
		return NULL;

	dbus_message_append_args(reply, DBUS_TYPE_STRING, &data->introspect,
			DBUS_TYPE_INVALID);

	return reply;
}

static void generic_unregister(DBusConnection *connection, void *user_data)
{
	struct generic_data *data = user_data;

	mylog_trace("start %s", __func__);
	free(data->introspect);
	free(data);
}

static struct interface_data *find_interface(struct interface_data *interfaces,
		const char *name)
{
	struct list_head *pos, *q;
	struct interface_data *iface;

	mylog_trace("start %s", __func__);
	if (!name)
		return NULL;


	list_for_each_safe(pos, q, &(interfaces->list)){
		iface = list_entry(pos, struct interface_data, list);
		if (!strcmp(name, iface->name))
			return iface;
	}
	mylog_trace("%s done", __func__);

	return NULL;
}

static DBusHandlerResult generic_message(DBusConnection *connection,
		DBusMessage *message, void *user_data)
{
	struct generic_data *data = user_data;
	struct interface_data *iface;
	MyDBusMethodTable *method;
	const char *interface;

	mylog_trace("start %s", __func__);
	interface = dbus_message_get_interface(message);

	iface = find_interface(data->interfaces, interface);
	if (!iface)
		return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;

	for (method = iface->methods; method &&
			method->name && method->function; method++) {
		DBusMessage *reply;

		if (dbus_message_is_method_call(message, iface->name,
					method->name) == FALSE)
			continue;

		if (dbus_message_has_signature(message,
					method->signature) == FALSE)
			continue;

		reply = method->function(connection, message, iface->user_data);

		if (method->flags & MYDBUS_METHOD_FLAG_NOREPLY) {
			if (reply != NULL)
				dbus_message_unref(reply);
			return DBUS_HANDLER_RESULT_HANDLED;
		}

		if (method->flags & MYDBUS_METHOD_FLAG_ASYNC) {
			if (reply == NULL)
				return DBUS_HANDLER_RESULT_HANDLED;
		}

		if (reply == NULL)
			return DBUS_HANDLER_RESULT_NEED_MEMORY;

		dbus_connection_send(connection, reply, NULL);
		dbus_message_unref(reply);

		return DBUS_HANDLER_RESULT_HANDLED;
	}

	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}

static DBusObjectPathVTable generic_table = {
	.unregister_function	= generic_unregister,
	.message_function	= generic_message,
};

static void invalidate_parent_data(DBusConnection *conn, const char *child_path)
{
	struct generic_data *data = NULL;
	char *parent_path, *slash;

	mylog_trace("start %s", __func__);
	parent_path = strdup(child_path);
	slash = strrchr(parent_path, '/');
	if (!slash)
		goto done;

	if (slash == parent_path && parent_path[1] != '\0')
		parent_path[1] = '\0';
	else
		*slash = '\0';

	if (!strlen(parent_path))
		goto done;

	if (!dbus_connection_get_object_path_data(conn, parent_path,
				(void *) &data))
		goto done;

	if (!data)
		goto done;

	free(data->introspect);
	data->introspect = NULL;

done:
	free(parent_path);
}

static MyDBusMethodTable introspect_methods[] = {
	{ "Introspect",	"",	"s", introspect	},
	{ }
};

static void add_interface(struct generic_data *data, const char *name,
		MyDBusMethodTable *methods,
		MyDBusSignalTable *signals,
		void *user_data,
		MyDBusDestroyFunction destroy)
{
	struct interface_data *iface;

	mylog_trace("start %s", __func__);

	iface = calloc(sizeof(struct interface_data), 1);
	memset(iface, 0, sizeof(struct interface_data));
	iface->name = strdup(name);
	iface->methods = methods;
	iface->signals = signals;
	iface->user_data = user_data;
	iface->destroy = destroy;

	list_add(&(iface->list), &(data->interfaces->list));
}

static struct generic_data *object_path_ref(DBusConnection *connection,
		const char *path)
{
	struct generic_data *data;

	mylog_trace("start %s", __func__);
	if (dbus_connection_get_object_path_data(connection, path,
				(void *) &data) == TRUE) {
		if (data != NULL) {
			data->refcount++;
			return data;
		}
	}

	data = calloc(sizeof(struct generic_data), 1);
	memset(data, 0, sizeof(struct generic_data));

	data->introspect = strdup(DBUS_INTROSPECT_1_0_XML_DOCTYPE_DECL_NODE "<node></node>");

	data->refcount = 1;

	if (!dbus_connection_register_object_path(connection, path,
				&generic_table, data)) {
		free(data->introspect);
		free(data);
		return NULL;
	}

	invalidate_parent_data(connection, path);

	data->interfaces = calloc(sizeof(struct interface_data), 1);
	memset(data->interfaces, 0, sizeof(struct interface_data));
	INIT_LIST_HEAD(&(data->interfaces->list));

	add_interface(data, DBUS_INTERFACE_INTROSPECTABLE,
			introspect_methods, NULL, data, NULL);

	return data;
}

static int remove_interface(struct generic_data *data, const char *name)
{
	struct interface_data *iface;

	mylog_trace("start %s", __func__);
	iface = find_interface(data->interfaces, name);
	if (!iface)
		return FALSE;

	list_del(&(iface->list));

	if (iface->destroy)
		iface->destroy(iface->user_data);

	free(iface->name);
	free(iface);

	return TRUE;
}

static void object_path_unref(DBusConnection *connection, const char *path)
{
	struct generic_data *data = NULL;

	mylog_trace("start %s", __func__);
	if (dbus_connection_get_object_path_data(connection, path,
				(void *) &data) == FALSE)
		return;

	if (data == NULL)
		return;

	data->refcount--;

	if (data->refcount > 0)
		return;

	remove_interface(data, DBUS_INTERFACE_INTROSPECTABLE);
	
	free(data->interfaces);
	data->interfaces = NULL;

	invalidate_parent_data(connection, path);

	dbus_connection_unregister_object_path(connection, path);
}

static int check_signal(DBusConnection *conn, const char *path,
		const char *interface, const char *name,
		const char **args)
{
	struct generic_data *data = NULL;
	struct interface_data *iface;
	MyDBusSignalTable *signal;

	mylog_trace("start %s", __func__);

	*args = NULL;
	if (!dbus_connection_get_object_path_data(conn, path,
				(void *) &data) || !data) {
		mylog_error("dbus_connection_emit_signal: path %s isn't registered",
				path);
		return FALSE;
	}

	iface = find_interface(data->interfaces, interface);
	if (!iface) {
		mylog_error("dbus_connection_emit_signal: %s does not implement %s",
				path, interface);
		return FALSE;
	}

	for (signal = iface->signals; signal && signal->name; signal++) {
		if (!strcmp(signal->name, name)) {
			*args = signal->signature;
			break;
		}
	}

	if (!*args) {
		mylog_error("No signal named %s on interface %s", name, interface);
		return FALSE;
	}

	mylog_trace("%s done", __func__);
	return TRUE;
}

static dbus_bool_t emit_signal_valist(DBusConnection *conn,
		const char *path,
		const char *interface,
		const char *name,
		int first_arg_type,
		va_list var_args)
{
	DBusMessage *signal;
	dbus_bool_t ret;
	const char *signature, *args;

	mylog_trace("start %s", __func__);

	if (!check_signal(conn, path, interface, name, &args))
		return FALSE;

	mylog_trace("%s create new signal", __func__);
	signal = dbus_message_new_signal(path, interface, name);
	if (!signal) {
		mylog_error("Unable to allocate new %s.%s signal", interface,  name);
		return FALSE;
	}

	mylog_trace("%s append arg list", __func__);
	ret = dbus_message_append_args_valist(signal, first_arg_type, var_args);
	if (!ret)
		goto fail;

	mylog_trace("%s check signature", __func__);
	signature = dbus_message_get_signature(signal);
	if (strcmp(args, signature) != 0) {
		mylog_error("%s.%s: expected signature'%s' but got '%s'",
				interface, name, args, signature);
		ret = FALSE;
		goto fail;
	}

	mylog_trace("%s send", __func__);
	ret = dbus_connection_send(conn, signal, NULL);

fail:
	dbus_message_unref(signal);

	mylog_trace("%s done", __func__);
	return ret;
}

DBusConnection *mydbus_setup_bus(DBusBusType type, const char *name,
		DBusError *error)
{
	DBusConnection *conn;

	mylog_trace("start %s", __func__);
	conn = dbus_bus_get(type, error);
	if (error != NULL) {
		if (dbus_error_is_set(error) == TRUE)
			return NULL;
	}

	if (conn == NULL)
		return NULL;

	if (name != NULL) {
		if (dbus_bus_request_name(conn, name,
					DBUS_NAME_FLAG_REPLACE_EXISTING, error) !=
				DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER ) {
			dbus_connection_unref(conn);
			return NULL;
		}

		if (error != NULL) {
			if (dbus_error_is_set(error) == TRUE) {
				dbus_connection_unref(conn);
				return NULL;
			}
		}
	}

	return conn;
}

int mydbus_register_interface(DBusConnection *connection,
		const char *path, const char *name,
		MyDBusMethodTable *methods,
		MyDBusSignalTable *signals,
		void *user_data,
		MyDBusDestroyFunction destroy)
{
	struct generic_data *data;

	mylog_trace("start %s", __func__);
	data = object_path_ref(connection, path);
	if (data == NULL)
		return FALSE;

	if (find_interface(data->interfaces, name))
		return FALSE;

	add_interface(data, name, methods, signals,
			user_data, destroy);

	free(data->introspect);
	data->introspect = NULL;

	return TRUE;
}

int mydbus_unregister_interface(DBusConnection *connection,
		const char *path, const char *name)
{
	struct generic_data *data = NULL;

	mylog_trace("start %s", __func__);
	if (!path)
		return FALSE;

	if (dbus_connection_get_object_path_data(connection, path,
				(void *) &data) == FALSE)
		return FALSE;

	if (data == NULL)
		return FALSE;

	if (remove_interface(data, name) == FALSE)
		return FALSE;

	free(data->introspect);
	data->introspect = NULL;

	object_path_unref(connection, path);

	return TRUE;
}

DBusMessage *mydbus_create_error_valist(DBusMessage *message, const char *name,
		const char *format, va_list args)
{
	char str[1024];

	mylog_trace("start %s", __func__);
	vsnprintf(str, sizeof(str), format, args);

	return dbus_message_new_error(message, name, str);
}

DBusMessage *mydbus_create_error(DBusMessage *message, const char *name,
		const char *format, ...)
{
	va_list args;
	DBusMessage *reply;

	mylog_trace("start %s", __func__);
	va_start(args, format);

	reply = mydbus_create_error_valist(message, name, format, args);

	va_end(args);

	return reply;
}

DBusMessage *mydbus_create_reply_valist(DBusMessage *message,
		int type, va_list args)
{
	DBusMessage *reply;

	mylog_trace("start %s", __func__);
	reply = dbus_message_new_method_return(message);
	if (reply == NULL)
		return NULL;

	if (dbus_message_append_args_valist(reply, type, args) == FALSE) {
		dbus_message_unref(reply);
		return NULL;
	}

	return reply;
}

DBusMessage *mydbus_create_reply(DBusMessage *message, int type, ...)
{
	va_list args;
	DBusMessage *reply;

	mylog_trace("start %s", __func__);
	va_start(args, type);

	reply = mydbus_create_reply_valist(message, type, args);

	va_end(args);

	return reply;
}

int mydbus_send_message(DBusConnection *connection, DBusMessage *message)
{
	dbus_bool_t result;

	mylog_trace("start %s", __func__);
	if (dbus_message_get_type(message) == DBUS_MESSAGE_TYPE_METHOD_CALL)
		dbus_message_set_no_reply(message, TRUE);

	result = dbus_connection_send(connection, message, NULL);

	dbus_message_unref(message);

	return result;
}

int mydbus_send_reply_valist(DBusConnection *connection,
		DBusMessage *message, int type, va_list args)
{
	DBusMessage *reply;

	mylog_trace("start %s", __func__);
	reply = dbus_message_new_method_return(message);
	if (reply == NULL)
		return FALSE;

	if (dbus_message_append_args_valist(reply, type, args) == FALSE) {
		dbus_message_unref(reply);
		return FALSE;
	}

	return mydbus_send_message(connection, reply);
}

int mydbus_send_reply(DBusConnection *connection,
		DBusMessage *message, int type, ...)
{
	va_list args;
	int result;

	mylog_trace("start %s", __func__);
	va_start(args, type);

	result = mydbus_send_reply_valist(connection, message, type, args);

	va_end(args);

	return result;
}

int mydbus_emit_signal(DBusConnection *connection,
		const char *path, const char *interface,
		const char *name, int type, ...)
{
	va_list args;
	int result;

	mylog_trace("start %s", __func__);
	va_start(args, type);

	result = emit_signal_valist(connection, path, interface,
			name, type, args);

	va_end(args);

	return result;
}

int mydbus_emit_signal_valist(DBusConnection *connection,
		const char *path, const char *interface,
		const char *name, int type, va_list args)
{
	mylog_trace("start %s", __func__);
	return emit_signal_valist(connection, path, interface,
			name, type, args);
}

int mydbus_signal_setup(DBusConnection *connection, MyDBusSignalFunction signal_filter)
{
	DBusError err;

	dbus_error_init(&err);

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

	if (!dbus_connection_add_filter(connection, signal_filter, NULL, NULL)) {
		mylog_error("after dbus_connection_add_filter error");
		return -1;
	}
	dbus_error_free(&err);
	mylog_trace("%s done", __func__);
	return 0;
}
