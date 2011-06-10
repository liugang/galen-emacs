#ifndef _MY_DBUS_H__
#define _MY_DBUS_H__

#include <dbus/dbus.h>

/*
 * Alpha Networks dbus domain: NAME/PATH/INTERFACE/ERROR
 */
#define MYDBUS_BASE_NAME "com.alphanetworks"
#define MYDBUS_BASE_PATH "/com/alphanetworks"
#define MYDBUS_BASE_INTERFACE "com.alphanetworks"
#define MYDBUS_ERROR_INTERFACE MYDBUS_BASE_NAME ".Error"

/*
 * Network Manager dbus: NAME/PATH/INTERFACE/ERROR
 */
#define NM_NAME                MYDBUS_BASE_NAME ".NM"
#define NM_PATH                MYDBUS_BASE_PATH "/NM"
#define NM_INTERFACE           MYDBUS_BASE_INTERFACE ".NM"
#define NM_ERROR_INTERFACE     NM_INTERFACE ".Error"

/*
 * Useragent dbus: NAME/PATH/INTERFACE/ERROR
 */
#define DBUS_USERAGENT_NAME		MYDBUS_BASE_NAME ".useragent"
#define DBUS_USERAGENT_PATH		MYDBUS_BASE_PATH "/useragent"
#define DBUS_USERAGENT_INTERFACE	MYDBUS_BASE_INTERFACE ".useragent"
#define DBUS_ERROR_INTERFACE		DBUS_USERAGENT_INTERFACE ".error"

typedef void (* MyDBusWatchFunction) (DBusConnection *connection,
							void *user_data);

typedef DBusHandlerResult (* MyDBusSignalFunction) (DBusConnection *connection,
													DBusMessage *message, void *user_data);

DBusConnection *mydbus_setup_bus(DBusBusType type, const char *name,
							DBusError *error);

int mydbus_request_name(DBusConnection *connection, const char *name,
							DBusError *error);

int mydbus_set_disconnect_function(DBusConnection *connection,
				MyDBusWatchFunction function,
				void *user_data, DBusFreeFunction destroy);

typedef void (* MyDBusDestroyFunction) (void *user_data);

typedef DBusMessage * (* MyDBusMethodFunction) (DBusConnection *connection,
					DBusMessage *message, void *user_data);

typedef enum {
	MYDBUS_METHOD_FLAG_NOREPLY    = (1 << 0),
	MYDBUS_METHOD_FLAG_ASYNC      = (1 << 1),
	MYDBUS_METHOD_FLAG_NONE       = (1 << 2),
} MyDBusMethodFlags;

typedef struct {
	const char *name;
	const char *signature;
	const char *reply;
	MyDBusMethodFunction function;
	MyDBusMethodFlags flags;
} MyDBusMethodTable;

typedef struct {
	const char *name;
	const char *signature;
} MyDBusSignalTable;

DBusConnection *mydbus_setup_bus(DBusBusType type, const char *name,
		DBusError *error);

int mydbus_register_interface(DBusConnection *connection,
					const char *path, const char *name,
					MyDBusMethodTable *methods,
					MyDBusSignalTable *signals,
					void *user_data,
					MyDBusDestroyFunction destroy);
int mydbus_unregister_interface(DBusConnection *connection,
					const char *path, const char *name);

DBusMessage *mydbus_create_error(DBusMessage *message, const char *name,
						const char *format, ...);
DBusMessage *mydbus_create_error_valist(DBusMessage *message, const char *name,
					const char *format, va_list args);
DBusMessage *mydbus_create_reply(DBusMessage *message, int type, ...);
DBusMessage *mydbus_create_reply_valist(DBusMessage *message,
						int type, va_list args);

int mydbus_send_message(DBusConnection *connection, DBusMessage *message);
int mydbus_send_reply(DBusConnection *connection,
				DBusMessage *message, int type, ...);
int mydbus_send_reply_valist(DBusConnection *connection,
				DBusMessage *message, int type, va_list args);

int mydbus_emit_signal(DBusConnection *connection,
				const char *path, const char *interface,
				const char *name, int type, ...);
int mydbus_emit_signal_valist(DBusConnection *connection,
				const char *path, const char *interface,
				const char *name, int type, va_list args);

void mydbus_dict_append_entry(DBusMessageIter *dict,
			const char *key, int type, void *val);

void mydbus_dict_append_array(DBusMessageIter *dict, const char *key, int type,
			void *val, int n_elements);

int mydbus_emit_signal_dict(DBusConnection *conn,
					const char *path,
					const char *interface,
					const char *name,
					const char *key,
					int type, void *value);

int mydbus_emit_signal_dict_array(DBusConnection *conn,
					const char *path,
					const char *interface,
					const char *name,
					const char *key,
					int type, void *value);

int mydbus_signal_setup(DBusConnection *connection, MyDBusSignalFunction signal_filter);

#endif /* _MY_DBUS_H__ */
