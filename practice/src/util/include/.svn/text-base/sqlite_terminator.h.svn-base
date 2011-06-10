/**
  A sqlite api wrapper
  Author: ethan_han
  Email: ethan_han@alphanetworks.com
  */
#ifndef __SQLITE_TERMINATOR_H__
#define __SQLITE_TERMINATOR_H__

typedef enum db2_datatype_t {
	db2_datatype_none = 0, 
	db2_datatype_int, 
	db2_datatype_double, 
	db2_datatype_string, 
} db2_datatype_t;

/**
 like C string function, in almost all of the functions provided below.
 it's the caller's responsibility to make sure the input parameter is not NULL.
 */

/**
  1. db2_open(), open a database , return a database connection object, return NULL on an error.
  2. db2_close(), close a database connection object.
  */
void *db2_open(const char *db_filename);
void db2_close(void *db_conn);

/**
  1. db2_execute_sql() is used to execute non-return sql query, like INSERT, DELETE, UPDATE etc. 
     return 0 on success, -1 on error.
  2. db2_query_sql(), execute SELECT sql statements. 
     return NULL if an error occured. return a recordset when success.
     maxrecordnum, specifies the maximum records you want to get. 0 or smaller means as many as possible.
     this is like "select top n ...", because sqlite3 doesn't support top select, so I provide this function.
     db2_query_sql_ex(), same as db2_query_sql(), but it provides an additional parameter start_index.
     start_index, specifies which record you want to start with. start_index should be >= 1.
  3. db2_query_sql_v2(), execute SELECT sql statements.
     db2_query_sql_v2() calls query_cbfunc once it meets a record.
     if the query_cbfunc return -1, db2_query_sql_v2 will return.
  4. db2_istable_exist(), check if the table is exist in database. 
     return -1 on an error, 0 on table not exist, 1 on table is found.
  5. db2_verify_table(), check if the table exists and has and only has the fields specified by params.
     return 0 indicates the table match the schema, otherwise return -1.
  */
int db2_execute_sql(void *db_conn, const char *sql);
void *db2_query_sql(void *db_conn, const char *sql, int maxrecordnum);
void *db2_query_sql_ex(void *db_conn, const char *sql, int start_index, int maxrecordnum);
int db2_query_sql_v2(void *db_conn, const char *sql, int maxrecordnum, 
		int (*query_cbfunc) (void *cb_record, void *cb_data), void *cb_data);
int db2_query_sql_ex_v2(void *db_conn, const char *sql, int start_index, int maxrecordnum, 
		int (*query_cbfunc) (void *cb_record, void *cb_data), void *cb_data);
int db2_istable_exist(void *db_conn, const char *tablename);
int db2_verify_table(void *db_conn, const char *tablename, const char *field_names[]);

/** functions related with recordset object.
  1. db2_recordset_free(), free the recordset.
  2. db2_recordset_recordcount(), get the number of records in recordset.
  3. db2_recordset_fieldcount(), get the number of fields in recordset.
  4. db2_recordset_firstrecord(), get the first record of recordset. return NULL if there is no record.
  5. db2_recordset_lastrecord(), get the last record of recordset. return NULL if there is no record.
  */
void db2_recordset_free(void *recordset);
int db2_recordset_recordcount(void *recordset);
int db2_recordset_fieldcount(void *recordset);
void *db2_recordset_firstrecord(void *recordset);
void *db2_recordset_lastrecord(void *recordset);

/** functions related with record object.
  1. db2_record_nextrecord(), get the next record of input record. return NULL if we meet the end of the recordset.
  2. db2_record_prevrecord(), get the next record of input record. return NULL if we meet the begin of the recordset.
  3. db2_record_remove, remove the record from the recordset, notice that this will not remove the record from database.
  4. db2_record_getfieldbyname(), get field according to name. return NULL if there is no such field.
  5. db2_record_getfieldbyindex(), get field according to index. return NULL if there is no field at the specified position.
  */
void *db2_record_nextrecord(void *record);
void *db2_record_prevrecord(void *record);
void db2_record_remove(void *record);
void *db2_record_getfieldbyname(void *record, const char *field_name);
void *db2_record_getfieldbyindex(void *record, int field_index);

/**
  1. db2_field_type(), get the data type of the field.
  2. db2_field_name(), get the name of the field.
  3. db2_field_intvalue(), take field as int type and get its value.
  4. db2_field_doublevalue(), take field as double type and get its value.
  5. db2_field_stringvalue(), take field as string type and get its value.
  */
db2_datatype_t db2_field_type(void *field);
const char *db2_field_name(void *field);
int db2_field_intvalue(void *field);
long long int db2_field_int64value(void *field);
double db2_field_doublevalue(void *field);
const char *db2_field_stringvalue(void *field);

/** transaction related functions
  db2_transaction_start(), start a transaction.
  db2_transaction_rollback(), rollback the transaction.
  db2_transaction_commit(), commit the transaction.
  parameter: 
  db_conn, should be the value returned by db2_open()
  return 0 on success, -1 on error.
  */
int db2_transaction_start(void *db_conn);
int db2_transaction_rollback(void *db_conn);
int db2_transaction_commit(void *db_conn);

/**
 associate connection, recordset, record, field object with a userdata.
 */
void db2_connection_setuserdata(void *db_conn, void *userdata);
void* db2_connection_getuserdata(void *db_conn);
void db2_recordset_setuserdata(void *recordset, void *userdata);
void *db2_recordset_getuserdata(void *recordset);
void db2_record_setuserdata(void *record, void *userdata);
void *db2_record_getuserdata(void *record);
void db2_field_setuserdata(void *field, void *userdata);
void *db2_field_getuserdata(void *field);

#endif


