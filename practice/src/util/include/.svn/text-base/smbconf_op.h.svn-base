#ifndef __SMBCONF_OP_H__
#define __SMBCONF_OP_H__

typedef enum {
	SMBCONF_ERRORCODE_NOERROR = 0, 
	SMBCONF_ERRORCODE_OPENFILEFAILED, 
	SMBCONF_ERRORCODE_READFILEFAILED, 
	SMBCONF_ERRORCODE_WRITEFILEFAILED, 
	SMBCONF_ERRORCODE_SECTIONNOTFOUND, 
	SMBCONF_ERRORCODE_PARAMETERNOTFOUND, 
	SMBCONF_ERRORCODE_SECTIONALREADYEXIST, 
	SMBCONF_ERRORCODE_INVALIDPARAMETER, 
	SMBCONF_ERRORCODE_FAILALLOCMEMORY, 
	SMBCONF_ERRORCODE_FAILXLINKEDLISTCREATE, 
}SMBCONF_ERRORCODE;


// 创建存放config数据的context，这个context将被使用在其他接口调用中
void* smbconf_create_context_data();


void smbconf_delete_context_data(void* context_data);

/*
SMBCONF_ERRORCODE smbconf_readfrom_file(const char* filename, 
					void* context_data);
*/

SMBCONF_ERRORCODE smbconf_writeto_file(const char* filename, 
					void* context_data);


SMBCONF_ERRORCODE smbconf_create_section(const char* section_name, 
					void* context_data);


SMBCONF_ERRORCODE smbconf_delete_section(const char* section_name, 
					void* context_data);


// 添加或更改现有parameter
SMBCONF_ERRORCODE smbconf_set_parameter(const char* section_name, 
					const char* parameter_name, 
					const char* parameter_value,
					void* context_data);


SMBCONF_ERRORCODE smbconf_delete_parameter(const char* section_name, 
					const char* parameter_name, 
					void* context_data);


#endif // __SMBCONF_OP_H__


