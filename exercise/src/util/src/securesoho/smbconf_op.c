#include "ILibLinkedList.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "smbconf_op.h"


#define SMBCONF_MAX_LINE_LEN 1024 * 1
#define SMBCONF_READFILE_BUFFERSIZE SMBCONF_MAX_LINE_LEN


typedef struct {
	char* name;
	char* value;
}SMBCONF_PARAMETER_DATA;


typedef struct {
	char* name;
	void* parameter_list;
}SMBCONF_SECTION_DATA;


typedef struct {
	void* section_list;
} SMBCONF_DATA;

void* smbconf_create_context_data()
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) malloc(sizeof(SMBCONF_DATA));
	if (!smbconf_data)
		return NULL;
	smbconf_data->section_list = XLinkedList_Create();
	if (!smbconf_data->section_list) {
		free(smbconf_data);
		smbconf_data = NULL;
		return NULL;
	}
	return smbconf_data;
}

SMBCONF_ERRORCODE smbconf_writeto_file(const char* filename, 
					void* context_data)
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	if (!filename || filename[0] == '\0' || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	FILE* fp = fopen(filename, "wb+");
	if (!fp) {
		return SMBCONF_ERRORCODE_OPENFILEFAILED;
	}

	void* section_node = XLinkedList_GetNode_Head(smbconf_data->section_list);
	while (section_node) {
		SMBCONF_SECTION_DATA* section_data = XLinkedList_GetDataFromNode(section_node);
		fprintf(fp, "[%s]\n", section_data->name);
		void* parameter_node = XLinkedList_GetNode_Head(section_data->parameter_list);
		while (parameter_node) {
			SMBCONF_PARAMETER_DATA* parameter_data = (SMBCONF_PARAMETER_DATA*) XLinkedList_GetDataFromNode(parameter_node);
			fprintf(fp, "	%s = %s\n", parameter_data->name, parameter_data->value);
			parameter_node = XLinkedList_GetNextNode(parameter_node);
		}
		section_node = XLinkedList_GetNextNode(section_node);
	}

	fclose(fp);

	return SMBCONF_ERRORCODE_NOERROR;
}

static SMBCONF_SECTION_DATA* smbconf_find_section(const char* section_name, 
						SMBCONF_DATA* smbconf_data, 
						SMBCONF_ERRORCODE* error_code)
{
	SMBCONF_SECTION_DATA* section_data = NULL;
	*error_code = SMBCONF_ERRORCODE_NOERROR;

	if (!section_name || section_name[0] == '\0' || 
		!smbconf_data || !smbconf_data->section_list) {
		*error_code = SMBCONF_ERRORCODE_INVALIDPARAMETER;
		return NULL;
	}

	void* section_node = XLinkedList_GetNode_Head(smbconf_data->section_list);
	while (section_node) {
		section_data = (SMBCONF_SECTION_DATA*) XLinkedList_GetDataFromNode(section_node);
		if (strcmp(section_data->name, section_name) == 0) {
			break;
		}
		section_node = XLinkedList_GetNextNode(section_node);
	}

	if (!section_node) {
		*error_code = SMBCONF_ERRORCODE_SECTIONNOTFOUND;
		return NULL;
	}

	return section_data;
}

SMBCONF_ERRORCODE smbconf_create_section(const char* section_name, 
					void* context_data)
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	SMBCONF_ERRORCODE error_code;
	SMBCONF_SECTION_DATA* section_data = NULL;

	if (!section_name || section_name[0] == '\0' || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	section_data = smbconf_find_section(section_name, smbconf_data, &error_code);
	if (section_data) {
		return SMBCONF_ERRORCODE_SECTIONALREADYEXIST;
	}

	SMBCONF_SECTION_DATA* new_section = (SMBCONF_SECTION_DATA*) malloc(sizeof(SMBCONF_SECTION_DATA));
	if (!new_section)
		return SMBCONF_ERRORCODE_FAILALLOCMEMORY;
	new_section->parameter_list = XLinkedList_Create();
	if (!new_section->parameter_list) {
		free(new_section);
		return SMBCONF_ERRORCODE_FAILXLINKEDLISTCREATE;
	}
	new_section->name = strdup(section_name);
	if (!new_section->name) {
		XLinkedList_Destroy(new_section->parameter_list);
		free(new_section);
		return SMBCONF_ERRORCODE_FAILALLOCMEMORY;
	}

	XLinkedList_AddTail(smbconf_data->section_list, (void*) new_section);

	return SMBCONF_ERRORCODE_NOERROR;
}

static SMBCONF_ERRORCODE smbconf_delete_parameter_internal(SMBCONF_SECTION_DATA* section_data, 
							const char* parameter_name)
{
	if (!section_data || !section_data->parameter_list || 
		!parameter_name || parameter_name[0] == '\0') {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	void* parameter_node = XLinkedList_GetNode_Head(section_data->parameter_list);
	while (parameter_node) {
		SMBCONF_PARAMETER_DATA* parameter_data = (SMBCONF_PARAMETER_DATA*) XLinkedList_GetDataFromNode(parameter_node);
		if (strcmp(parameter_data->name, parameter_name) == 0) {
			free(parameter_data->name); // 释放SMBCONF_PARAMETER->name
			free(parameter_data->value); // 释放SMBCONF_PARAMETER->value
			free(parameter_data); // 释放SMBCONF_PARAMETER自身
			XLinkedList_Remove(parameter_node); // 从链表中移出SMBCONF_PARAMETER占用的结点
			return SMBCONF_ERRORCODE_NOERROR;
		}
		parameter_node = XLinkedList_GetNextNode(parameter_node);
	}

	return SMBCONF_ERRORCODE_PARAMETERNOTFOUND;
}

SMBCONF_ERRORCODE smbconf_delete_parameter(const char* section_name, 
					const char* parameter_name, 
					void* context_data)
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	if (!section_name || section_name[0] == '\0' || 
		!parameter_name || parameter_name[0] == '\0' || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	SMBCONF_ERRORCODE error_code;
	SMBCONF_SECTION_DATA* section_data = smbconf_find_section(section_name, smbconf_data, &error_code);
	if (!section_data) {
		return error_code;
	}

	return smbconf_delete_parameter_internal(section_data, parameter_name);
}

static SMBCONF_ERRORCODE smbconf_set_parameter_internal(SMBCONF_SECTION_DATA* section_data, 
							SMBCONF_PARAMETER_DATA* parameter_data)
{
	if (!section_data || !section_data->parameter_list || 
		!parameter_data || !parameter_data->name || parameter_data->name[0] == '\0' || 
		!parameter_data->value || parameter_data->value[0] == '\0') {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	XLinkedList_AddTail(section_data->parameter_list, (void*) parameter_data);

	return SMBCONF_ERRORCODE_NOERROR;
}

SMBCONF_ERRORCODE smbconf_set_parameter(const char* section_name, 
					const char* parameter_name, 
					const char* parameter_value,
					void* context_data)
{
	SMBCONF_ERRORCODE error_code;
	SMBCONF_SECTION_DATA* section_data = NULL;
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	SMBCONF_PARAMETER_DATA* new_parameter = NULL;

	if (!section_name || section_name[0] == '\0' || 
		!parameter_name || parameter_name[0] == '\0' || 
		!parameter_value || parameter_value[0] == '\0' || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	new_parameter = (SMBCONF_PARAMETER_DATA*) malloc(sizeof(SMBCONF_PARAMETER_DATA));
	if (!new_parameter) {
		return SMBCONF_ERRORCODE_FAILALLOCMEMORY;
	}

	new_parameter->name = strdup(parameter_name);
	new_parameter->value = strdup(parameter_value);
	if (!new_parameter->name || !new_parameter->value) {
		error_code = SMBCONF_ERRORCODE_FAILALLOCMEMORY;
		goto error_handle;
	}

	section_data = smbconf_find_section(section_name, smbconf_data, &error_code);
	if (!section_data) {
		goto error_handle;
	}

	smbconf_delete_parameter_internal(section_data, parameter_name);
	error_code = smbconf_set_parameter_internal(section_data, new_parameter);
	if (error_code != SMBCONF_ERRORCODE_NOERROR) {
		goto error_handle;
	}
	return SMBCONF_ERRORCODE_NOERROR;

error_handle: 
	if (new_parameter) {
		if (new_parameter->name) free(new_parameter->name);
		if (new_parameter->value) free(new_parameter->value);
		free(new_parameter);
	}
	return error_code;
}

static SMBCONF_ERRORCODE smbconf_delete_section_internal(SMBCONF_SECTION_DATA* section_data, 
						SMBCONF_DATA* smbconf_data)
{
	if (!section_data || !section_data->parameter_list || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	void* parameter_node = XLinkedList_GetNode_Head(section_data->parameter_list);
	while (parameter_node) {
		SMBCONF_PARAMETER_DATA* parameter_data = (SMBCONF_PARAMETER_DATA*) XLinkedList_GetDataFromNode(parameter_node);
		free(parameter_data->name);
		free(parameter_data->value);
		free(parameter_data);
		parameter_node = XLinkedList_GetNextNode(parameter_node);
	}
	XLinkedList_Destroy(section_data->parameter_list);
	free(section_data->name);
	XLinkedList_Remove_ByData(smbconf_data->section_list, section_data);
	free(section_data);

	return SMBCONF_ERRORCODE_NOERROR;
}

SMBCONF_ERRORCODE smbconf_delete_section(const char* section_name, 
					void* context_data)
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	if (!section_name || strlen(section_name) == 0 || 
		!smbconf_data || !smbconf_data->section_list) {
		return SMBCONF_ERRORCODE_INVALIDPARAMETER;
	}

	SMBCONF_ERRORCODE error_code;
	SMBCONF_SECTION_DATA* section_data = smbconf_find_section(section_name, smbconf_data, &error_code);
	if (!section_data) {
		return error_code;
	}

	return smbconf_delete_section_internal(section_data, smbconf_data);
}

void smbconf_delete_context_data(void* context_data)
{
	SMBCONF_DATA* smbconf_data = (SMBCONF_DATA*) context_data;
	if (!smbconf_data || !smbconf_data->section_list)
		return;
	void* section_node = XLinkedList_GetNode_Head(smbconf_data->section_list);
	while (section_node) {
		SMBCONF_SECTION_DATA* section_data = (SMBCONF_SECTION_DATA*) XLinkedList_GetDataFromNode(section_node);
		smbconf_delete_section_internal(section_data, smbconf_data);
		section_node = XLinkedList_GetNode_Head(smbconf_data->section_list);
	}
	XLinkedList_Destroy(smbconf_data->section_list);
	free(smbconf_data);
}

