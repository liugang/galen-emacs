/* Copyright (C) 2010, Alphanetworks, inc.
 */

#ifndef __REMUX_FILE_H
#define __REMUX_FILE_H

typedef struct remux_item_s
{
	char *path;
} remux_item_t;

typedef struct remux_file_handle_s
{
	char *path;
	void *items;
	int   dirty;
} remux_file_handle_t;

remux_file_handle_t *remux_file_open(char *path, int reset);
void  remux_file_close(remux_file_handle_t *hd);
void  remux_file_append(remux_file_handle_t *hd, char *path);

#endif
