#ifndef _BD_PARSER_H_
#define _BD_PARSER_H_
#include <stdio.h>
#include <stdint.h>

#ifdef CONF_ENABLE_BLURAY_NAVIGATION
#include "bdtools/clpi_parse.h"
#include "bdtools/mpls_parse.h"
#endif

#define ISO_MOUNT_PATH		"/tmp/dvd/"
#define ISO_OBJECT_ID	"0/tmp/dvd/"
#define BD_STREAM_PATH		"BDMV/STREAM/"
#define BD_PLAYLIST_PATH		"BDMV/PLAYLIST/"
#define BD_FILE_EXTENSION	".mpls"
#define BD_MPLAYLIST_FILTER_DURATION	120 /*Filter out short titles that duration less than 120 seconds*/
#define BD_MPLAYLIST_FILTER_REPEATS	2
/*
 *	purpose : check the file is in the BD or not
 *	arguments:
 *		1) the filename (Ex: /abc/cde/BDMV/STREAM/xxxxx.m2ts)
 *	return:
 *		1) return the path when this file is in the BD
 *			Ex: /abc/cde
 *		2) return NULL when this file is not in the BD
 *	remarks:
 *		the caller should free the return pointer when the pointer is not NULL
 *
 */
char* BD_Parser_CheckFile(const	char *filename);

/*
 *	purpose : get the movie filename  in the BD
 *	arguments:
 *		1) the BD path (Ex: /abc/cde)
 *		2) get the filesize of movie file
 *	return:
 *		1) it is the BD , return the max filesize filename
 *			Ex: /abc/cde/BDMV/STREAM/00019.m2ts
 *		2) it is not BD, return NULL
 *	remarks:
 *		1) we will scan the xxxx/BDMV/STREAM/ path to get the filename with max filesize.
 *		2) the caller should free the return pointer when the pointer is not NULL
 *
 */
char* BD_Parser_GetMaxSizeFile(const char *path,long long int *filesize);

#ifdef CONF_ENABLE_BLURAY_NAVIGATION
void get_bd_language_info_by_pid(CLPI_PROG_INFO *pi, int pid,char *language);
char *m2tsname_to_clipname(char *m2tsname);
char *bd_get_browse_id(char *browse_id,char *filterstring);
#endif

#endif //_BD_PARSER_H_
