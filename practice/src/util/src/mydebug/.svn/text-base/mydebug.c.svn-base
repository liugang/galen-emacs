#include  <stdarg.h>
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  "mydebug.h"

#ifndef  MAXDBGITEM
/* Use prime is prefer. */
#define  MAXDBGITEM  211
#endif

#define  HBLANK   0
#define  HHIDE    1
#define  HSHOW    2

typedef struct {
	int    type; /* HBLANK/HHIDE/HSHOW */
	char * name;
	char * fname;
	FILE * fp;
	int    linemid; /* In the middle of line. */
}  dbgitem;

static dbgitem   dbgitem_hash[MAXDBGITEM];
static void      dbgdirective_parse(const char * directive);
static void      dbgitem_show(const char * name, const char * fname);
static void      dbgitem_hide(const char * name);
static dbgitem * dbgitem_srh(const char * name);
static void      dbgitem_printf(dbgitem * di, const char * fmt, ...);

void   mydbgstart(int argc, char * argv[]) {
	int          i, debugopt;
	const char * cfgfn;  FILE * cfgfp;  char  cfgbuf[256];

	bzero(dbgitem_hash, sizeof(dbgitem_hash));

	/* Add the two default items. */
	dbgitem_show("stdout", "stdout");
	dbgitem_show("stderr", "stderr");

	if ((cfgfn = getenv("DBGCONFIG")) != NULL &&
	    (cfgfp = fopen(cfgfn, "r")) != NULL) {
		while (fgets(cfgbuf, sizeof(cfgbuf), cfgfp) != NULL) {
			strtok(cfgbuf, "\n"); /* Remove the tail \n */
			dbgdirective_parse(cfgbuf);
		}
		fclose(cfgfp);
	}

	debugopt = 0;
	for (i = 1; i < argc; ++i)
		if (!strcmp(argv[i], "--debug"))  debugopt = !debugopt;
		else if (debugopt)  dbgdirective_parse(argv[i]);
}
void   dbgend(const char * fname, int lineno)
{
	dbgitem    * curdi;

	for (curdi = dbgitem_hash; curdi - dbgitem_hash < MAXDBGITEM; ++curdi) {
		if (curdi->name)  free(curdi->name);
		if (curdi->fname) free(curdi->fname);
		if (curdi->fp)    fclose(curdi->fp);
	}
}

void   mydbgprintf(const char * fname, int lineno, char * dbgstr) {
	char    * dbgname, * dbginforstr;
	dbgitem * di;
	int       l;

	dbginforstr = strchr(dbgstr, ':');
	dbgname = strndupa(dbgstr, dbginforstr - dbgstr);
	++dbginforstr;

	di = dbgitem_srh(dbgname);
	if (di == NULL) {
		di = dbgitem_srh("stderr"); /* Default behavior. */
		if (di == NULL)  return; /* Abort it. */
	}

	if (di->type == HHIDE)  return; /* Hide it. */

	if (di->linemid)  dbgitem_printf(di, "%s", dbginforstr);
	else {
		dbgitem_printf(di, "%s(%d):%s", fname, lineno, dbginforstr);
		di->linemid = 1;
	}

	if ((l = strlen(dbginforstr)) > 0 && dbginforstr[l - 1] == '\n')
		di->linemid = 0;

	free(dbgstr);
}

char * mydbginfor(const char * dbgname, const char * format, ...) {
	va_list   ap;
	char      result[1024], * bigresult, * infor;
	int       retval;

	if (dbgname)
		infor = result + snprintf(result, sizeof(result), "%s:", dbgname);
	else
		infor = result + snprintf(result, sizeof(result), "stderr:");
	va_start(ap, format);
	retval = vsnprintf(infor, sizeof(result) - (infor - result), format, ap);
	va_end(ap);
	if (retval < sizeof(result) - (infor - result))  return  strdup(result);
	/* Because the length of the whole string is longer, we have to malloc it
	   directly. */
	if (dbgname) {
		bigresult = malloc(strlen(dbgname) + 1 + retval + 1);
		infor = bigresult + sprintf(bigresult, "%s:", dbgname);
	} else {
		bigresult = malloc(strlen("stderr") + 1 + retval + 1);
		infor = bigresult + sprintf(bigresult, "stderr:");
	}
	va_start(ap, format);
	vsprintf(infor, format, ap);
	va_end(ap);
	return  bigresult;
}

#define  BYTE_WIDTH  16
void   mydbgdata(const char * fname, int lineno,
		 const char * dbgname, const void * data, int len)
{
	dbgitem       * di;
	unsigned char   ch;
	const char    * cdata;
	char            bytehexbuf[BYTE_WIDTH * 3 + 1];
	char            bytech[BYTE_WIDTH + 1];
	int             pos, x;

	if (dbgname == NULL)  di = dbgitem_srh("stderr");
	else if ((di = dbgitem_srh(dbgname)) == NULL)
		di = dbgitem_srh("stderr");

	cdata = (const char *)data;
	bytehexbuf[BYTE_WIDTH * 3] = 0;
	bytech[BYTE_WIDTH] = 0;

	dbgitem_printf(di, "---- %s(#%d) START(len = %d) ----\n",
		       fname, lineno, len);
	for (pos = 0; pos < len; pos += BYTE_WIDTH) {
		memset(bytehexbuf, ' ', BYTE_WIDTH * 3);
		memset(bytech, ' ', BYTE_WIDTH);
		for (x = 0; x < BYTE_WIDTH; ++x) {
			if (pos + x < len) {
				ch = (unsigned char)cdata[pos + x];
				sprintf(bytehexbuf + x * 3, "%02x", (int)ch);
				bytech[x] = (ch > ' ' && ch < 128) ? ch : '.';
			}
			if (x % 4 == 3)  bytehexbuf[x * 3 + 2] = '|';
			else  bytehexbuf[x * 3 + 2] = ' ';
		}
		dbgitem_printf(di, "|%04x|%s%s|\n", pos, bytehexbuf, bytech);
	}
	dbgitem_printf(di, "---- %s(#%d) ----\n", fname, lineno, len);
}
static char * str_escape(const void * data, int len);
void   mydbgstrdata(const char * fname, int lineno,
		    const char * dbgname, const void * data, int len)
{
	dbgitem    * di;
	const char * start, * cur;
	char       * string;
	int          output_newline;

	start = (const char *)data;
	for (cur = start; cur - start < len - 1; ++cur)
		if (!*cur) {
			mydbgdata(fname, lineno, dbgname, data, len);
			return;
		}

	if (dbgname == NULL)  di = dbgitem_srh("stderr");
	else if ((di = dbgitem_srh(dbgname)) == NULL)
		di = dbgitem_srh("stderr");

	dbgitem_printf(di, "---- %s(#%d) START(len = %d) ----\n",
		       fname, lineno, len);
	string = NULL; output_newline = 0;
	while (len > 0) {
		if (len > 512) {
			string = str_escape(data, 512);
			data = ((const char *)data) + 512;
			len -= 512;
		} else {
			string = str_escape(data, len);
			len = 0;
			output_newline =
				(strlen(string) > 0 && string[strlen(string) - 1] != '\n');
		}
		dbgitem_printf(di, "%s", string);
		free(string);
	}
	if (output_newline)  dbgitem_printf(di, "\n");
	dbgitem_printf(di, "---- %s(#%d) END ----\n", fname, lineno);
}

static int       hash_item(const char * name);
static void      reset_fname(dbgitem * di, const char * fname);
static void      dbgdirective_parse(const char * directive) {
	const char * pos;
	if (*directive == '-')  dbgitem_hide(directive + 1);
	else {
		pos = strchr(directive, '=');
		if (pos == NULL)  dbgitem_show(directive, "stderr");
		else  dbgitem_show(strndupa(directive, pos - directive), pos + 1);
	}
}
static void      dbgitem_show(const char * name, const char * fname) {
	int  pos, start;

	pos = start = hash_item(name);
	do {
		if (dbgitem_hash[pos].type == HBLANK) {
			dbgitem_hash[pos].type = HSHOW;
			dbgitem_hash[pos].name = strdup(name);
			reset_fname(dbgitem_hash + pos, fname);
			return;
		} else if (!strcmp(dbgitem_hash[pos].name, name)) {
			dbgitem_hash[pos].type = HSHOW;
			reset_fname(dbgitem_hash + pos, fname);
			return;
		}
		pos = (pos + 1) % MAXDBGITEM;
	}  while (pos != start);
	fprintf(stderr, "WARNING: TOO MANY DEBUG ITEMS(Maximum %d)\n", MAXDBGITEM);
}
static void      dbgitem_hide(const char * name) {
	int  pos, start;

	pos = start = hash_item(name);
	do {
		if (dbgitem_hash[pos].type == HBLANK) {
			dbgitem_hash[pos].type = HHIDE;
			dbgitem_hash[pos].name = strdup(name);
			return;
		} else if (!strcmp(dbgitem_hash[pos].name, name)) {
			dbgitem_hash[pos].type = HHIDE;
			reset_fname(dbgitem_hash + pos, NULL);
			return;
		}
		pos = (pos + 1) % MAXDBGITEM;
	}  while (pos != start);
	/* Not found. */
}
static dbgitem * dbgitem_srh(const char * name) {
	int  pos, start;

	pos = start = hash_item(name);
	do {
		if (dbgitem_hash[pos].type == HBLANK)  return NULL;  /* Not found. */
		else if (!strcmp(dbgitem_hash[pos].name, name))
			return dbgitem_hash + pos;  /* Found. */
		pos = (pos + 1) % MAXDBGITEM;
	}  while (pos != start);
	return  NULL;
}
static void      dbgitem_printf(dbgitem * di, const char * format, ...) {
	va_list  ap;
	if ( di && di->fp == NULL)  di->fp = fopen(di->fname, "w");
	if ( di && di->fp) {
		va_start(ap, format);
		vfprintf(di->fp, format, ap);
		va_end(ap);
		fflush(di->fp);
	}
}

static int       hash_item(const char * name) {
	int  value, pos;

	value = 0;  pos = 1;

	while (*name) {
		value = (value + *name * pos) % MAXDBGITEM;
		++name; ++pos;
	}

	return  value;
}

static void      reset_fname(dbgitem * di, const char * fname) {
	if (di->fname != NULL && fname != NULL && !strcmp(di->fname, fname))
		return;
	if (di->fname != NULL) { /* Close the original file. */
		if (di->fp != NULL &&
		    strcmp(di->fname, "stdout") && strcmp(di->fname, "stderr"))
			fclose(NULL);
		free(di->fname);
		di->fp = NULL;  di->fname = NULL;
	}
	if (fname != NULL) { /* Open the new file. */
		di->fname = strdup(fname);
		di->fp = !strcmp(fname, "stdout") ? stdout :
			!strcmp(fname, "stderr") ? stderr : NULL;
		di->linemid = 0;
	}
}

static const char * esc[] = {
	"\\0", "\\x01", "\\x02", "\\x03", "\\x04", "\\x05", "\\x06", "\\a",
	"\\b", "\\t", "\\n\n", "\\x0B", "\\f", "\\r", "\\x0E", "\\x0F",
	"\\x10", "\\x11", "\\x12", "\\x13", "\\x14", "\\x15", "\\x16", "\\x17",
	"\\x18", "\\x19", "\\x1A", "\\e", "\\x1C", "\\x1D", "\\x1E", "\\x1F"
};
static char * str_escape(const void * data, int len)
{
	const unsigned char * src, * cursrc;
	unsigned char       * dst, * curdst;

	src = (const unsigned char *)data;
	curdst = dst = alloca(len * 4 + 1);
	for (cursrc = src; cursrc - src < len; ++cursrc)
		if (*cursrc < 32) {
			strcpy((char *)curdst, (char *)esc[(int)(*cursrc)]);
			curdst += strlen(esc[(int)(*cursrc)]);
		} else if ((int)(*cursrc) >128) {
			curdst += sprintf((char *)curdst, "\\x%02X", (int)(*cursrc));
		} else
			*curdst++ = *cursrc;
	*curdst = 0;
	return strdup((char *)dst);
}
