/* Copyright (C) 2005, Alphanetworks, inc.
 * Author: redsonic 
 * $Header: /data/cvsroot/DMA/util/src/securesoho/securesoho.c,v 1.82.2.36 2007-11-26 06:13:38 qi_lu Exp $
 * vim:cindent:ts=8:sw=8:
 * Wills Yin: The orignal file is too big to maintain and can't be reused 
 *            by other modules; Split the securesoho into 5 files: 
 *            securesoho_network.c, securesoho_wireless.c,
 *            securesoho_firmware.c, securesoho_config.c, securesoho.c.
 */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <sys/file.h>
#include "securesoho.h"
#include "config_access.h"
#include "mkconfig_api.h"
#include "util.h"
#include <pthread.h>

#ifndef	__TARGET_H__
#error	"NO Target.h"
#endif

#include <sys/mount.h>
#include <linux/posix_types.h>
#undef dev_t
#define dev_t __kernel_dev_t
#include <linux/loop.h>
#undef dev_t
#ifndef CONF_LOOP_FORMAT
#define LOOP_FORMAT	"/dev/loop%d"
#else
#define LOOP_FORMAT	CONF_LOOP_FORMAT
#endif
#define MAX_PATH_LENGTH 1024

static int debug_level = MSG_DMACFG;
//static int debug_level = MSG_SEURESOHO_WIRELESS | MSG_DMACFG;

static int chomp(char* buf)
{
	char *p, *q;

	if (!buf || !*buf )
		return -1;
	p = buf;
	while( *p && isspace(*p) ) p++;
	if (*p == '\0') {
		*buf = '\0';
		return 0;
	} 

	if ( p != buf)
		memmove(buf, p, strlen(p) + 1);

	q = buf + strlen(buf) - 1;
	while( *q && isspace(*q)  && q >= buf) q--;
	*(q+1) = '\0';

	return 0;
}

int decode_str(char *buf, char *key, char *value, const char* delim)
{
	int n;

	if ( !buf || !key || !value || !delim)
		return -1;

	if ( (n = strcspn(buf, delim)) == 0 ) {
		*key   = '\0';
		*value = '\0';
		fprintf(stderr, "Bad line:%s\n", buf);
		return -1;
	}

	strncpy(key, buf, n);
	key[n] = '\0';
	chomp(key);

	strcpy(value, buf + n + 1);
	chomp(value);

	return 0;
}

int close_all_fd_before_execve(void)
{
	int i;

	for(i=3; i<256; i++)
		close(i);
	return 0;
}

int do_cmd(char *fmt, ...)
{
	va_list ap;
	char *argv[10];
	int argc = 0, flag;

	va_start(ap, fmt);
	argv[argc++] = fmt;
	do {
		argv[argc] = va_arg(ap, char *);
	} while (argv[argc++] != 0);
	va_end(ap);
	argc--;

#if 0
	if (0){
		flag = 0;
		fprintf(stderr, "command: ");
		while (argv[flag]) {
			fprintf(stderr, "%s ", argv[flag]);
			flag++;
		}
		printf("\n");
	}
#endif
	if (vfork() == 0) {
		close_all_fd_before_execve();
		execv(argv[0], argv);
		perror(argv[0]);
		_exit(-1);
	}
	wait(&flag);
	return flag;
}

int do_cmd_quiet(char *fmt, ...)
{
        va_list ap;
        char *argv[10];
        int argc = 0, flag;

        va_start(ap, fmt);
        argv[argc++] = fmt;
        do {
                argv[argc] = va_arg(ap, char *);
        } while (argv[argc++] != 0);
        va_end(ap);
        argc--;

        if (1){
                flag = 0;
                fprintf(stderr, "command: ");
                while (argv[flag]) {
                        fprintf(stderr, "%s ", argv[flag]);
                        flag++;
                }
                printf("\n");
        }
        if (vfork() == 0) {
		close_all_fd_before_execve();
		close(1);
		close(2);
                execv(argv[0], argv);
                perror(argv[0]);
                _exit(-1);
        }
        wait(&flag);
        return flag;
}

void do_cmd_bg(char *fmt, ...)
{
	va_list ap;
	char *argv[10];
	int argc = 0, flag;
	pid_t child_pid;

	va_start(ap, fmt);
	argv[argc++] = fmt;
	do {
		argv[argc] = va_arg(ap, char *);
	} while (argv[argc++] != 0);
	va_end(ap);
	argc--;

	if (1){
		flag = 0;
		fprintf(stderr, "command: ");
		while (argv[flag]) {
			fprintf(stderr, "%s ", argv[flag]);
			flag++;
		}
		printf("\n");
	}
	if ((child_pid=vfork()) == 0) {
		close_all_fd_before_execve();
		if (vfork() == 0){
			execvp(argv[0], argv);
			perror(argv[0]);
			_exit(0);
		}
		_exit(0);
	}
	else {
		waitpid(child_pid, &flag, 0);
	}
}

int set_pid_to_file(const char *pidfile)
{
    pid_t pid;
    FILE *fp;

    pid = getpid();
    fp = fopen(pidfile, "w+");
    if (fp == NULL) {
        perror("Write to pidfile");
        abort();
    }
    fprintf(fp, "%d", (int) pid);
    fclose(fp);
    return 1;
}

int kill_pidfile(char *file, int signal)
{
	FILE *fp = fopen(file,"r");
	int pid, x;
	int loop=0;

	if (fp == NULL) return 0;
	x = fscanf(fp,"%d",&pid);
	fclose(fp);
	kill(pid, signal);
	for(loop=0;loop<10;loop++){
		if (kill(pid,0)) break;
		sleep(1);
	}
	if (loop == 10) {
		printf("Can not kill process ID %d, pidfile=%s, signal=%d\n",pid, file, signal);
		return -1;
	}
	unlink(file);
	return 0;
}

int kill_pidfile_nowait(char *file, int signal)
{
	FILE *fp = fopen(file,"r");
	int pid, x;

	if (fp == NULL) return 0;
	x = fscanf(fp,"%d",&pid);
	fclose(fp);
	kill(pid, signal);
	unlink(file);
	return 0;
}

/* open/close is much quicker than stat for ntfs */
int is_file_exist(const char *name)
{
	int fd = open(name, O_RDONLY);
	if (fd < 0) {
		return 0;
	}
	close(fd);
	return 1;
}

int exist(const char *filename)
{
	struct stat st;

	return !stat(filename,&st);
}

int touch(const char *name)
{
	int fd = open(name ,  O_RDWR | O_CREAT,S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
	if (fd < 0) {
		perror("open");
		fprintf(stderr,"Can not create file %s\n",name);
		return 0;
	}
	close(fd);
	return 1;
}

int ps_exist(const char *pidfile)
{
	FILE *fp;
	int pid, x;

	if (!exist(pidfile)) {
		return 0;
	}
	fp = fopen(pidfile, "r");
	if (fp == NULL) {
		return 0;
	}
	x = fscanf(fp, "%d", &pid);
	fclose(fp);
	if (kill(pid, 0)) {
		return 0;
	} else {
		return 1;
	}
}

int my_system(char *cmd,...)
{
	va_list ap;
	char buf[254];

	va_start(ap,cmd);
	vsnprintf(buf, sizeof(buf), cmd, ap);
	va_end(ap);
	printf("my_system cmd buf = %s\n",buf);

	return system(buf);
}

long getul(char *num, int lower, int upper)
{
	unsigned long r;
	char *e;

	if (num == NULL) return 0;

	if ((*num == '-') || isspace(num)) {
		return 0;
	}

	r = strtoul(num, &e, 10);
	if (num == e) { /* no digit exists */
		return 0;
	}

	if (*e || (r < lower) || (r > upper)) {
		return 0;
	}

	return r;
}

int do_mknod (char *name, char *type, char *major, char *minor)
{
	mode_t mode;
	dev_t dev=0;

	if (name == NULL || type == NULL || major == NULL || minor == NULL) {
		printf("%s: %d: invalid settings.\n", __FUNCTION__, __LINE__);
		return 1;
	}

	if (strcmp(type, "c") == 0) { 
		mode = S_IFCHR;
	} else if (strcmp(type, "b") == 0) {
		mode = S_IFBLK;
	} else {
		mode = S_IFIFO;
	}

	if (strcmp(type, "p") != 0) { 
		dev = (getul(major, 0, 255) << 8) + getul(minor, 0, 255);
	}

	if (mknod(name, mode, dev) == 0) {
		return 0;
	} else {
		printf("mknod %s failed\n", name);
		return 1;
	}
}

static int del_loop(const char *device)
{
	int fd;

	if ((fd = open(device, O_RDONLY)) < 0) {
		printf("%s", device);
		return (1);
	}
	if (ioctl(fd, LOOP_CLR_FD, 0) < 0) {
		close(fd);
		printf("%s: %d: LOOP_CLR_FD failed", __FUNCTION__, __LINE__);
		return (1);
	}
	close(fd);
	return (0);
}

static int set_loop(const char *device, const char *file, int offset, int *loopro)
{
	struct loop_info loopinfo;
	int fd, ffd, mode;

	mode = *loopro ? O_RDONLY : O_RDWR;

	if ((ffd = open(file, mode)) < 0 && !*loopro && (errno != EROFS || (ffd = open(file, mode = O_RDONLY)) < 0)) {
		printf("%s: %s", __FUNCTION__, file);
		return 1;
	}
	if ((fd = open(device, mode)) < 0) {
		close(ffd);
		printf("%s:%d: %s", __FUNCTION__, __LINE__, device);
		return 1;
	}

	*loopro = (mode == O_RDONLY);

	memset(&loopinfo, 0, sizeof(loopinfo));
	strncpy(loopinfo.lo_name, file, LO_NAME_SIZE);

	loopinfo.lo_offset = offset;

	loopinfo.lo_encrypt_key_size = 0;
	if (ioctl(fd, LOOP_SET_FD, ffd) < 0) {
		printf("%s:%d: LOOP_SET_FD failed", __FUNCTION__, __LINE__);
		close(fd);
		close(ffd);
		return 1;
	}
	if (ioctl(fd, LOOP_SET_STATUS, &loopinfo) < 0) {
		(void) ioctl(fd, LOOP_CLR_FD, 0);
		printf("%s:%d: LOOP_SET_STATUS failed.", __FUNCTION__, __LINE__);
		close(fd);
		close(ffd);
		return 1;
	}
	close(fd);
	close(ffd);
	return 0;
}

static char *find_unused_loop_device(void)
{
	char dev[20];
	int i, fd;
	struct stat statbuf;
	struct loop_info loopinfo;

	for (i = 0; i <= 7; i++) {
		sprintf(dev, LOOP_FORMAT, i);
		if (stat(dev, &statbuf) == 0 && S_ISBLK(statbuf.st_mode)) {
			if ((fd = open(dev, O_RDONLY)) >= 0) {
				if (ioctl(fd, LOOP_GET_STATUS, &loopinfo) != 0) {
					if (errno == ENXIO) {	/* probably free */
						close(fd);
						return strdup(dev);
					}
				}
				close(fd);
			}
		}
	}
	return NULL;
}

typedef struct _LOOP_DATA {
	char *source;		// /dev/loopx
	char *target; 
	char *orgfile;
}LOOP_DATA;

#define MAX_LOOP_DATA 20
static LOOP_DATA *g_LoopData = NULL;

int do_mount (char *source, char *target, char *fstype, long flags, 
		int fakeIt, int mountall, int loop)
{
	int ret = 0;
	char *loopfile = NULL;

	if (source == NULL || target == NULL || fstype == NULL) { 
		printf("%s:%d: invalid settings.\n", __FUNCTION__, __LINE__);
		return 1;
	}

	if (!fakeIt) {
		if (loop) { 
			int loro = flags & MS_RDONLY;
			loopfile = strdup(source);
			source = find_unused_loop_device();
			if (!source) { 
				printf("Find spare loop device failed.\n");
				if (loopfile != NULL) { free(loopfile); loopfile = NULL;}
				return 1;
			} 
			if (set_loop(source, loopfile, 0, &loro)) { 
				if (loopfile != NULL) { free(loopfile); loopfile = NULL;}
				return 1;
			} 
			if (!(flags & MS_RDONLY) && !loro) { 
				flags |= MS_RDONLY;
			}
		}
		ret = mount(source, target, fstype, flags, NULL);
		if (ret < 0) {
			perror("Mount fail");
			fprintf(stderr, "%s:%d, mount %s on %s fail\n", __FUNCTION__, __LINE__, source, target);
		}
		if (ret < 0 && errno == EROFS) {
			printf("%s is write-protected, mounting read-only.\n", source);
			ret = mount(source, target, fstype, flags |= MS_RDONLY, NULL);
		}
		/* Don't whine about already mounted filesystems when mounting all. */
		if (ret < 0 && errno == EBUSY && mountall) {
			if (loopfile != NULL) { free(loopfile); loopfile = NULL;}
			return 0;
		}
	}

	if (ret == 0 || fakeIt == 1) {
		if (loop) {
			if (g_LoopData == NULL) {
				g_LoopData = malloc(sizeof(LOOP_DATA) * MAX_LOOP_DATA);
				if (g_LoopData) {
					memset(g_LoopData,0,sizeof(LOOP_DATA) * MAX_LOOP_DATA);
				}
			}
			if (g_LoopData) { // we need to save it for umount
				int k,loopfile_len;
				for (k=0;k<MAX_LOOP_DATA;k++) {
					if (g_LoopData[k].source == NULL) {
							g_LoopData[k].source = source; //we need to free it when we do umount
							g_LoopData[k].target = strdup(target);
							loopfile_len = strlen(loopfile) + 1;
							g_LoopData[k].orgfile = malloc(loopfile_len);
							strncpy(g_LoopData[k].orgfile,loopfile,loopfile_len);
							printf("save loop data(%d) source : %s target : %s\n",k,g_LoopData[k].source,g_LoopData[k].target);
						break;
					}
				}
			}
		}
		if (loopfile != NULL) { free(loopfile); loopfile = NULL;}
		return 0;
	}

	if (loopfile != NULL) { 
		del_loop(source);
		free(source);
		if (loopfile != NULL) { free(loopfile); loopfile = NULL;}
	}

	if (ret == EPERM) {
		printf("You don't have enough permission.\n");
	}

	return 1;
}

int do_umount (char *target)
{
	int i,ret;
	if (target) {
		ret = umount(target);
		if (ret < 0 && errno == EBUSY) {
			fprintf(stderr, "%s:%d, %s mount  fail\n", __FUNCTION__, __LINE__, target);
			return -1;
		}

		for(i=0;g_LoopData && i<MAX_LOOP_DATA;i++) {
			if (g_LoopData[i].target) {
				if (strcmp(target,g_LoopData[i].target) == 0) {
					printf("del loop(%d) source : %s target : %s\n",i,g_LoopData[i].source,g_LoopData[i].target);
					del_loop(g_LoopData[i].source);
					safe_free(g_LoopData[i].source);
					safe_free(g_LoopData[i].target);
					safe_free(g_LoopData[i].orgfile);
				}
			}
		}
		return 0;
	}
	return -1;
}

char *do_get_mount_source_by_target(char *target)
{
	int i;
	
	if (target) {
		for(i=0;g_LoopData && i<MAX_LOOP_DATA;i++) {
			if (g_LoopData[i].target) {
				if (strcmp(target,g_LoopData[i].target) == 0) {
					return g_LoopData[i].orgfile;
				}
			}
		}
	}
	return NULL;
}

int do_mkdir (char *dirname)
{
	mode_t mode = (mode_t) -1;
	mode_t mask = umask(0);
	int ret = 0;
	struct stat st;

	if (dirname == NULL) {
		printf("%s:%d: Invalid settings.\n", __FUNCTION__, __LINE__);
		return 1;
	}

	if (!stat(dirname, &st) && S_ISDIR(st.st_mode)) { 
		printf("%s already created.\n", dirname);
		return 0;
	}

	umask(mask & ~0300);
	ret = mkdir(dirname, 0700);
	if (ret < 0 && errno != EEXIST) {
		printf("mkdir %s failed (%d) \n", dirname, ret);
		return 1;
	}
	umask(mask);
	if ((mode == -1) && (chmod(dirname, mode) < 0)) {
		printf("chmod %s failed\n", dirname);
	}
	return 0;
}

static char *next_seperator(char *path)
{
    while (*path)
        if (*path == '/' || *path == '\\')
            return path;
        else
            path++;
    return NULL;
}

//make the full directory hierarchy
int do_mkdirhier(char *dirname)
{
    char *next, *prev;
    char buf[MAX_PATH_LENGTH];
    struct stat sb;
    if (strlen(dirname) >= MAX_PATH_LENGTH) return 1; 
    prev = dirname;
    while ((next = next_seperator(prev))) {
        strncpy(buf, dirname, next - dirname);
        buf[next - dirname] = '\0';
        //if parent directory doesn't exist then we create it
        if (stat(buf, &sb))
            mkdir(buf, 755);
        prev = next + 1;
    }
    if (mkdir(dirname, 755) == -1)
        return 1;

    return 0;
}

/*
 * Test to see if the passed file name is a directory.
 * Return true if it is.
 */
int
is_dir(const char *name)
{
	struct stat sbuf;

	if (stat(name, &sbuf) < 0)
		return(0);
	return(S_ISDIR(sbuf.st_mode));
}
    	

#define NOMATCH         0
#define MATCH           1
#define STARTOFSTRINGS  10
#define MIDDLEOFSTRINGS 11
#define ENDOFSTRINGS    12

#define IS_WILD(c)  ((c == '*' || c == '?') ? 1 : 0)

/*************************************************************************/

static int is_wildcard (const char *str)
{
	return (strchr(str,'?') || strchr(str,'*'));
}

/* If the last wildcard was a ? then this just tries to      */
/* match the substrings from the present position, otherwise */
/* looks for next occurrance of small within big and returns */
/* a pointer to the next character in big after small or     */
/* NULL if there is no string found to match                 */
/* If end of strings is signalled, make sure that the string */
/* is tied to the end of big. This makes sure that there is  */
/* correct alignment with end of string marker.              */

static char *after_substring(char *big, char *small, int status, char lastwild)
{ 
	char *bigptr;

	if (strlen(small) > strlen(big))                       /* 13 */
	{
		return(NULL);
	}

	if (lastwild == '?')                                   /* 14 */
	{
		if (strncmp(big,small,strlen(small)) == 0)
		{
			return(big+strlen(small));
		}
		else
		{
			return(NULL);
		}
	}

	if (status == ENDOFSTRINGS)                             /* 15 */
	{
		big = big + strlen(big) - strlen(small);
	}

	for (bigptr = big; *bigptr != '\0'; ++bigptr)           /* 16 */
	{
		if (strncmp(bigptr,small,strlen(small)) == 0)
		{
			return(bigptr+strlen(small));
		}

		if (status == STARTOFSTRINGS)                        /* 17 */
		{
			return(NULL);
		}
	}

	return(NULL);                                           /* 18 */
}


static int wild_match (char *wildptr,char *cmpptr)
{ 
	char buffer[256];
	int i, status = STARTOFSTRINGS;
	char lastwild = '\0';

	if (strstr(wildptr,"*") == NULL && strstr(wildptr,"?") == NULL)
	{
		return (! strcmp(wildptr,cmpptr));
	}

	while (1)
	{
		while (*wildptr == '?')                                /* 1 */
		{
			wildptr++;
			cmpptr++;
			if ((*cmpptr == '\0') && (*wildptr != '\0'))        /* 2 */
			{
				return(NOMATCH);
			}
			lastwild = '?';
			status = MIDDLEOFSTRINGS;
		}

		if (*wildptr == '\0' && *cmpptr == '\0')                /* 3 */
		{
			return(MATCH);
		}
		else if (*wildptr == '\0')                              /* 4 */
		{
			return(NOMATCH);
		}

		if (*wildptr == '*')                                    /* 5 */
		{
			while (*wildptr == '*')                              /* 6 */
			{
				wildptr++;
			}
			if (*wildptr == '\0')                                /* 7 */
			{
				if (*cmpptr == '\0')                              /* 8 */
				{
					return(NOMATCH);
				}
				else
				{
					return(MATCH);
				}
			}

			cmpptr++;                                            /* 9 */
			status = MIDDLEOFSTRINGS;
			lastwild = '*';
		}

		for (i = 0; !(IS_WILD(*wildptr) || *wildptr == '\0'); i++) /* 10 */
		{
			buffer[i] = *wildptr++;
			if (*wildptr == '\0')                                /* 11 */
			{
				status = ENDOFSTRINGS;
			}
		}

		buffer[i] = '\0';

		if ((cmpptr = after_substring(cmpptr,buffer,status,lastwild)) == NULL)
		{
			return(NOMATCH);                                      /* 12 */
		}

		status = MIDDLEOFSTRINGS;
	}
}

/* get the dirname and basename of path 
 * Please make sure the length of dirname and basename not less than path length
 * */

#define SLASH '/'
static int
path_split(const char *path, char *dirname, char *basename)
{
	char *b, *c;

	if (!path || !dirname || !basename){
		fprintf(stderr, "%s:%d The path is null\n", __FUNCTION__, __LINE__);
		return -1;
	}
	
	if (!(c = strrchr(path, SLASH))) {
		strcpy(dirname, ".");
		strcpy(basename, path);
		return 0;
	}

	//There is slash 
	b = c + 1; //beginning of basename
	while ( *c == SLASH && c != path ) 
		c--;

	strncpy(dirname, path, c - path + 1);
	dirname[c - path + 1] = '\0';
	strcpy(basename, b);
	return 0;
}

static int file_copy(const char *src, const char *dst)
{
	int ret = 0, n=0;
	FILE *fp_src=NULL, *fp_dst=NULL;
	char buf[256];

	if(src && dst){
		fp_src = fopen(src, "r");
		fp_dst = fopen(dst, "w+");
		if((NULL==fp_src)){
			printf("F:%s:%d, open src file[%s]\n", __FUNCTION__, __LINE__, src);
			ret = -1;
			goto out;
		}
		if (NULL==fp_dst){
			printf("F:%s:%d, open dst file[%s], err:%s\n", __FUNCTION__, __LINE__, dst, strerror(errno));
			/* To avoid the exception of existed file can't open. */
			/* Remove the file first, then open this file again. */
			if(exist(dst)) {
				unlink(dst);
				fp_dst = fopen(dst, "w+");
				if (NULL == fp_dst) {
					/*oops: just get out*/
					ret = -1; goto out;
				}
			} else {
				ret = -1;
				goto out;
			}
		}

		while((n = fread(buf, 1, 256, fp_src))){
			fwrite(buf, 1, n, fp_dst);
		}
	}
out:
	if(fp_src) fclose(fp_src);
	if(fp_dst) fclose(fp_dst);
	return ret;
}


int securesoho_copy(const char *src, const char *dst){
	char dirname[256], basename[256];
	DIR *sdir;
	struct dirent *sdirent;

	if(!src || !dst) {
		fprintf(stderr, "%s:%d The src:0x%p or dst:%p is NULL\n", __FUNCTION__, __LINE__, src, dst);
		return -1;
	}
	
	if (path_split(src, dirname, basename) < 0){
		fprintf(stderr, "%s:%d, Failed to get the dirname and basename\n", __FUNCTION__, __LINE__);
		return -1;
	}

	if (is_wildcard(dirname)){
		fprintf(stderr, "%s:%d Don't support wildcard for directory\n", __FUNCTION__, __LINE__);
		return -1;
	}
	//FIXME: to make simple, we just consider following situaltion
	//The source file are wildcard string
	//The dest are directory
	if (!is_wildcard(src)){
		if (is_dir(dst)){
			char fullpath[512];
			sprintf(fullpath, "%s/%s", dst, basename);
			return file_copy(src, fullpath);
		}else{
			return file_copy(src, dst);	
		}
	}


	sdir = opendir(dirname);
	if(NULL == sdir) {
		fprintf(stderr, "%s:%d Failed to open dir:%s\n", __FUNCTION__, __LINE__, dirname);
		return -1;	
	}
	while((sdirent = readdir(sdir)) != NULL){
		if (is_dir(sdirent->d_name)) {
			continue;
		}
		if (wild_match(basename, sdirent->d_name)){
			char sfile[512], dfile[512];
			sprintf(dfile, "%s/%s", dst, sdirent->d_name);
			sprintf(sfile, "%s/%s", dirname, sdirent->d_name);
			file_copy(sfile, dfile);
		}
	}
	if(sdir) 
		closedir(sdir);

	return 0;
}

int     securesoho_remove(char *file)
{
	int ret=1;
	if(file){
		if(unlink(file)){
			/* remove error */
			//printf("F:%s:%d, remove file[%s] error:%s\n", __FUNCTION__, __LINE__, file, strerror(errno));
			goto out;
		}
		ret = 0;
	}
out:
	return ret;
}

int	DMA_ConfLoad(void)
{
	return 0;
}

void DMA_ConfSave(void)
{
	/* we will monitor the "/tmp/conf/" files modified time in code (osd/cp/DMA_FlashWriteTimer.c),
	 * not really write flash now until defined timeout (default: 1 mins) 
	 */
	printf("\033[1;42m\nreceive a flash write request, but we will write flash at special timeout!\033[0m\n");
}

/*
 * This API is used to do factory default
 *   1) replace the config with factory default ('/conf_src/config_factory_default'->'/conf/config')
 *   2) remove wireless profile settings ('/conf/wireless_profile.xml')
 */
#define SECURESOHO_FACTORY_DEFAULT_SETTINGS      "/conf_src/config_factory_default"
#define SECURESOHO_CONFIG_SETTINGS               "/conf/config"
int securesoho_factory_default(void)
{
	int tvmode=0; /* default is NTSC */
	/* remove the /conf directory */
	do_cmd("/bin/rm", SOURCE_DIR, "-fr", NULL);
	do_mkdir(SOURCE_DIR);

	/* to copy correct config as default */
	tvmode = bs_config_int_get(CONF_DEVICE_MP_CONFIG, "CONF_DEVICE_MP_TVMODE");
	securesoho_copy(SECURESOHO_FACTORY_DEFAULT_SETTINGS, SECURESOHO_CONFIG_SETTINGS);

	if (tvmode){
		/* PAL */
		securesoho_string_set(CONFIG_NTSC_OR_PAL, "PAL");
	}
	securesoho_copy("/conf_src/wizard.conf.default", "/conf/wizard.conf");
	return 0;
}

/*
 * This API is used to do mass product mode
 *   1) replace the config with mass product ('/conf_src/config_for_mass_product'->'/conf/config')
 *   2) remove wireless profile settings ('/conf/wireless_profile.xml')
 */
#define SECURESOHO_MASS_PRODUCT_SETTINGS  "/conf_src/config_for_mass_product"
int securesoho_mass_product_mode(void)
{
	/* 1) replace the config with factory default */
	securesoho_copy(SECURESOHO_MASS_PRODUCT_SETTINGS, SECURESOHO_CONFIG_SETTINGS);

	/* Remove the affiliation data */
	securesoho_remove("/conf/setting.FavoredHost.txt");
	return 0;
}

void debug_vprintf(const char* fmt, va_list ap)
{
	char buf[1024];
	
	vsnprintf(buf, sizeof(buf), fmt, ap);
	printf("%s", buf);
}

void PRINTDEBUGMSG(unsigned int level, const char* fmt, ...)
{
	if (debug_level & level) {
		va_list ap;

		va_start(ap, fmt);
		debug_vprintf(fmt, ap);
		va_end(ap);
	}
}

#define SECURESOHO_DMA_EX_LOCK  "/tmp/dma_ex_lock"

int g_dma_ex_lockfd=0;
int securesoho_dma_ex_lock()
{
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	g_dma_ex_lockfd = open(SECURESOHO_DMA_EX_LOCK, O_CREAT | O_RDWR, 0644);
        if (g_dma_ex_lockfd >= 0 && flock(g_dma_ex_lockfd, LOCK_EX) < 0) {
		exit(0);
        }
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	return g_dma_ex_lockfd;
}

int securesoho_dma_ex_unlock(int lockfd)
{
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	if(g_dma_ex_lockfd>0){
		flock(g_dma_ex_lockfd, LOCK_UN);
		close(g_dma_ex_lockfd);
		unlink(SECURESOHO_DMA_EX_LOCK);
	}
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	return 0;
}

int securesoho_dma_ex_wait_lock()
{
	int lockfd;
	
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	lockfd = open(SECURESOHO_DMA_EX_LOCK, O_RDWR, 0644);
	if(lockfd>0){
		if (lockfd >= 0 && flock(lockfd, LOCK_EX) < 0) {
			exit(0);
		}
		flock(lockfd, LOCK_UN);
		close(lockfd);
	}
	//fprintf(stderr, ">>>>[%d]F:%s,%d\n", getpid(), __FUNCTION__, __LINE__);
	return 0;
}
