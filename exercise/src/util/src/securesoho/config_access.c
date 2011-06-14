/* Copyright (C) 2005, Alphanetworks, inc.
 * vim:cindent:ts=8:sw=8
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <dirent.h>
#include <errno.h>
#include "securesoho.h"
#include "config_access.h"

#ifdef	CONF_DEBUG_CONFIG_ACCESS
#define	DEBUGP	printf
#else
#define	DEBUGP(...)	{} while(0)
#endif

#define	SECURESOHO_TMP_CONFIG	"/tmp/config"
#define	SECURESOHO_CONFIG_LOCK	"/tmp/config_lock"
#ifdef	BUFSIZ
#undef	BUFSIZ
#endif	// BUFSIZ
#define	BUFSIZ		512
#define ESEC_NONE         0
#define ESEC_IO_ERROR     1

void securesoho_parse_value(char *buf, char *key, char *value)
{
	char *s = buf;
	char *pk=key,*pv=value;

	while(*s&&isspace(*s)) s++;
	while(*s&&(*s!='=')) *pk++=*s++; *pk='\0';
	s++;
	while(*s&&*s!='\n'&&*s!='\'') s++;
	//failed to find the correct expression
	if ( *s =='\n') {
		pv[0]='0';
		return;
	}
	strcpy(pv,s+1);
	s = pv+strlen(pv);
	while(*s!= '\'') s--;
	*s=0;
}

/*********************************************************** 
 * A general interface to set/get a setting.	-- Willim Chen
 *
 * File lock : securesoho_values_set( tuple_t *);
 * File lock : securesoho_values_get( tuple_t *);
 * File lock : securesoho_string_get( char *label, char *container)
 * File lock : securesoho_string_set( char *label, char *target );
 *             securesoho_int_get( char *);
 *             securesoho_int_set( char *);
 *
 * Example:
 * 			Use the source, Luke.
 ***********************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/file.h>
static	char	bigbuf[BUFSIZ];

/* basic config access utility */
int bs_config_lock(const char *lock)
{
	int	lockfd = -1;

	if (NULL == lock)
		goto out;
	lockfd = open(lock, O_CREAT | O_RDWR, 0644);
	if (lockfd >= 0 && flock( lockfd, LOCK_EX ) < 0)
		fprintf(stdout, "\033[1;42mflock (%s) failed..\033[0m\n", lock);
out:
	return lockfd;
}

int bs_config_unlock(int fd)
{
	if (fd >= 0) {
		flock(fd, LOCK_UN );
		close(fd);
	}
	return fd;
}


int bs_config_string_get(const char *file, const char *label, char *container)
{
	FILE	*fp;
	char	buf[BUFSIZ];
	char	value[BUFSIZ];
	char	key[BUFSIZ];
	int		ret=-1;
	
	container[0] = '\0';
	if (NULL == file)
		goto out;
	fp = fopen(file, "r");
	if (!fp) {
		printf("%s:%d (%s) can not be opened\n", __FILE__, __LINE__, file);
		goto out;
	}
	while(fgets(buf, BUFSIZ, fp)) {
		securesoho_parse_value(buf, key, value);
		if (!strcmp(key, label)) {
			strcpy(container, value );
			ret = 0;
			break;
		}
	}
	fclose(fp);
out:
	return ret;
}

int bs_config_string_set(const char *file, const char *label, const char *target)
{
	FILE    *fp, *fw;
	char    buf[BUFSIZ];
	char    tmp_file[BUFSIZ];
	char    value[BUFSIZ];
	char    key[BUFSIZ];
	int     found = 0;
	int     ret = -1;
	int     changed = 0;
	struct stat st;
	

	if (NULL == file)
		goto out;
	if (stat(file, &st)){
	    /* not existed, create a new one */
	    touch(file);
	}
	if ( (fp = fopen(file, "r")) == NULL ){
		printf("%s:%d (%s) can not be opened\n", __FILE__, __LINE__, file);
		goto out;
	}
	snprintf(tmp_file, BUFSIZ, "%s.bk", file);
	if ( (fw = fopen(tmp_file, "w")) == NULL ) {
		printf("%s:%d (%s) can not be opened\n", __FILE__, __LINE__, tmp_file);
		fclose(fp);
		goto out;
	}

	while(fgets(buf,BUFSIZ,fp)) {
		securesoho_parse_value(buf, key, value );	
		if (!strcmp(key, label)) {
			if ( strcmp(value, target) ) {
				changed = 1;
				strcpy( value, target );
			}
			found++;
		}
		fprintf(fw, "%s='%s'\n", key, value);
	}
	if (!found) {
		changed = 1;
		fprintf(fw,"%s='%s'\n", label, target );
	}

	fclose(fp);
	fclose(fw);

	if (changed) {
		unlink(file);
		safe_rename(tmp_file, (char *)file);
		DEBUGP("\033[1;33mstring_set: %s: [%s], changed\033[0m\n", label, target);
	} else {
		unlink(tmp_file);
		DEBUGP("\033[1;33mstring_set: %s: [%s], no change\033[0m\n", label, target);
	}
out:
	return ret;
}
int bs_config_int_get(const char *file, const char *label)
{
	char container[BUFSIZ]={0};
	
	bs_config_string_get(file, label, container);
	return atoi(container);
}

int bs_config_int_set(const char *file, const char *label, int value)
{
	char target[BUFSIZ]={0};
	
	sprintf(target, "%d", value);
	return bs_config_string_set(file, label, target);
}

static	int	safe_cpmv( char *oldpath, char *newpath, int mv)
{
	struct	stat	st;
	int	chunk;
	int	ofd = -1;
	int	nfd = -1;
	int	ret = 0;

	/*
	 * We just try to use rename() first. 
	 * If it is not ok, the files might be in different directories.
	 * We just move it on our own.
	 */

	if (mv && !rename( oldpath, newpath ))
		return 0;

	unlink(newpath);
	if (stat(oldpath, &st))
		return -ESEC_IO_ERROR;
	ofd = open( oldpath, O_RDONLY );
	if (ofd < 0)
		return -ESEC_IO_ERROR;
	nfd = open( newpath, O_CREAT | O_RDWR| O_TRUNC, 0644 );
	if (nfd < 0) {
		close(nfd);
		return -ESEC_IO_ERROR;
	}
	while( st.st_size ) {
		if (st.st_size > BUFSIZ)
			chunk = BUFSIZ;
		else
			chunk = st.st_size;
		ret = read(ofd, bigbuf, chunk );
		write(nfd, bigbuf, ret );
		st.st_size -= ret;
	}
	close(nfd);
	close(ofd);
	if (mv)
		unlink( oldpath );
	return 0;
}


#if CONF_DEBUG_CONFIG_ACCESS
static	void securesoho_values_dump( tuple_t *tp, const char *prompt )
{
	int	i;
	printf("\n\033[1;33m%s\n", prompt);
	for(i=0; tp[i].label; i++) {
		if (tp[i].type == TUPLE_STRING)
			printf("%s: [%s]\n", tp[i].label, (char *)tp[i].container );
		else
		if (tp[i].type == TUPLE_STRING_DUP)
			printf("%s: [%s]\n", tp[i].label, *((char **)tp[i].container));
		else
			printf("%s: [%d]\n", tp[i].label, *((int*)tp[i].container));
	}
	printf("\033[0m\n");
}
#endif

int	safe_rename( char *oldpath, char *newpath)
{
	return safe_cpmv( oldpath, newpath, 1 );
}

int	safe_cp( char *oldpath, char *newpath)
{
	return safe_cpmv( oldpath, newpath, 0 );
}

int	securesoho_config_lock(void)
{
	return bs_config_lock(SECURESOHO_CONFIG_LOCK);
}

void	securesoho_config_unlock(int fd)
{
	bs_config_unlock(fd);
}

void	securesoho_replace_config( char *path )
{
	int	lockfd = securesoho_config_lock();
	safe_cp( path, SECURESOHO_CONFIG ); 
	securesoho_config_unlock(lockfd);
}

static int flush_tuple(tuple_t *t)
{
	int i;
	for(i=0; t[i].label; i++){
		switch(t[i].type){
			case TUPLE_STRING:
				((char *)t[i].container)[0] = '\0';
				break;
			case TUPLE_STRING_DUP:
				*((char **)t[i].container) = NULL;
				break;
			case TUPLE_INT:
				*((int *)t[i].container) = 0;
				break;
			default:
				break;
		}
	}
	return 0;
}
int	securesoho_values_set(tuple_t	*t)
{
	int	i;
	int	ret = 0;

	for(i=0; t[i].label; i++) {
		switch(t[i].type){
			case TUPLE_STRING:
				securesoho_string_set(t[i].label,(char *)t[i].container);
				break;
			case TUPLE_INT:
				securesoho_int_set(t[i].label, *((int *)t[i].container));
				break;
			default:
				break;
		}
	}
	return ret;
}
	
int	securesoho_values_get(tuple_t	*t)
	{
		FILE	*fp;
		char	buf[BUFSIZ];
		char	value[BUFSIZ];
	char	key[BUFSIZ];
	int		i;
	int		lockfd = -1;
	int		ret = 0;

	flush_tuple(t);

	lockfd = securesoho_config_lock();

	fp = fopen(SECURESOHO_CONFIG, "r");
	if (!fp) {
		printf("%s can not be opened, errno=%d, %s\n", 
				SECURESOHO_CONFIG, errno, strerror(errno));
		ret = -ESEC_IO_ERROR;
		goto OUT;
	}
	while(fgets(buf, BUFSIZ, fp)) {
		securesoho_parse_value(buf, key, value);
		for(i=0;t[i].label;i++) {
			if (!strcmp(key, t[i].label)) {
				switch(t[i].type){
				case TUPLE_STRING:
					strcpy(t[i].container, value );
					break;
				case TUPLE_STRING_DUP:
					*((char **)t[i].container) = strdup(value);
					break;
				case TUPLE_INT:
					*((int*)t[i].container) = atoi( value );
					break;
				default:
					printf("TUPLE_STRING_UNKNOWN value:%s\n", value);
					break;
				}
			}
		}
	}
	fclose(fp);
#if CONF_DEBUG_CONFIG_ACCESS
	securesoho_values_dump( t, __FUNCTION__ );
#endif
OUT:
	securesoho_config_unlock( lockfd );
	return ret;
}

void    securesoho_string_get(const char *label, char *container)
{
	FILE	*fp;
	char	buf[BUFSIZ];
	char	value[BUFSIZ];
	char	key[BUFSIZ];
	int	lockfd = -1;
	
	container[0] = '\0';
	lockfd = securesoho_config_lock();

	fp = fopen(SECURESOHO_CONFIG, "r");
	if (!fp) {
		printf("%s can not be opened\n", SECURESOHO_CONFIG);
		goto OUT;
	}
	while(fgets(buf, BUFSIZ, fp)) {
		securesoho_parse_value(buf, key, value);
		if (!strcmp(key, label)) {
			strcpy(container, value );
			break;
		}
	}
	fclose(fp);
	DEBUGP("\033[1;33mstring_get: %s: [%s]\033[0m\n", label, container );
OUT:
	securesoho_config_unlock( lockfd );
}

int securesoho_int_get( const char *label )
{
	char    ret[BUFSIZ]={0};
	int	value;

	securesoho_string_get( label, ret );
	value = atoi(ret);
	DEBUGP("\033[1;33mint_get: %s: [%d]\033[0m\n", label, value );
	return value;
}

int securesoho_string_set(const char *label,const char *target )
{
	FILE    *fp, *fw;
	char    buf[BUFSIZ];
	char    value[BUFSIZ];
	char    key[BUFSIZ];
	int     found = 0;
	int	lockfd = -1;
	int	ret = 0;
	int     changed = 0;
	
	lockfd = securesoho_config_lock();
	if ( (fp = fopen(SECURESOHO_CONFIG, "r")) == NULL ) {
		ret = -ESEC_IO_ERROR;
		goto OUT;
	}

	if ( (fw = fopen( SECURESOHO_TMP_CONFIG, "w")) == NULL ) {
		fclose(fp);
		ret = -ESEC_IO_ERROR;
		goto OUT;
	}

	while(fgets(buf,BUFSIZ,fp)) {
		securesoho_parse_value(buf, key, value );	
		if (!strcmp(key, label)) {
			if ( strcmp(value, target) ) {
				changed = 1;
				strcpy( value, target );
			}
			found++;
		}
		fprintf(fw, "%s='%s'\n", key, value);
	}
	if (!found) {
		changed = 1;
		fprintf(fw,"%s='%s'\n", label, target );
	}

	fclose(fp);
	fclose(fw);

	if (changed) {
		unlink(SECURESOHO_CONFIG);
		safe_rename( SECURESOHO_TMP_CONFIG, SECURESOHO_CONFIG );
		DEBUGP("\033[1;33mstring_set: %s: [%s], changed\033[0m\n", label, target);
	} else {
		unlink(SECURESOHO_TMP_CONFIG);
		DEBUGP("\033[1;33mstring_set: %s: [%s], no change\033[0m\n", label, target);
	}

OUT:
	securesoho_config_unlock( lockfd );
	return ret;
}

int securesoho_int_set( const char *label, int val )
{
	char    target[BUFSIZ];
	int	ret;

	sprintf(target, "%d", val );
	ret = securesoho_string_set( label, target );
	DEBUGP("\033[1;33mint set: %s: [%d]\033[0m\n", label, val );
	return ret;
}

/**************************************************** 
 * The end of general interface to set/get a setting.
 ****************************************************/
