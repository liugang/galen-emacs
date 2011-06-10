/* Copyright (C) 2006, Alphanetworks, inc.
 * vim:cindent:ts=8:sw=8
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <sys/file.h>
#include "config_access.h"
#include "securesoho.h"
#include "mkconfig_api.h"
#include "flash_op.h"

#include <pthread.h>
#include <semaphore.h>

#define LVL3DEBUG(x)	x
#define NOT_USED(a)	(void)(a)

#define FLASH_WRITE_LOCK_FILE	 "/tmp/flash_write_lock"

/*
 * Description:
 * To avoid writing the flash excessively and blocking system when 
 * writing flash. we now use the new flash monitor module to monitor 
 * the changes of /tmp/conf directory, and write the changes back to 
 * flash when necessary.
 * Usage:
 * At system start, we will add a timer in osd main chain to monitor the 
 * changes made by all processes by periodically checking the /tmp/conf 
 * directory's modify time, We now provide 3 APIs for external modules:
 *     void flash_monitor_init()
 *     void flash_monitor_checkforwrite(int is_async)
 *     void flash_monitor_destroy()
 *     void flash_monitor_add_task(const char *dir, const char *ofile);
 */

#define MAX_MY_PATH_LENGTH	(128)

typedef enum {
	FLASH_STATUS_DIRTY=0,
	FLASH_STATUS_WRITTING,
	FLASH_STATUS_CLEAN
}FLASH_CONTENT_STATUS;

struct flash_monitor_task_s{
	char		    *mdir;
	char		    *ofile;
	FLASH_CONTENT_STATUS status;
	unsigned long	     CRC;
	struct flash_monitor_task_s *next;
};

typedef struct flash_monitor_task_s flash_monitor_task_t;

struct flash_monitor_t {
	flash_monitor_task_t  * task;
	sem_t			task_lock;
	pthread_t		pid;
};

static struct flash_monitor_t  dma_flash_monitor;


static int     flash_write_lock(void)
{
	int	lockfd = -1;

	lockfd = open(FLASH_WRITE_LOCK_FILE, O_CREAT | O_RDWR, 0644);
	if (lockfd >= 0 && flock( lockfd, LOCK_EX ) < 0) {
		printf("F:%s, flock FLASH_WRITE_LOCK_FILE, failed..\n", __FUNCTION__);
	}
	return lockfd;
}

static void    flash_write_unlock(int fd)
{
	if (fd >= 0) {
		flock(fd, LOCK_UN );
		close(fd);
	}
}

static unsigned long get_dir_crc(const char *mdir)
{
	unsigned long CRC=0;
	DIR *sdir;
	struct dirent *sdirent;
	
	CRC = 0xffffffff;
	make_crc_table();
	sdir = opendir(mdir);
	if(NULL == sdir) goto out;
	while((sdirent = readdir(sdir))){
		struct stat st;
		char path[256], buf[512];
		if (!strcmp(sdirent->d_name, ".") || !strcmp(sdirent->d_name, "..")) {
			continue;
		}
		memset(buf, '\0', sizeof(buf));
		sprintf(path, "%s/%s", mdir, sdirent->d_name);
		if(!(stat(path, &st))){
			int offset=0, len;
			memset(buf, '\0', sizeof(buf));
			sprintf(buf, "%s.%ld", path, st.st_mtime);
			len = strlen(buf);
			while(offset<len){
				CRC = update_crc(CRC, (char *)buf+offset, 4);
				offset += 4;
			}
		}
	}
out:
	if(sdir) closedir(sdir);
	return CRC;
}

static int get_crc_path_name(char *str, char *path)
{
	char udn[32];
	udn16_maker(str, udn);
	
	sprintf(path, "/tmp/%s", udn);
	
	return 0;
}

static int get_saved_dir_crc(char *mdir)
{
	char path[64]={0};
	int crc=0;
	FILE *fp=0;

	get_crc_path_name(mdir, path);
	fp = fopen(path, "r");
	if(fp){
		fread((void *)&crc, 4, 1, fp);
		fclose(fp);
	}
	return crc;
}

static int set_saved_dir_crc(char *mdir, int crc)
{
	char path[64]={0};
	FILE *fp=NULL;

	get_crc_path_name(mdir, path);
	fp = fopen(path, "w+");
	if(fp){
		fwrite((const void *)&crc, 4, 1, fp);
		fclose(fp);
	}
	return 0;
}

static int flash_monitor_update_status()
{
	int lockfd;
	int changed=0;
	flash_monitor_task_t *task, *task_base;

	task_base = dma_flash_monitor.task;
	task = task_base;

	lockfd = flash_write_lock();
	sem_wait(&dma_flash_monitor.task_lock);
	while(task){
		unsigned long crc=0;
		crc = get_dir_crc(task->mdir);
		task->CRC = get_saved_dir_crc(task->mdir);
		if(crc != task->CRC){
			/* changed */
			if(changed==0) 
			changed = 1;
			task->status = FLASH_STATUS_DIRTY;
		}
		task = task->next;
	}
	task = task_base;
	while(task){
		unsigned long crc=0;
		crc = get_dir_crc(task->mdir);
		task->CRC = get_saved_dir_crc(task->mdir);
		if(crc != task->CRC){
			/* changed */
			task->CRC = crc;
			set_saved_dir_crc(task->mdir, task->CRC);
		}
		task = task->next;
	}
	sem_post(&dma_flash_monitor.task_lock);
	flash_write_unlock(lockfd);
	return changed;
}


static void flash_monitor_write2flash(void)
{
	int lockfd;
	char version[32];
	flash_monitor_task_t *task = dma_flash_monitor.task;

	lockfd = flash_write_lock();
	sem_wait(&dma_flash_monitor.task_lock);
	while(task){
		if(task->status == FLASH_STATUS_DIRTY){
			flash_op_t *p_flash_op = NULL;
			/* changed */
			fprintf(stdout, "F:%s:%d, write flash from srouce dir(%s)->%s\n", 
				__FUNCTION__, __LINE__, task->mdir, task->ofile);
			task->status = FLASH_STATUS_WRITTING;
			/* erase the flash */
			p_flash_op = create_flash_op();	
			flash_op_erase(p_flash_op, task->ofile, NULL);
			destroy_flash_op(&p_flash_op);
			securesoho_get_curversion(version);
			mkconfig_translate_config_dir_to_file(task->mdir, 
					task->ofile, CONF_PRODUCT, version);
			task->status = FLASH_STATUS_CLEAN;		
		}
		task = task->next;
	}
	sem_post(&dma_flash_monitor.task_lock);
	flash_write_unlock(lockfd);
}

static void *flash_monitor_write_thread(void *args)
{
	flash_monitor_write2flash();
	pthread_exit(NULL);
}

static void flash_monitor_write( int async )
{
	flash_monitor_task_t *task;
	FLASH_CONTENT_STATUS status = FLASH_STATUS_CLEAN;

	task = dma_flash_monitor.task;
	
	sem_wait(&dma_flash_monitor.task_lock);
	while(task){
		if (task->status == FLASH_STATUS_DIRTY){
			status = FLASH_STATUS_DIRTY;
			break;
		}
		task = task->next;
	}
	sem_post(&dma_flash_monitor.task_lock);
	if (status == FLASH_STATUS_DIRTY){
		if (async)
			pthread_create(&(dma_flash_monitor.pid), NULL, 
					flash_monitor_write_thread, NULL);
		else
			flash_monitor_write2flash();
	}
}

void flash_monitor_init()
{
	memset(&dma_flash_monitor, 0, sizeof( struct flash_monitor_t));

	/* init the lock */
	sem_init(&dma_flash_monitor.task_lock, 0, 1);
	
	/* add Application SOURCE_DIR to the monitor task */
	flash_monitor_add_task(SOURCE_DIR, TARGET_FILE);
#ifdef CONF_CONFIG_MTD_PARTITION_BACKUP
	flash_monitor_add_task(SOURCE_DIR, TARGET_FILE2);
#endif

	/* add MP_SOURCE_DIR to the monitor task */
	flash_monitor_add_task(MP_SOURCE_DIR, CONF_MP_MTD_PARTITION);
}

static flash_monitor_task_t *flash_monitor_get_task(const char *ofile)
{
	flash_monitor_task_t *task=NULL;
	
	task = dma_flash_monitor.task;
	while(task){
		if(!strcmp(task->ofile, ofile)){
			/* found the task */
			break;
		}
		task = task->next;
	}
	return task;
}

void flash_monitor_del_tasks(const char *mdir)
{
	flash_monitor_task_t *task;

	sem_wait(&dma_flash_monitor.task_lock);
	task = flash_monitor_get_task(mdir);
	if(task){
		flash_monitor_task_t *p = task;
		while(task){
			if(!strcmp(task->mdir, mdir)){
				/* found */
				if(task == dma_flash_monitor.task){
					dma_flash_monitor.task = task->next;
					break;
				}else{
					p->next = task->next;
					break;
				}
			}
			p = task;
			task = task->next;
		}
	}
	if(task){
		if(task->mdir) free(task->mdir);
		if(task->ofile) free(task->ofile);
		free(task);
	}
	sem_post(&dma_flash_monitor.task_lock);
}

void flash_monitor_del_all_tasks()
{
	flash_monitor_task_t *task, *p;

	task = dma_flash_monitor.task;
	
	sem_wait(&dma_flash_monitor.task_lock);
	while(task){
		if(task->mdir) free(task->mdir);
		if(task->ofile) free(task->ofile);
		p = task; 
		task = task->next;
		free(p);
	}
	dma_flash_monitor.task = NULL;
	sem_post(&dma_flash_monitor.task_lock);
}

void flash_monitor_add_task(const char *mdir, const char *ofile)
{
	flash_monitor_task_t *task;
	int lockfd;

	lockfd = flash_write_lock();
	if(mdir && ofile){
		sem_wait(&dma_flash_monitor.task_lock);
		task = flash_monitor_get_task(ofile);
		if(task){
			fprintf(stdout, "%s,%d,	 we have the task(%s,%s)\n",
				__FUNCTION__, __LINE__, mdir, ofile);
		}else{
			fprintf(stdout, "%s,%d, add a new task(%s,%s)\n", 
				__FUNCTION__, __LINE__, mdir, ofile);
			task=(flash_monitor_task_t*)malloc(sizeof(flash_monitor_task_t));
			memset(task, '\0', sizeof(flash_monitor_task_t));
			task->mdir = strdup(mdir);
			task->ofile = strdup(ofile);
			task->status = FLASH_STATUS_CLEAN;
			task->CRC = get_dir_crc(mdir);
			set_saved_dir_crc(task->mdir, task->CRC);
			/* add the task */
			task->next = dma_flash_monitor.task;
			dma_flash_monitor.task = task;
		}
		sem_post(&dma_flash_monitor.task_lock);
	}
	flash_write_unlock(lockfd);
}

void flash_monitor_checkforwrite( int is_async )
{
	int changed = flash_monitor_update_status();
	
	if(changed)
		flash_monitor_write(is_async);
}

void flash_monitor_destroy( void )
{
	flash_monitor_del_all_tasks();
	sem_destroy(&dma_flash_monitor.task_lock);
}
