#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <stdint.h>
#include "flash_op.h"

/*
 * Kernel structure and constant for mtd-user
 */
struct mtd_info_user {
    uint8_t type;
    uint32_t flags;
    uint32_t size;   // Total size of the MTD
    uint32_t erasesize;
    uint32_t oobblock;  // Size of OOB blocks (e.g. 512)
    uint32_t oobsize;   // Amount of OOB data per block (e.g. 16)
    uint32_t ecctype;
    uint32_t eccsize;
};

struct erase_info_user {
    uint32_t start;
    uint32_t length;
};

typedef struct mtd_info_user mtd_info_t;
typedef struct erase_info_user erase_info_t;

#define MEMGETINFO              _IOR('M', 1, struct mtd_info_user)
#define MEMERASE                _IOW('M', 2, struct erase_info_user)


/*
 * The flash_op implement for C2 platform, now we just implemented th erase. 
 * And we haven't got the source code for flash_eraseall, then we call system 
 * to erase the flash block.
 */

static int earseMtd(int devFd, size_t start_pos, size_t fsize,  flash_erase_callback_fn cb)
{
	struct mtd_info_user mtd;
	struct erase_info_user erase;

	int  i, blocks;

	/* get some info about the flash device */
	memset(&mtd,0,sizeof(struct mtd_info_user));
	if (ioctl (devFd,MEMGETINFO,&mtd) < 0) {
		perror("ioctl MEMGETINFO -");
		fprintf(stdout,"Can not get MTD infomation!\n");
		return -2;
	}
#if 0
	else {
		fprintf(stdout,"MTD '%s' infomation:\n","/dev/mtd1");
		fprintf(stdout,    "type      = %d\n",mtd.type);
		fprintf(stdout,    "size      = %d\n",mtd.size);
		fprintf(stdout,    "erasesize = %d\n",mtd.erasesize);
	}
#endif

	/* calc all the max length of the operation */
	if(fsize<=0) {
		erase.length = mtd.size;
	} else {
		erase.length = fsize & ~(mtd.erasesize - 1);
		if (fsize % mtd.erasesize) erase.length  += mtd.erasesize;
		if (erase.length > mtd.size) erase.length = mtd.size;
	}

	/* erase 1 block at a time and show what's going on */
	blocks = erase.length / mtd.erasesize;
	erase.length = mtd.erasesize;

	if (start_pos <= 0)
		erase.start = 0;
	else {
		erase.start = start_pos;
	}

	for (i = 1; i <= blocks; i++) {
//		printf("Erasing blocks: %d/%d\n", i,blocks);
		if (ioctl ((int)devFd,MEMERASE,&erase) < 0) {
			printf("Error while erasing blocks %d!\n",blocks);
			return -3;
		}
		erase.start += mtd.erasesize;

		/* To call extern callback Fn to update erase status */
		if(cb)	
			cb(NULL);
	}
	printf("Erasing finished!\n");
	return 0;
}

static int c2_get_flash_block_count(char *p_dev)
{
	struct mtd_info_user mtd;
    	uint32_t length;
	int devFd = -1;
	
	if (!p_dev)  return -1;

	devFd = open(p_dev, O_SYNC | O_RDWR);
	if(devFd <= 0){
		fprintf(stderr,"open %s - %s(%d)\n",p_dev,strerror(errno),errno);
		close(devFd);
		return -1;
	}

	/* get some info about the flash device */
	memset(&mtd,0,sizeof(struct mtd_info_user));
	if (ioctl (devFd,MEMGETINFO,&mtd) < 0) {
		perror("ioctl MEMGETINFO -");
		fprintf(stdout,"Can not get MTD infomation!\n");
		return -2;
	}
	else {
		fprintf(stderr,"MTD '%s' infomation:\n", p_dev);
		fprintf(stderr,    "type      = %d\n",mtd.type);
		fprintf(stderr,    "size      = %d\n",mtd.size);
		fprintf(stderr,    "erasesize = %d\n",mtd.erasesize);
	}

	/* calc all the max length of the operation */
	length = mtd.size;

	/* erase 1 block at a time and show what's going on */
	close(devFd);
	return length/mtd.erasesize;
}

static int c2_flash_erase(char *p_dev, flash_erase_callback_fn cb)
{
	int ret = 0;
	int fd  = -1;
	
	if (!p_dev) {  ret = -1; goto FUNC_EXIT; }

	fd = open(p_dev, O_SYNC | O_RDWR);
	if(fd <= 0){
    	fprintf(stderr,"open %s - %s(%d)\n",p_dev,strerror(errno),errno);
    	ret = -1;  goto FUNC_EXIT;
	}

  	ret = earseMtd(fd, 0, 0, cb);

FUNC_EXIT:	
	if (fd >= 0) close(fd);
	return ret;
}

static int c2_flash_erase_range(char *p_dev, size_t start_pos, size_t fsize,  flash_erase_callback_fn cb)
{
	int ret = 0;
	int fd  = -1;
	
	if (!p_dev) {  ret = -1; goto FUNC_EXIT; }

	fd = open(p_dev, O_SYNC | O_RDWR);
	if(fd <= 0){
    	fprintf(stderr,"open %s - %s(%d)\n",p_dev,strerror(errno),errno);
    	ret = -1;  goto FUNC_EXIT;
	}

  	ret = earseMtd(fd, start_pos, fsize, cb);

FUNC_EXIT:	
	if (fd >= 0) close(fd);
	return ret;
}

static flash_op_t *  c2_create_flash_op()
{
	flash_op_t *p_this = NULL;

	p_this = (flash_op_t *) malloc(sizeof(flash_op_t));
	if (!p_this) return NULL;
	else	memset(p_this, 0, sizeof(flash_op_t));

	p_this->erase = c2_flash_erase;
	p_this->erase_range =  c2_flash_erase_range;
	p_this->get_flash_block_count = c2_get_flash_block_count;

	return p_this;
}

static void c2_destroy_flash_op(flash_op_t *p_flash_op)
{
	flash_op_t *p_this = p_flash_op;

	if (p_this) free(p_this);
}

/*
 * Public API: create_flash_op, just create a flash_op object
 */
extern flash_op_t *create_flash_op()
{
	flash_op_t *p_this = NULL;
	p_this = c2_create_flash_op();
	return p_this;
}

/*
 * Public API: destroy_flash_op, just delete a flash_op object
 */
extern void destroy_flash_op(flash_op_t **p_flash_op)
{
	c2_destroy_flash_op(*p_flash_op);
	if(*p_flash_op) *p_flash_op = NULL;
}


//#define FLASH_OP_UNITEST
#ifdef FLASH_OP_UNITEST
int main()
{
	flash_op_t * p_flash_op = NULL;

	p_flash_op = create_flash_op();	
 
	flash_op_erase_range(p_flash_op, "/dev/mtd2", (4*1024*1024-64*1024), (64*1024),  NULL);
	
	destroy_flash_op(&p_flash_op);		
}
#endif // FLASH_OP_UNITEST
