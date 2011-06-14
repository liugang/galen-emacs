#include "chardev.h"

MODULE_LICENSE("GPL");
MODULE_AUTHOR("galen");


//#define __KERNEL_SYSCALLS__
//#define bao "/dev/baovar"

//static char buf1[20];
//static char buf2[20];

RTMP_OS_FD RtmpOSFileOpen(char *pPath,
			  int flag,
			  int mode) {
	struct file *filePtr;

	if (flag == RTMP_FILE_RDONLY)
		flag = O_RDONLY;
	else if (flag == RTMP_FILE_WRONLY)
		flag = O_WRONLY;
	else if (flag == RTMP_FILE_CREAT)
		flag = O_CREAT;
	else if (flag == RTMP_FILE_TRUNC)
		flag = O_TRUNC;

	filePtr = filp_open(pPath, flag, 0);
	if (IS_ERR(filePtr)) {
		DBGPRINT(RT_DEBUG_ERROR,
			 ("%s(): Error %ld opening %s\n", __FUNCTION__,
			  -PTR_ERR(filePtr), pPath));
	}


	return (RTMP_OS_FD) filePtr;
}

int RtmpOSFileClose(RTMP_OS_FD osfd) {
	filp_close(osfd, NULL);
	return 0;
}

void RtmpOSFileSeek(RTMP_OS_FD osfd,
		    int offset) {
	osfd->f_pos = offset;
}

int RtmpOSFileRead(RTMP_OS_FD osfd,
		     char *pDataPtr, int readLen) {
	/* The object must have a read method */
	if (osfd->f_op && osfd->f_op->read) {
		DBGPRINT(RT_DEBUG_ERROR, ("file read method\n"));

		return osfd->f_op->read(osfd, pDataPtr, readLen, &osfd->f_pos);
	} else {
		DBGPRINT(RT_DEBUG_ERROR, ("no file read method\n"));
		return -1;
	}
}

int RtmpOSFileWrite(RTMP_OS_FD osfd,
		    char *pDataPtr, int writeLen) {
	return osfd->f_op->write(osfd,
				 pDataPtr,
				 (
	size_t) writeLen,
				 &osfd->f_pos);
}

NDIS_STATUS os_alloc_mem(
	IN VOID *pReserved,
	OUT UCHAR **mem,
	IN ULONG size)
{
	/* #include <linux/slab.h> void *kmalloc(size_t size, int flags);  */
	*mem = (PUCHAR) kmalloc(size, GFP_ATOMIC);
	if (*mem) {
		return (NDIS_STATUS_SUCCESS);
	} else
		return (NDIS_STATUS_FAILURE);
}

NDIS_STATUS os_free_mem(
	IN VOID *pReserved,
	IN PVOID mem)
{
	ASSERT(mem);
	kfree(mem);

	return (NDIS_STATUS_SUCCESS);
}

/* 注意：在调用filp->f_op->read和filp->f_op->write等对文件的操作之前，应该先设置
FS。默认情况下，filp->f_op->read或者filp->f_op->write会对传进来的参数buff进行指针
检查。如果不是在用户空间会拒绝访问。因为是在内核模块中，所以buff肯定不在用户空间，
所以要增大其寻址范围。 */
static inline void __RtmpOSFSInfoChange(OS_FS_INFO * pOSFSInfo,
					BOOLEAN bSet) {
	if (bSet) {
		/* Save uid and gid used for filesystem access. */
		/* Set user and group to 0 (root) */
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,29)
		pOSFSInfo->fsuid = current->fsuid;
		pOSFSInfo->fsgid = current->fsgid;
		current->fsuid = current->fsgid = 0;
#else
		pOSFSInfo->fsuid = current_fsuid();
		pOSFSInfo->fsgid = current_fsgid();
#endif
		pOSFSInfo->fs = get_fs();
		set_fs(KERNEL_DS);
	} else {
		set_fs(pOSFSInfo->fs);
#if LINUX_VERSION_CODE < KERNEL_VERSION(2,6,29)
		current->fsuid = pOSFSInfo->fsuid;
		current->fsgid = pOSFSInfo->fsgid;
#endif
	}
}

void RtmpOSFSInfoChange(IN RTMP_OS_FS_INFO *pOSFSInfoOrg,
			IN BOOLEAN bSet) {
	__RtmpOSFSInfoChange(pOSFSInfoOrg, bSet);
}

static int testmod_init(void)
{
	PSTRING					src = NULL;
	RTMP_OS_FD				srcf;
	RTMP_OS_FS_INFO				osFSInfo;
	INT					retval = NDIS_STATUS_FAILURE;
	PSTRING					buffer;

	os_alloc_mem(NULL, (UCHAR **)&buffer, MAX_INI_BUFFER_SIZE);
	if(buffer == NULL)
		return NDIS_STATUS_FAILURE;
	memset(buffer, 0x00, MAX_INI_BUFFER_SIZE);

	src = STA_PROFILE_PATH;

	if (src && *src)
	{
		// it must call set_fs() before read, otherwise it get errCode=-14( address error)
		RtmpOSFSInfoChange(&osFSInfo, TRUE);
		srcf = RtmpOSFileOpen(src, O_RDONLY, 0);
		if (IS_FILE_OPEN_ERR(srcf)) 
		{
			DBGPRINT(RT_DEBUG_ERROR, ("Open file \"%s\" failed!\n", src));
		}
		else 
		{
			retval =RtmpOSFileRead(srcf, buffer, MAX_INI_BUFFER_SIZE);
			if (retval > 0)
			{
//				RTMPSetProfileParameters(pAd, buffer);
				DBGPRINT(RT_DEBUG_ERROR, ("Read file \"%s\": buffer = %s !\n", src, buffer));
				retval = NDIS_STATUS_SUCCESS;
			}
			else
				DBGPRINT(RT_DEBUG_ERROR, ("Read file \"%s\" failed(errCode=%d)!\n", src, retval));

			retval = RtmpOSFileClose(srcf);
			if ( retval != 0)
			{
				retval = NDIS_STATUS_FAILURE;
				DBGPRINT(RT_DEBUG_ERROR, ("Close file \"%s\" failed(errCode=%d)!\n", src, retval));
			}
		}

		RtmpOSFSInfoChange(&osFSInfo, FALSE);
	}

	os_free_mem(NULL, buffer);

	return (retval);
}

static void testmod_cleanup(void)
{
	printk("\nmodule exit...\n");
}

module_init(testmod_init);
module_exit(testmod_cleanup);
