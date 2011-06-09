/* http://tldp.org/LDP/lkmpg/2.6/html/x892.html#FTN.AEN919 */
/*
 *  chardev.c - Create an input/output character device
 */

#include <linux/kernel.h>	/* We're doing kernel work */
#include <linux/module.h>	/* Specifically, a module */
#include <linux/fs.h>
#include <asm/uaccess.h>	/* for get_user and put_user */

#include "chardev.h"
#define SUCCESS 0
#define DEVICE_NAME "char_dev"
#define BUF_LEN 80

MODULE_LICENSE("GPL");
MODULE_AUTHOR("galen");

/*
 * Is the device open right now? Used to prevent
 * concurent access into the same device
 */
static int Device_Open = 0;

/*
 * The message the device will give when asked
 */
static char Message[BUF_LEN];

/*
 * How far did the process reading the message get?
 * Useful if the message is larger than the size of the
 * buffer we get to fill in device_read.
 */
static char *Message_Ptr;

/*
 * This is called whenever a process attempts to open the device file
 */
static int device_open(struct inode *inode, struct file *file)
{
#ifdef DEBUG
	printk(KERN_INFO "device_open(%p)\n", file);
#endif

	/*
	 * We don't want to talk to two processes at the same time
	 */
	if (Device_Open)
		return -EBUSY;

	Device_Open++;
	/*
	 * Initialize the message
	 */
	Message_Ptr = Message;
	try_module_get(THIS_MODULE);
	return SUCCESS;
}

static int device_release(struct inode *inode, struct file *file)
{
#ifdef DEBUG
	printk(KERN_INFO "device_release(%p,%p)\n", inode, file);
#endif

	/*
	 * We're now ready for our next caller
	 */
	Device_Open--;

	module_put(THIS_MODULE);
	return SUCCESS;
}

/*
 * This function is called whenever a process which has already opened the
 * device file attempts to read from it.
 */
static ssize_t device_read(struct file *file,	/* see include/linux/fs.h   */
			   char __user * buffer,	/* buffer to be
							 * filled with data */
			   size_t length,	/* length of the buffer	    */
			   loff_t * offset)
{
	/*
	 * Number of bytes actually written to the buffer
	 */
	int bytes_read = 0;

#ifdef DEBUG
	printk(KERN_INFO "device_read(%p,%p,%d)\n", file, buffer, length);
#endif

	/*
	 * If we're at the end of the message, return 0
	 * (which signifies end of file)
	 */
	if (*Message_Ptr == 0)
		return 0;

	/*
	 * Actually put the data into the buffer
	 */
	while (length && *Message_Ptr) {

		/*
		 * Because the buffer is in the user data segment,
		 * not the kernel data segment, assignment wouldn't
		 * work. Instead, we have to use put_user which
		 * copies data from the kernel data segment to the
		 * user data segment.
		 */
		put_user(*(Message_Ptr++), buffer++);
		length--;
		bytes_read++;
	}

#ifdef DEBUG
	printk(KERN_INFO "Read %d bytes, %d left\n", bytes_read, length);
#endif

	/*
	 * Read functions are supposed to return the number
	 * of bytes actually inserted into the buffer
	 */
	return bytes_read;
}

/*
 * This function is called when somebody tries to
 * write into our device file.
 */
static ssize_t
device_write(struct file *file,
	     const char __user * buffer, size_t length, loff_t * offset)
{
	int i;

#ifdef DEBUG
	printk(KERN_INFO "device_write(%p,%s,%d)", file, buffer, length);
#endif

	for (i = 0; i < length && i < BUF_LEN; i++)
		get_user(Message[i], buffer + i);

	Message_Ptr = Message;

	/*
	 * Again, return the number of input characters used
	 */
	return i;
}

/*
 * This function is called whenever a process tries to do an ioctl on our
 * device file. We get two extra parameters (additional to the inode and file
 * structures, which all device functions get): the number of the ioctl called
 * and the parameter given to the ioctl function.
 *
 * If the ioctl is write or read/write (meaning output is returned to the
 * calling process), the ioctl call returns the output of this function.
 *
 */
//int device_ioctl(struct inode *inode,	/* see include/linux/fs.h */
//		 struct file *file,	/* ditto */
//		 unsigned int ioctl_num,	/* number and param for ioctl */
//		 unsigned long ioctl_param)
// long (*unlocked_ioctl) (struct file *, unsigned int, unsigned long);
long device_ioctl(struct file *file,	/* ditto */
		  unsigned int ioctl_num,	/* number and param for ioctl */
		  unsigned long ioctl_param)
{
	int i;
	char *temp;
	char ch;

	/*
	 * Switch according to the ioctl called
	 */
	switch (ioctl_num) {
		case IOCTL_SET_MSG:
			/*
			 * Receive a pointer to a message (in user space) and set that
			 * to be the device's message.	Get the parameter given to
			 * ioctl by the process.
			 */
			temp = (char *)ioctl_param;

			/*
			 * Find the length of the message
			 */
			get_user(ch, temp);
			for (i = 0; ch && i < BUF_LEN; i++, temp++)
				get_user(ch, temp);

			device_write(file, (char *)ioctl_param, i, 0);
			break;

		case IOCTL_GET_MSG:
			/*
			 * Give the current message to the calling process -
			 * the parameter we got is a pointer, fill it.
			 */
			i = device_read(file, (char *)ioctl_param, 99, 0);

			/*
			 * Put a zero at the end of the buffer, so it will be
			 * properly terminated
			 */
			put_user('\0', (char *)ioctl_param + i);
			break;

		case IOCTL_GET_NTH_BYTE:
			/*
			 * This ioctl is both input (ioctl_param) and
			 * output (the return value of this function)
			 */
			return Message[ioctl_param];
			break;
	}

	return SUCCESS;
}

/* Module Declarations */

/*
 * This structure will hold the functions to be called
 * when a process does something to the device we
 * created. Since a pointer to this structure is kept in
 * the devices table, it can't be local to
 * init_module. NULL is for unimplemented functions.
 */

struct file_operations Fops = {
	.read = device_read,
	.write = device_write,
//	.ioctl = device_ioctl, // 在kernel 2.6.36 中已经完全删除了struct
//	file_operations 中的ioctl 函数指针，取而代之的是unlocked_ioctl,

	/* unlocked_ioctl 实际上取代了用了很久的ioctl，主要的改进就是不再需要上大
	   内核锁 （调用之前不再先调用lock_kernel()然后再unlock_kernel()）总的来说kernel 开
	   发者正在试图朝移除大内核锁的方向努力，ioctl的移除就是被革命了。相信以后越来越多的
	   内核函数会摆脱大内核锁的依赖，并且大内核锁最终会被移除。 */
	/* http://lp007819.wordpress.com/2011/01/06/kernel-2-6-36-ioctl-%E5%8F%98%E6%9B%B4/ */
	.unlocked_ioctl = device_ioctl,
	.open = device_open,
	.release = device_release,	/* a.k.a. close */
};

/*
 * Initialize the module - Register the character device
 */
static int init_char_module(void)
{
	int ret_val;
	/*
	 * Register the character device (atleast try)
	 */
	ret_val = register_chrdev(MAJOR_NUM, DEVICE_NAME, &Fops);

	/*
	 * Negative values signify an error
	 */
	if (ret_val < 0) {
		printk(KERN_ALERT "%s failed with %d\n",
		       "Sorry, registering the character device ", ret_val);
		return ret_val;
	}

	printk(KERN_INFO "%s The major device number is %d.\n",
	       "Registeration is a success", MAJOR_NUM);
	printk(KERN_INFO "If you want to talk to the device driver,\n");
	printk(KERN_INFO "you'll have to create a device file. \n");
	printk(KERN_INFO "We suggest you use:\n");
	printk(KERN_INFO "mknod %s c %d 0\n", DEVICE_FILE_NAME, MAJOR_NUM);
	printk(KERN_INFO "The device file name is important, because\n");
	printk(KERN_INFO "the ioctl program assumes that's the\n");
	printk(KERN_INFO "file you'll use.\n");

	return 0;
}

/*
 * Cleanup - unregister the appropriate file from /proc
 */
//void cleanup_char_module()
static void cleanup_char_module(void)

{
	/* int ret; */

	/*
	 * Unregister the device
	 */
	/* static inline void unregister_chrdev(unsigned int major, const char *name) */
	unregister_chrdev(MAJOR_NUM, DEVICE_NAME);
	printk(KERN_INFO "unregister_chrdev().\n");

	/*
	 * If there's an error, report it
	 */
	/* if (ret < 0) */
	/*	printk(KERN_ALERT "Error: unregister_chrdev: %d\n", ret); */
}

module_init(init_char_module);
module_exit(cleanup_char_module);
