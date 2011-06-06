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
#include <pthread.h>
#include <semaphore.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <asm/types.h>
#include <linux/if.h>
#include <linux/wireless.h>
#include <arpa/inet.h>
#include <errno.h>

#define IW_MAX_PRIV_DEF	160
/* Size (in bytes) of various events */
static const int priv_type_size[] = {
	0,				/* IW_PRIV_TYPE_NONE */
	1,				/* IW_PRIV_TYPE_BYTE */
	1,				/* IW_PRIV_TYPE_CHAR */
	0,				/* Not defined */
	sizeof(__u32),			/* IW_PRIV_TYPE_INT */
	sizeof(struct iw_freq),		/* IW_PRIV_TYPE_FLOAT */
	sizeof(struct sockaddr),	/* IW_PRIV_TYPE_ADDR */
	0,				/* Not defined */
};

/*------------------------------------------------------------------*/
/*
 * Max size in bytes of an private argument.
 */
int
iw_get_priv_size(int	args)
{
	int	num = args & IW_PRIV_SIZE_MASK;
	int	type = (args & IW_PRIV_TYPE_MASK) >> 12;

	return(num * priv_type_size[type]);
}

/*
 * Open a socket.
 * Depending on the protocol present, open the right socket. The socket
 * will allow us to talk to the driver.
 */
int
iw_sockets_open(void)
{
  static const int families[] = {
    AF_INET, AF_IPX, AF_AX25, AF_APPLETALK
  };
  unsigned int	i;
  int		sock;

  /*
   * Now pick any (exisiting) useful socket family for generic queries
   *
   * Note : don't open all the socket, only returns when one matches,
   * all protocols might not be valid.
   * Workaround by Jim Kaba <jkaba@sarnoff.com>
   * Note : in 99% of the case, we will just open the inet_sock.
   * The remaining 1% case are not fully correct...
   */

  /* Try all families we support */
  for(i = 0; i < sizeof(families)/sizeof(int); ++i)
    {
      /* Try to open the socket, if success returns it */
      sock = socket(families[i], SOCK_DGRAM, 0);
      if(sock >= 0)
	return sock;
  }

  return -1;
}

/*------------------------------------------------------------------*/
/*
 * Get information about what private ioctls are supported by the driver
 */
int
iw_get_priv_info(int skfd, char *ifname, struct iw_priv_args * priv, int maxpriv)
{
	struct iwreq wrq;

	/* Ask the driver */
	wrq.u.data.pointer = (caddr_t) priv;
	wrq.u.data.length = maxpriv;
	wrq.u.data.flags = 0;
	strncpy(wrq.ifr_name, ifname, IFNAMSIZ);
	/* Do the request */
	if (ioctl(skfd, SIOCGIWPRIV, &wrq) < 0)
		return (-1);

	printf("========== errno = %d\n", errno);
	/* Return the number of ioctls */
	return (wrq.u.data.length);
}

#define RT_OID_WE_VERSION_COMPILED		    0x0622
// #define RT_PRIV_IOCTL								(SIOCIWFIRSTPRIV + 0x0E)
#define RT_PRIV_IOCTL							(SIOCIWFIRSTPRIV + 0x01) /* Sync. with AP for wsc upnp daemon */

static int
ralink_get_we_version_compiled(int skfd, char *ifname)
{
	struct iwreq iwr;
	unsigned int we_version_compiled = 0;

	memset(&iwr, 0, sizeof(iwr));
//	os_strlcpy(iwr.ifr_name, drv->ifname, IFNAMSIZ);
	iwr.u.data.pointer = (caddr_t) &we_version_compiled;
	iwr.u.data.flags = RT_OID_WE_VERSION_COMPILED;
	strncpy(iwr.ifr_name, ifname, IFNAMSIZ);

	if (ioctl(skfd, RT_PRIV_IOCTL, &iwr) < 0) {
		printf("%s: failed, errno = %d\n", __func__, errno);
		return -1;
	}

	// drv->we_version_compiled = we_version_compiled;
	printf("we version = %d\n", we_version_compiled);
	return 0;
}

#define OID_P2P_WSC_PIN_CODE		0x080a
#define OID_802_11_P2P_DEVICE_NAME			0x0802
static int
ralink_get_device_name(int skfd, char *ifname)
{
	struct iwreq iwr;
	char device_name[120];

	memset(&iwr, 0, sizeof(iwr));
	memset(device_name, 0, sizeof(device_name));
	printf("size of device_name[120] = %d\n", sizeof(device_name));

	iwr.u.data.pointer = (caddr_t) device_name;
	iwr.u.data.flags = OID_802_11_P2P_DEVICE_NAME;
	strncpy(iwr.ifr_name, ifname, IFNAMSIZ);

	if (ioctl(skfd, RT_PRIV_IOCTL, &iwr) < 0) {
		printf("%s: failed, errno = %d\n", __func__, errno);
		return -1;
	}

	// drv->device_name = device_name;
	printf("device name = %s\n", device_name);
	return 0;
}

static int
ralink_get_pin_code(int skfd, char *ifname)
{
	struct iwreq iwr;
	unsigned int pin_code = 0;

	memset(&iwr, 0, sizeof(iwr));
	iwr.u.data.pointer = (caddr_t) &pin_code;
	iwr.u.data.flags = OID_P2P_WSC_PIN_CODE;
	strncpy(iwr.ifr_name, ifname, IFNAMSIZ);

	if (ioctl(skfd, RT_PRIV_IOCTL, &iwr) < 0) {
		printf("%s: failed, errno = %d\n", __func__, errno);
		return -1;
	}

	// drv->pin_code = pin_code;
	printf("pin code = %d\n", pin_code);
	return 0;
}

/*------------------------------------------------------------------*/
/*
 * Execute a private command on the interface
 */
int set_private(int skfd,	/* Socket */
	      char *ifname,
	      char *cmdname,
	      void **ret,
	      int count,
	      void *args[])
{				/* Dev name */
	struct iwreq	wrq;
	unsigned char	buffer[4096];	/* Only that big in v25 and later */
	struct	iw_priv_args priv[IW_MAX_PRIV_DEF];
	int	priv_num;		/* Max of private ioctl */
	int	subcmd = 0;	/* sub-ioctl index */
	int	offset = 0;	/* Space for sub-ioctl index */
	int	k, i = 0;
	unsigned char	*data = NULL;

	/* Read the private ioctls */
	priv_num = iw_get_priv_info(skfd, ifname, priv, IW_MAX_PRIV_DEF);

	/* Is there any ? */
	if (priv_num <= 0) {
		/* Could skip this message ? */
		fprintf(stderr, "%-8.8s	 no private ioctls.\n\n", ifname);
		return (-1);
	}

	/* Search the correct ioctl */
	k = -1;
	while((++k < priv_num) && strcmp(priv[k].name, cmdname));

	/* If not found... */
	if(k == priv_num)
	{
		printf("Invalid command : %s\n", cmdname);
		return(-1);
	}

	/* Watch out for sub-ioctls ! */
	if(priv[k].cmd < SIOCDEVPRIVATE) {
		int	j = -1;

		/* Find the matching *real* ioctl */
		while((++j < priv_num) && ((priv[j].name[0] != '\0') ||
				(priv[j].set_args != priv[k].set_args) ||
				(priv[j].get_args != priv[k].get_args)));

		/* If not found... */
		if(j == priv_num) {
			fprintf(stderr, "Invalid private ioctl definition for : %s\n",
				cmdname);
			return(-1);
		}

		/* Save sub-ioctl number */
		subcmd = priv[k].cmd;
		/* Reserve one int (simplify alignement issues) */
		offset = sizeof(__u32);
		/* Use real ioctl definition from now on */
		k = j;
	}

	/* If we have to set some data */
	if((priv[k].set_args & IW_PRIV_TYPE_MASK) &&
			(priv[k].set_args & IW_PRIV_SIZE_MASK)){
		switch(priv[k].set_args & IW_PRIV_TYPE_MASK){

		case IW_PRIV_TYPE_INT:
			/* Number of args to fetch */
			wrq.u.data.length = 1;
			if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
				wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

			/* Fetch args */
			for(; i < wrq.u.data.length; i++) {
				//printf("(%s:%d) args[%d]=%d, wrq.u.data.length=%d\n", __FUNCTION__, __LINE__, i, (int)args[i], wrq.u.data.length);
				((__s32 *) buffer)[i] = (__s32) args[i];
			}
			break;

		case IW_PRIV_TYPE_CHAR:
			if(i < count){
				/* Size of the string to fetch */
				wrq.u.data.length = strlen(args[i]) + 1;
				if(wrq.u.data.length > (priv[k].set_args & IW_PRIV_SIZE_MASK))
					wrq.u.data.length = priv[k].set_args & IW_PRIV_SIZE_MASK;

				/* Fetch string */
				//printf("(%s:%d) args[%d]=%s, wrq.u.data.length=%d\n", __FUNCTION__, __LINE__, i, (char *)args[i], wrq.u.data.length);
				memcpy(buffer, args[i], wrq.u.data.length);
				buffer[sizeof(buffer) - 1] = '\0';
				//printf("(%s:%d) buffer=%s\n", __FUNCTION__, __LINE__, buffer);
				i++;
			} else {
				wrq.u.data.length = 1;
				buffer[0] = '\0';
			}
			break;


		default:
			printf("%s:%d ERROR()\n", __FUNCTION__, __LINE__);
			fprintf(stderr, "Not yet implemented...\n");
			return(-1);
		}

		if((priv[k].set_args & IW_PRIV_SIZE_FIXED) &&
				(wrq.u.data.length != (priv[k].set_args & IW_PRIV_SIZE_MASK))){
			printf("The command %s need exactly %d argument...\n",
					cmdname, priv[k].set_args & IW_PRIV_SIZE_MASK);
			return(-1);
		}
	}	/* if args to set */
	else
	{
		wrq.u.data.length = 0L;
	}

	strncpy(wrq.ifr_name, ifname, IFNAMSIZ);

	/* Those two tests are important. They define how the driver
	 * will have to handle the data */
	if((priv[k].set_args & IW_PRIV_SIZE_FIXED) &&
			((iw_get_priv_size(priv[k].set_args) + offset) <= IFNAMSIZ)){
		/* First case : all SET args fit within wrq */
		if(offset)
			wrq.u.mode = subcmd;
		memcpy(wrq.u.name + offset, buffer, IFNAMSIZ - offset);
	} else {
		if((priv[k].set_args == 0) &&
				(priv[k].get_args & IW_PRIV_SIZE_FIXED) &&
				(iw_get_priv_size(priv[k].get_args) <= IFNAMSIZ)){
			/* Second case : no SET args, GET args fit within wrq */
			if(offset)
				wrq.u.mode = subcmd;
		} else {
			/* Thirst case : args won't fit in wrq, or variable number of args */
			wrq.u.data.pointer = (caddr_t) buffer;
			wrq.u.data.flags = subcmd;
		}
	}

	/* Perform the private ioctl */
	if(ioctl(skfd, priv[k].cmd, &wrq) < 0) {
		fprintf(stderr, "Interface doesn't accept private ioctl...\n");
		fprintf(stderr, "%s (%X): %s\n", cmdname, priv[k].cmd, strerror(errno));
		return(-1);
	}

	/* If we have to get some data */
	if((priv[k].get_args & IW_PRIV_TYPE_MASK) &&
			(priv[k].get_args & IW_PRIV_SIZE_MASK)) {
		int	j;
		int	n = 0;		/* number of args */

		printf("%-8.8s	%s:\n", ifname, cmdname);

		/* Check where is the returned data */
		if((priv[k].get_args & IW_PRIV_SIZE_FIXED) &&
				(iw_get_priv_size(priv[k].get_args) <= IFNAMSIZ)) {
			memcpy(buffer, wrq.u.name, IFNAMSIZ);
			n = priv[k].get_args & IW_PRIV_SIZE_MASK;
		} else {
			n = wrq.u.data.length;
		}

		switch(priv[k].get_args & IW_PRIV_TYPE_MASK) {

		case IW_PRIV_TYPE_INT:
			/* Display args */
			for(j = 0; j < n; j++){
				*ret = (void *)((__s32 *) buffer)[j];
			}
			break;

		case IW_PRIV_TYPE_CHAR:
			/* Display args */
			buffer[wrq.u.data.length - 1] = '\0';
			data = malloc(wrq.u.data.length);
			strncpy((char *)data, (char *)buffer, wrq.u.data.length);
			data[wrq.u.data.length - 1] = '\0';
			*ret = data;

			break;

		default:
			fprintf(stderr, "Not yet implemented...\n");
			return(-1);
		}
	}	/* if args to set */

	return 0;
}

int main(int argc, char *argv[])
{
	int skfd;
	char *data = NULL;
	int arg_count = 0;
	int ret;

	if ((skfd = iw_sockets_open()) < 0) {
		printf("(%s:%d) iw_sockets_open() fail.\n", __FUNCTION__, __LINE__);
		return -1;
	}

	printf("======	skfd = %d ======\n", skfd);

//	ret = set_private(skfd, "p2p0", "stat", (void *)&data, arg_count, NULL);
	// test
//	ralink_get_we_version_compiled(skfd, "ra0");
	ralink_get_pin_code(skfd, "p2p0");
	ralink_get_device_name(skfd, "p2p0");

#if 0
	if (ret == -1) {
		fprintf(stderr, "wrong stat ioctl command\n");
		close(skfd);
		return -1;
	}

	printf("arg_count = %d\n", arg_count);

	if(data != NULL) {
		printf("data from driver = %s\n", data);
		free(data);
	} else {
		printf("data from driver is empty\n");
	}
#endif

	close(skfd);

	return 0;
}
