/* Getting MAC address from a network interface */

/* This page shows you how to obtain the MAC address from a network
 * interface.  */

#include <stdio.h>

#include <string.h> /* for strncpy */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

int
main()
{
	int fd;
	struct ifreq ifr;

	fd = socket(AF_INET, SOCK_DGRAM, 0);

	ifr.ifr_addr.sa_family = AF_INET;
	strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1);

	ioctl(fd, SIOCGIFHWADDR, &ifr);

	close(fd);

	/* display result */
	printf("%.2x:%.2x:%.2x:%.2x:%.2x:%.2x\n",
	       (unsigned char)ifr.ifr_hwaddr.sa_data[0],
	       (unsigned char)ifr.ifr_hwaddr.sa_data[1],
	       (unsigned char)ifr.ifr_hwaddr.sa_data[2],
	       (unsigned char)ifr.ifr_hwaddr.sa_data[3],
	       (unsigned char)ifr.ifr_hwaddr.sa_data[4],
	       (unsigned char)ifr.ifr_hwaddr.sa_data[5]);

	return 0;
}


/* On Linux platform, you can use ioctl to obtain the MAC address
 * information. To use ioctl, you must create a socket. The important thing is
 * that the socket family used is AF_INET. You can use either TCP(SOCK_STREAM)
 * or UDP(SOCK_DGRAM) socket. This sample uses an UDP socket.

 * The socket used to get the MAC address information can be closed after
 * obtaining the MAC address information. You can use it as a normal socket to
 * communicate too.  */
