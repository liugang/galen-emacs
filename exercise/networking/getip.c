/* Getting IP address from a network interface */

/* This page shows you how to obtain the IP address from a network interface.  */

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

	/* I want to get an IPv4 IP address */
	ifr.ifr_addr.sa_family = AF_INET;

	/* I want IP address attached to "eth0" */
	strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1);

	ioctl(fd, SIOCGIFADDR, &ifr);

	close(fd);

	/* display result */
	printf("%s\n", inet_ntoa(((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr));

	return 0;
}

/* On Linux platform, you can use ioctl to obtain the IP address information. To
use ioctl, you must create a socket. The important thing is that the socket
family used is AF_INET. You can use either TCP(SOCK_STREAM) or UDP(SOCK_DGRAM)
socket. This sample uses an UDP socket.

The socket used to get the IP address
information can be closed after obtaining the IP address information. You can
use it as a normal socket to communicate too.  */
