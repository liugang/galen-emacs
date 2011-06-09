/* The getaddrinfo function is used to resolve both IPv4 and IPv6
 * hostnames. This document shows an easy example of how to use getaddrinfo on
 * Linux platform.  */

/* The following example shows you how to resolve an IPv4 address.  */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int
main()
{
	char *hostname = "localhost"; /* you can use www.google.com */
	struct addrinfo hints, *res;
	struct in_addr addr;
	int err;

	memset(&hints, 0, sizeof(hints));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_INET;

	if ((err = getaddrinfo(hostname, NULL, &hints, &res)) != 0) {
		printf("error %d\n", err);
		return 1;
	}

	addr.s_addr = ((struct sockaddr_in *)(res->ai_addr))->sin_addr.s_addr;

	printf("ip address : %s\n", inet_ntoa(addr));

	freeaddrinfo(res);

	return 0;
}

/* Please note that you would need to use freeaddrinfo to free the memory
 * allocated by getaddrinfo. It is likely to forget using freeaddrinfo, and
 * cause a memory leak.	 */
