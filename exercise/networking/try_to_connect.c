/* getaddrinfo : try to connect until success
 */

/* On the Internet, hostnames often have multiple IP address. Sometimes both
 * IPv4 and IPv6 address are resolved. It is very popular to try connecting
 * until the connect system call succeeds. This document shows you how to
 * resolve both IPv4 and IPv6 address, and try connecting until connect system
 * call succeeds.  */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int
main()
{
	char *hostname = "localhost";
	char *service = "http";
	struct addrinfo hints, *res0, *res;
	int err;
	int sock;

	memset(&hints, 0, sizeof(hints));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = PF_UNSPEC;

	if ((err = getaddrinfo(hostname, service, &hints, &res0)) != 0) {
		printf("error %d\n", err);
		return 1;
	}

	for (res=res0; res!=NULL; res=res->ai_next) {
		sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
		if (sock < 0) {
			continue;
		}
 
		if (connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
			close(sock);
			continue;
		}

		break;
	}

	if (res == NULL) {
		/* could not create a valid connection */
		printf("failed\n");
		close(sock);
		return 1;
	}

	freeaddrinfo(res0);
	close(sock);
	/* Write transmission code using sock here... */

	return 0;
}
