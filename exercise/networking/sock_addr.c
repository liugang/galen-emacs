/* creating sockaddr using getaddrinfo */

/* The previous example showed how to use getaddrinfo like gethostbyname. This
 * page shows you another way to use getaddrinfo. To use socket system call, you
 * would have to know the protocol family, for example, AF_INET or AF_INET6. To
 * use connect system call, you would need sockaddr. This document shows you how
 * to get the protocol family and sockaddr from getaddrinfo result.  */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int
main()
{
	char *hostname = "localhost";
	char *service = "80";
	struct addrinfo hints, *res;
	int err;
	int sock;

	memset(&hints, 0, sizeof(hints));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_INET;

	if ((err = getaddrinfo(hostname, service, &hints, &res)) != 0) {
		printf("error %d : %s\n", err, gai_strerror(err));
		return 1;
	}

	sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	if (sock < 0) {
		perror("socket");
		return 1;
	}

	if (connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
		perror("connect");
		close(sock);
		return 1;
	}

	freeaddrinfo(res);
	close(sock);
	return 0;
}


/* The second argument given to getaddrinfo will be the port number or service
 * name. for example, you can use service="http" instead of service="80".

 * This example uses IPv4. You can use IPv6 by setting hints.ai_family =
 * AF_INET6. If you want to use either IPv4 or IPv6, you can do hints.ai_family
 * = PF_UNSPEC.
 */
